# murphy_etal_2026/maintained/corrected_slopes.R
# Output: maintained/output/corrected_slopes.csv, maintained/output/sign_audit.csv,
#   maintained/output/corrected_favorability_tables.csv
# Depends on: maintained/archive_run/ (the deposit, rerun by maintained/run_archive.R),
#   ground_truth/published_appendix_values.csv
# Description: Refit the published meta-regressions on the deposit's own CATEs under three
#   corrections, leaving the deposit's pipeline as published:
#   sanders    favorBS_rev is rescaled twice in clean_exps.R, so its CATEs carry a factor of
#              1/4. The rescale is linear: coefficient times 4, covariance times 16.
#   sign       estimate_all_cates.R multiplies every outcome by -1 for an anti ad. Turnout gets
#              no flip here, and an issue outcome takes the unused `direction` column instead.
#   tone       Table D.16 codes three anti-Trump ads as positive, against the article's rule
#              that an attack ad is coded negative. Recode them for the intended-emotion index.

library(tidyverse)
library(here)
library(metafor)
library(clubSandwich)
library(Matrix)

here::i_am("maintained/corrected_slopes.R")

archive <- here::here("maintained", "archive_run", "replication_archive")

cates <- read_rds(file.path(archive, "data", "estimates", "all_cates.rds"))
vcov_cates <- read_rds(file.path(archive, "data", "estimates", "block_diagonal_vcov.rds"))
ratings <- read_rds(file.path(archive, "data", "estimates", "ratings_pid.rds"))
tones <- read_csv(file.path(archive, "data", "ad_df_emotions.csv"), show_col_types = FALSE)
directions <- read_csv(file.path(archive, "data", "ad_id_to_base_by_hand.csv"), show_col_types = FALSE) |>
  filter(target_issue == 1) |>
  transmute(ad_id, outcome = str_c(outcome, "_01"), direction)

stopifnot(nrow(cates) == nrow(vcov_cates), !anyDuplicated(directions[c("ad_id", "outcome")]))

cates <- cates |>
  left_join(directions, by = join_by(ad_id, outcome), relationship = "many-to-one") |>
  mutate(id = row_number(),
         is_anti = str_detect(ad_valence, "Anti"),
         published_sign = if_else(is_anti, -1, 1),
         corrected_sign = case_when(outcome == "will_vote" ~ 1,
                                    str_detect(outcome, "Base") ~ direction,
                                    .default = published_sign),
         sign_change = corrected_sign * published_sign,
         sanders_scale = if_else(outcome == "favorBS_rev", 4, 1),
         dv = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        outcome == "will_vote" ~ "Turnout Intentions",
                        str_detect(outcome, "Base") ~ "Target Issue Support"))

stopifnot(all(cates$corrected_sign %in% c(-1, 1)))

sign_audit <- cates |>
  summarise(estimates = n(), sign_differs = sum(sign_change == -1), .by = dv)
write_csv(sign_audit, here::here("maintained", "output", "sign_audit.csv"))

# Indices, keyed the way the figure scripts key them ----
index_wide <- ratings |>
  filter(rating_outcome %in% c("positive_index", "negative_index")) |>
  select(ad_id, pid_3_pre, date, rating_outcome, rating_estimate) |>
  pivot_wider(names_from = rating_outcome, values_from = rating_estimate) |>
  left_join(select(tones, ad_id, ad_valence, emotion_valence), by = join_by(ad_id)) |>
  mutate(recoded_valence = if_else(str_detect(ad_valence, "Anti") & emotion_valence == "positive",
                                   "negative", emotion_valence),
         intended = case_when(
           emotion_valence == "negative" ~ negative_index - positive_index,
           emotion_valence == "positive" ~ positive_index - negative_index,
           emotion_valence == "both" ~ positive_index + negative_index),
         intended_recoded = case_when(
           recoded_valence == "negative" ~ negative_index - positive_index,
           recoded_valence == "positive" ~ positive_index - negative_index,
           recoded_valence == "both" ~ positive_index + negative_index)) |>
  select(ad_id, pid_3_pre, date, positive_index, negative_index, intended, intended_recoded) |>
  left_join(ratings |> filter(rating_outcome == "index") |>
              transmute(ad_id, pid_3_pre, date, rating_index = rating_estimate),
            by = join_by(ad_id, pid_3_pre, date), relationship = "one-to-one")

stopifnot(!anyDuplicated(index_wide[c("ad_id", "pid_3_pre", "date")]))

panel_data <- cates |>
  left_join(index_wide, by = join_by(ad_id, pid_3_pre, date), relationship = "many-to-one")

stopifnot(nrow(panel_data) == nrow(cates), !anyNA(panel_data$positive_index))

fit_panel <- function(dv_name, index_name, scale_factor) {
  rows <- which(panel_data$dv == dv_name)
  panel <- panel_data[rows, ] |>
    mutate(rating_estimate = .data[[index_name]], y = estimate * scale_factor[rows])
  scaling <- Diagonal(x = scale_factor[rows])
  fit <- rma.mv(yi = y, mods = ~ rating_estimate, random = ~ 1 | id,
                V = as.matrix(scaling %*% vcov_cates[rows, rows] %*% scaling), data = panel) |>
    robust(cluster = date, clubSandwich = TRUE)
  tibble(dv = dv_name, index = index_name, estimate = fit$beta[2, 1], std_error = fit$se[2],
         p_value = fit$pval[2])
}

specifications <- list(
  published = rep(1, nrow(cates)),
  sanders = cates$sanders_scale,
  sign = cates$sign_change,
  both = cates$sanders_scale * cates$sign_change
)

panels <- bind_rows(
  expand_grid(dv = c("Favorability", "Vote Choice", "Target Issue Support", "Turnout Intentions"),
              index = c("negative_index", "positive_index", "intended")),
  tibble(dv = c("Favorability", "Vote Choice"), index = "intended_recoded"),
  tibble(dv = c("Favorability", "Vote Choice"), index = "rating_index")
)

corrected_slopes <- imap(specifications, \(scale_factor, name) {
  pmap(panels, \(dv, index) fit_panel(dv, index, scale_factor)) |>
    list_rbind() |>
    mutate(specification = name)
}) |>
  list_rbind()

write_csv(corrected_slopes, here::here("maintained", "output", "corrected_slopes.csv"))

# The four favorability tables, refit column by column under the Sanders correction ----
table_indices <- c(C.1 = "positive_index", C.6 = "negative_index", C.11 = "intended", E.17 = "rating_index")
table_columns <- tribble(
  ~column, ~pid, ~side, ~controls,
  "Dem (PD)", "Democrat", "Pro Democrat", FALSE,
  "Dem (PR)", "Democrat", "Pro Republican", FALSE,
  "Ind (PD)", "Independent", "Pro Democrat", FALSE,
  "Ind (PR)", "Independent", "Pro Republican", FALSE,
  "Rep (PD)", "Republican", "Pro Democrat", FALSE,
  "Rep (PR)", "Republican", "Pro Republican", FALSE,
  "All", NA, NA, FALSE,
  "All (2)", NA, NA, TRUE
) |>
  mutate(column_order = row_number())

panel_table <- panel_data |>
  mutate(valence = if_else(ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"),
                           "Pro Republican", "Pro Democrat"))

fit_column <- function(index_name, scale_factor, pid, side, controls) {
  rows <- which(panel_table$dv == "Favorability" &
                  (is.na(pid) | panel_table$pid_3_pre == pid) &
                  (is.na(side) | panel_table$valence == side))
  panel <- panel_table[rows, ] |>
    mutate(rating_estimate = .data[[index_name]], y = estimate * scale_factor[rows])
  scaling <- Diagonal(x = scale_factor[rows])
  fit <- rma.mv(yi = y, mods = if (controls) ~ rating_estimate + valence + pid_3_pre else ~ rating_estimate,
                random = ~ 1 | id, V = as.matrix(scaling %*% vcov_cates[rows, rows] %*% scaling), data = panel) |>
    robust(cluster = date, clubSandwich = TRUE)
  tibble(term = rownames(fit$beta), estimate = fit$beta[, 1], std_error = fit$se, p_value = fit$pval,
         q_test_p = fit$QEp, observations = fit$k)
}

fit_table <- function(table_name, scale_factor, specification) {
  table_columns |>
    mutate(fit = pmap(list(pid, side, controls), \(p, s, k) fit_column(table_indices[[table_name]], scale_factor, p, s, k))) |>
    select(column, column_order, fit) |>
    unnest(fit) |>
    mutate(table = table_name, specification = specification)
}

favorability_tables <- names(table_indices) |>
  map(\(table_name) bind_rows(fit_table(table_name, specifications$published, "published"),
                              fit_table(table_name, specifications$sanders, "sanders"))) |>
  list_rbind()

print_cell <- \(x) str_replace(sprintf("%.3f", x), "^-(0\\.000)$", "\\1")
term_order <- c("intrcpt", "rating_estimate", "valencePro Republican", "pid_3_preIndependent", "pid_3_preRepublican")

table_cells <- favorability_tables |>
  mutate(term_order = match(term, term_order),
         estimate_cell = str_c(print_cell(estimate), if_else(p_value < 0.05, "*", "")),
         std_error_cell = print_cell(std_error))

coefficient_cells <- table_cells |>
  pivot_longer(c(estimate_cell, std_error_cell), names_to = "part", values_to = "value") |>
  mutate(part_order = if_else(part == "estimate_cell", 1, 2)) |>
  arrange(table, specification, term_order, part_order, column_order) |>
  select(table, specification, column, term, part, value)

footer_cells <- table_cells |>
  distinct(table, specification, column, column_order, observations, q_test_p) |>
  arrange(table, specification, column_order) |>
  mutate(observations = as.character(observations), q_test_p = print_cell(q_test_p)) |>
  pivot_longer(c(observations, q_test_p), names_to = "term", values_to = "value") |>
  mutate(term_order = if_else(term == "observations", 1, 2), part = "footer") |>
  arrange(table, specification, term_order, column_order) |>
  select(table, specification, column, term, part, value)

all_cells <- bind_rows(coefficient_cells, footer_cells) |>
  mutate(position = row_number(), .by = c(table, specification))

# The published specification must return every cell the appendix prints in these tables.
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            col_types = cols(table = col_character(), position = col_integer(),
                                             value_paper = col_character())) |>
  filter(table %in% names(table_indices)) |>
  mutate(value_paper = str_replace(value_paper, "^-(0\\.000\\*?)$", "\\1"))

table_comparison <- all_cells |>
  pivot_wider(id_cols = c(table, position, column, term, part), names_from = specification, values_from = value) |>
  left_join(published_cells, by = join_by(table, position), relationship = "one-to-one")

stopifnot(nrow(table_comparison) == nrow(published_cells),
          all(table_comparison$published == table_comparison$value_paper))

write_csv(table_comparison, here::here("maintained", "output", "corrected_favorability_tables.csv"))
