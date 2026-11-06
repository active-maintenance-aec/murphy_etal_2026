# murphy_etal_2026/ground_truth/build_ground_truth.R
# Output: ground_truth/murphy_etal_2026_ground_truth.csv, ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/published_appendix_values.csv,
#   ground_truth/published_table_d16.csv, errata_entries.csv, maintained/archive_run/
# Description: Compare every extracted published value with the deposit's rerun at the page's
#   precision, then halt on a broken join, an adverse row with no locus, a locus on a clean match
#   outside the spine, or a spine id with no extraction row. value_rewrite comes from the rerun of
#   the deposit's code, since this repo holds no independent rewrite.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

archive <- here::here("maintained", "archive_run", "replication_archive")
tables <- file.path(archive, "output", "tables")
read_table <- \(name) read_csv(file.path(tables, name), show_col_types = FALSE)

claims <- read_csv(here::here("ground_truth", "published_claims.csv"),
                   col_types = cols(.default = col_character(), digits = col_integer()))
exps_clean <- read_rds(file.path(archive, "data", "exps_clean.rds"))
pairwise <- read_rds(file.path(archive, "data", "estimates", "observed_pairwise_frame.rds"))
ads <- read_csv(file.path(archive, "data", "ad_df_emotions.csv"), show_col_types = FALSE)

meta <- function(file, index_name, dv_name, column) {
  hit <- read_table(file) |> filter(index == index_name, dv == dv_name)
  stopifnot(nrow(hit) == 1)
  hit[[column]]
}
valence_count <- \(label) sum(ads$ad_valence == label)
alpha <- read_table("emotion_scale_reliability.csv")
counts <- read_table("cate_counts_by_outcome.csv")
tally <- \(dv_name, column) counts[[column]][counts$dv == dv_name]

slope_rows <- tribble(
  ~stem, ~file, ~index, ~dv,
  "fig2_fav_neg", "figure_2_meta_estimates.csv", "negative_index", "Favorability",
  "fig2_vote_neg", "figure_2_meta_estimates.csv", "negative_index", "Vote Choice",
  "fig2_fav_pos", "figure_2_meta_estimates.csv", "positive_index", "Favorability",
  "fig2_vote_pos", "figure_2_meta_estimates.csv", "positive_index", "Vote Choice",
  "fig3_fav", "figure_3_meta_estimates.csv", "intended_index", "Favorability",
  "fig3_vote", "figure_3_meta_estimates.csv", "intended_index", "Vote Choice",
  "fig4_issue_neg", "figure_4_meta_estimates.csv", "negative_index", "Target Issue Support",
  "fig4_turnout_neg", "figure_4_meta_estimates.csv", "negative_index", "Turnout Intentions",
  "fig4_issue_pos", "figure_4_meta_estimates.csv", "positive_index", "Target Issue Support",
  "fig4_turnout_pos", "figure_4_meta_estimates.csv", "positive_index", "Turnout Intentions"
)
slope_values <- slope_rows |>
  mutate(est = pmap_dbl(list(file, index, dv), \(f, i, d) meta(f, i, d, "estimate")),
         se = pmap_dbl(list(file, index, dv), \(f, i, d) meta(f, i, d, "std.error"))) |>
  select(stem, est, se) |>
  pivot_longer(c(est, se), names_to = "part", values_to = "value_rewrite") |>
  transmute(claim_id = str_c(stem, "_", part), value_rewrite)

e10_slope <- meta("figure_e10_meta_estimates.csv", "rating_index", "Favorability", "estimate")

other_values <- tribble(
  ~claim_id, ~value_rewrite,
  "abstract_weeks", n_distinct(exps_clean$date),
  "abstract_ads", nrow(ads),
  "design_treatments", nrow(distinct(filter(exps_clean, ad_id != 3), ad_id, date)),
  "valence_pro_clinton", valence_count("Pro Clinton"),
  "valence_pro_trump", valence_count("Pro Trump"),
  "valence_anti_clinton", valence_count("Anti Clinton"),
  "valence_anti_trump", valence_count("Anti Trump"),
  "valence_pro_kasich", valence_count("Pro Kasich"),
  "valence_pro_cruz", valence_count("Pro Cruz"),
  "valence_pro_sanders", valence_count("Pro Sanders"),
  "alpha_positive", alpha$cronbach_alpha[alpha$scale == "positive"],
  "alpha_negative", alpha$cronbach_alpha[alpha$scale == "negative"],
  "contrast_ads", sum(ads$emotion_valence == "both"),
  "contrast_trump", sum(ads$emotion_valence == "both" & ads$ad_sponsor == "trump"),
  "fav_tests", tally("Favorability", "tests"),
  "fav_estimates", tally("Favorability", "estimates"),
  "vote_tests", tally("Vote Choice", "tests"),
  "vote_estimates", tally("Vote Choice", "estimates"),
  "e10_alpha", read_table("rating_scale_reliability.csv")$cronbach_alpha,
  "e10_fav_slope", e10_slope,
  "e10_fav_points", 100 * e10_slope,
  "two_ad_n", n_distinct(pairwise$caseid)
)

# Descriptive claims: the verdict is computed, and each threshold is stated here ----
primary_ads <- exps_clean |>
  filter(election_phase == "Primary Election", ad_id != 3) |>
  distinct(ad_id) |>
  left_join(ads, by = join_by(ad_id), relationship = "one-to-one")
primary_positive_share <- mean(primary_ads$emotion_valence == "positive")
republican_sponsors <- c("our principles pac", "cruz")
republican_anti_trump <- primary_ads |>
  filter(ad_valence == "Anti Trump", ad_sponsor %in% republican_sponsors)
attack_positive <- ads |> filter(str_detect(ad_valence, "Anti"), emotion_valence == "positive")

holds_values <- tribble(
  ~claim_id, ~holds,
  # "Nearly all" is read generously as at least 90 percent.
  "fn5_nearly_all_positive", as.numeric(primary_positive_share >= 0.9),
  "fn5_sponsor_party", as.numeric(nrow(republican_anti_trump) == 0),
  "attack_coded_negative", as.numeric(nrow(attack_positive) == 0)
)

spine <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE)
spine_ids <- spine$claim_ids |> na.omit() |> str_split(";") |> unlist() |> discard(\(x) x == "")

normalize <- \(x) str_replace(x, "^-(0(\\.0*)?)(\\*?)$", "\\1\\3")
ground_truth <- claims |>
  left_join(bind_rows(slope_values, other_values), by = "claim_id", relationship = "one-to-one") |>
  left_join(holds_values, by = "claim_id", relationship = "one-to-one") |>
  mutate(printed = if_else(is.na(value_rewrite), NA_character_,
                           normalize(sprintf(str_c("%.", digits, "f"), value_rewrite))),
         match_rewrite = if_else(is.na(printed), NA_real_, as.numeric(printed == normalize(value_paper))),
         defect_locus = case_when(
           claim_id %in% c("fn5_nearly_all_positive", "fn5_sponsor_party", "attack_coded_negative") ~ "paper_internal",
           claim_id == "two_ad_n" & match_rewrite == 0 ~ "unresolved",
           claim_id %in% spine_ids & match_rewrite == 1 ~ "archive",
           match_rewrite == 0 ~ "unresolved",
           .default = NA_character_),
         paper_id = "murphy_etal_2026",
         notes = "") |>
  select(paper_id, claim_id, table_figure, claim, claim_type, value_paper, value_rewrite, match_rewrite, holds, defect_locus, notes)

# Gates ----
stopifnot(
  # Every pipeline claim has a value and every descriptive claim a verdict.
  all(!is.na(ground_truth$value_rewrite[ground_truth$claim_type == "pipeline"])),
  all(!is.na(ground_truth$holds[ground_truth$claim_type == "descriptive"])),
  # The locus rule: adverse rows carry one, and a clean match only as a listed printed defect.
  with(ground_truth, all(!is.na(defect_locus)[which(match_rewrite == 0 | holds == 0)])),
  with(ground_truth, all(claim_id[which(match_rewrite == 1 & !is.na(defect_locus))] %in% spine_ids)),
  # The spine's ids all name an extraction row.
  all(spine_ids %in% ground_truth$claim_id)
)
write_csv(ground_truth, here::here("ground_truth", "murphy_etal_2026_ground_truth.csv"))

# Appendix tables, cell by cell, in the order each table prints them ----
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            col_types = cols(table = col_character(), position = col_integer(),
                                             value_paper = col_character()))
table_files <- tibble(path = list.files(tables, "^table_[ce][0-9]+_.*\\.tex$", full.names = TRUE)) |>
  mutate(table = str_match(basename(path), "^table_([ce])([0-9]+)_") |>
           (\(m) str_c(str_to_upper(m[, 2]), ".", m[, 3]))())
token <- "(?<![\\w.{}\\[])-?\\d*\\.?\\d+\\*?(?![\\w.\\]])"
deposit_cells <- table_files |>
  mutate(value_rewrite = map(path, \(p) {
    text <- read_file(p) |> str_replace_all("\\\\num\\{([^}]*)\\}", "\\1") |>
      str_remove(regex("^.*?(?=Intercept)", dotall = TRUE)) |>
      str_remove(regex("\\\\end\\{.*$", dotall = TRUE))
    str_extract_all(text, token)[[1]]
  })) |>
  select(table, value_rewrite) |>
  unnest(value_rewrite) |>
  mutate(position = row_number(), .by = table)

cells <- published_cells |>
  full_join(deposit_cells, by = c("table", "position"), relationship = "one-to-one") |>
  mutate(agrees = coalesce(normalize(value_paper) == normalize(value_rewrite), FALSE))
d16_published <- read_csv(here::here("ground_truth", "published_table_d16.csv"), show_col_types = FALSE)
d16_agrees <- d16_published |>
  inner_join(ads, by = "ad_id", relationship = "one-to-one") |>
  summarise(n = n(), agrees = sum(valence == ad_valence & emotion_valence.x == emotion_valence.y))
float_coverage <- cells |>
  summarise(published_numbers = sum(!is.na(value_paper)), covered = sum(agrees), .by = table) |>
  add_row(table = "D.16", published_numbers = nrow(d16_published), covered = d16_agrees$agrees)
write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
print(float_coverage, n = 20)
print(filter(cells, !agrees), n = 30)
print(count(ground_truth, match_rewrite, holds, defect_locus))
print(filter(ground_truth, match_rewrite == 0 | holds == 0) |> select(claim_id, value_paper, value_rewrite, holds))
