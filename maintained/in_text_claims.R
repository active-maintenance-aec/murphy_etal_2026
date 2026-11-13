# murphy_etal_2026/maintained/in_text_claims.R
# Output: printed CLAIM lines and a CLAIM SUMMARY line; no files
# Depends on: maintained/archive_run/ (the deposit, rerun by maintained/run_archive.R),
#   maintained/output/corrected_slopes.csv, ground_truth/published_claims.csv, errata_entries.csv,
#   excheckr
# Description: One block per row of the extraction, each recomputing the number from the rerun of
#   the deposit and from the corrected refits, by a path of its own: it never reads the ground truth
#   or the tables build_ground_truth.R reads for the same claim. A slope label is read from the
#   specification that applies every correction (corrected_slopes.R's "both"), so a slope an
#   erratum names must equal the corrected value the note prints. Every block prints
#
#       CLAIM <claim_id> = <value> || [verdict] <label>
#
#   and the expectation behind each verdict is derived by excheckr from the extraction and the
#   errata spine, never typed here. The three descriptive claims print their truth value, 1 when the
#   article's statement holds in the deposit's data and 0 when it does not; a descriptive claim that
#   does not hold scores [hedged], and build_ground_truth.R records it in holds. assert_claims()
#   stops the run on any MISMATCH, STALE, DRIFT, or MISSING, on a corrected claim this file does not
#   print, and on a difference between the claims printed and the extraction's needs_block rows.

library(tidyverse)
library(here)
library(excheckr)

here::i_am("maintained/in_text_claims.R")

archive <- here::here("maintained", "archive_run", "replication_archive")
exps <- read_rds(file.path(archive, "data", "exps.rds"))
exps_clean <- read_rds(file.path(archive, "data", "exps_clean.rds"))
ads <- read_csv(file.path(archive, "data", "ad_df_emotions.csv"), show_col_types = FALSE)
cates <- read_rds(file.path(archive, "data", "estimates", "all_cates.rds"))
reliability <- bind_rows(
  read_csv(file.path(archive, "output", "tables", "emotion_scale_reliability.csv"), show_col_types = FALSE),
  read_csv(file.path(archive, "output", "tables", "rating_scale_reliability.csv"), show_col_types = FALSE)
)
slopes <- read_csv(here::here("maintained", "output", "corrected_slopes.csv"), show_col_types = FALSE) |>
  filter(specification == "both")

published <- read_csv(here::here("ground_truth", "published_claims.csv"),
                      col_types = cols(.default = col_character()))
errata <- read_csv(here::here("errata_entries.csv"), col_types = cols(.default = col_character()))

claim_start(published = published, errata = errata, format = "id")

one <- function(x) {
  stopifnot(length(x) == 1, !is.na(x))
  x
}
slope <- function(dv_name, index_name, column) {
  one(slopes[[column]][slopes$dv == dv_name & slopes$index == index_name])
}
valence_count <- \(label) sum(ads$ad_valence == label)
alpha <- \(scale_name) one(reliability$cronbach_alpha[reliability$scale == scale_name])
ads_fielded <- exps_clean |> filter(ad_id != 3) |> distinct(ad_id, date)

# Design: waves, ads, and treatments (abstract and p. 4) ----
claim("abstract_weeks", n_distinct(exps_clean$date))
claim("abstract_ads", n_distinct(ads_fielded$ad_id))
claim("design_treatments", nrow(ads_fielded))
claim("valence_pro_clinton", valence_count("Pro Clinton"))
claim("valence_pro_trump", valence_count("Pro Trump"))
claim("valence_anti_clinton", valence_count("Anti Clinton"))
claim("valence_anti_trump", valence_count("Anti Trump"))
claim("valence_pro_kasich", valence_count("Pro Kasich"))
claim("valence_pro_cruz", valence_count("Pro Cruz"))
claim("valence_pro_sanders", valence_count("Pro Sanders"))

# Scale reliability (p. 4 and Appendix E) ----
claim("alpha_positive", alpha("positive"))
claim("alpha_negative", alpha("negative"))
claim("e10_alpha", one(reliability$cronbach_alpha[!reliability$scale %in% c("positive", "negative")]))

# Contrast ads (p. 5) ----
contrast <- ads |> filter(emotion_valence == "both")
claim("contrast_ads", nrow(contrast))
claim("contrast_trump", sum(contrast$ad_sponsor == "trump"))

# Tests and estimates: one test per ad and wave, one estimate per partisan group (p. 7) ----
favorability <- cates |> filter(str_starts(outcome, "favor"))
vote_choice <- cates |> filter(str_starts(outcome, "general_vote"))
claim("fav_tests", nrow(distinct(favorability, date, ad_id, outcome)))
claim("fav_estimates", nrow(favorability))
claim("vote_tests", nrow(distinct(vote_choice, date, ad_id, outcome)))
claim("vote_estimates", nrow(vote_choice))

# Slope labels on Figures 2, 3, and 4, under every correction ----
figure_slopes <- tribble(
  ~stem, ~dv, ~index,
  "fig2_fav_neg", "Favorability", "negative_index",
  "fig2_vote_neg", "Vote Choice", "negative_index",
  "fig2_fav_pos", "Favorability", "positive_index",
  "fig2_vote_pos", "Vote Choice", "positive_index",
  "fig3_fav", "Favorability", "intended",
  "fig3_vote", "Vote Choice", "intended",
  "fig4_issue_neg", "Target Issue Support", "negative_index",
  "fig4_turnout_neg", "Turnout Intentions", "negative_index",
  "fig4_issue_pos", "Target Issue Support", "positive_index",
  "fig4_turnout_pos", "Turnout Intentions", "positive_index"
)
pwalk(figure_slopes, \(stem, dv, index) {
  claim(str_c(stem, "_est"), slope(dv, index, "estimate"))
  claim(str_c(stem, "_se"), slope(dv, index, "std_error"))
})

# Ratings (Appendix E) ----
claim("e10_fav_slope", slope("Favorability", "rating_index", "estimate"))
claim("e10_fav_points", 100 * slope("Favorability", "rating_index", "estimate"))
claim("two_ad_n", sum(exps$assignment == "Two Ads"))

# Descriptive claims about the ad codes (p. 5 and note 5) ----
primary_ads <- exps_clean |>
  filter(election_phase == "Primary Election", ad_id != 3) |>
  distinct(ad_id) |>
  left_join(ads, by = join_by(ad_id), relationship = "one-to-one")
# "Nearly all" is read generously as at least 90 percent.
claim("fn5_nearly_all_positive", as.numeric(mean(primary_ads$emotion_valence == "positive") >= 0.9))
claim("fn5_sponsor_party", as.numeric(!any(primary_ads$ad_valence == "Anti Trump" &
                                            primary_ads$ad_sponsor %in% c("our principles pac", "cruz"))))
claim("attack_coded_negative", as.numeric(!any(str_starts(ads$ad_valence, "Anti") & ads$emotion_valence == "positive")))

# Gates ----
assert_claims()
invisible(claim_summary())
