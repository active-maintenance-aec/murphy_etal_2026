# murphy_etal_2026/run_all.R
# Runs in order: verify the deposit, rerun it stripped in a scratch copy, compare that rerun with the
# deposited outputs, refit the published meta-regressions and rerun the accuracy bootstraps under the corrections, render the errata
# spine, build the ground truth, test its gates, and verify the deposit again.

library(here)
here::i_am("run_all.R")

source(here::here("download_original.R"))
source(here::here("maintained", "run_archive.R"))
source(here::here("maintained", "compare_archive_run.R"))
source(here::here("maintained", "corrected_slopes.R"))
source(here::here("maintained", "corrected_accuracy.R"))
quarto::quarto_render(here::here("errata.qmd"))
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("ground_truth", "negative_controls.R"))
source(here::here("download_original.R"))
