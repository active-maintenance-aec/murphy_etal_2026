# murphy_etal_2026/run_all.R
# Runs in order: verify the deposit, rerun it stripped in a scratch copy, refit the published
# meta-regressions under the corrections, render the errata spine, build the ground truth, and
# verify the deposit again.

library(here)
here::i_am("run_all.R")

source(here::here("download_original.R"))
source(here::here("maintained", "run_archive.R"))
source(here::here("maintained", "corrected_slopes.R"))
quarto::quarto_render(here::here("errata.qmd"))
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("download_original.R"))
