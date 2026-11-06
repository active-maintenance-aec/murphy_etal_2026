# murphy_etal_2026/maintained/corrected_accuracy.R
# Output: maintained/output/corrected_accuracy.csv
# Depends on: maintained/archive_run/replication_archive/ (the deposit, rerun by maintained/run_archive.R)
# Description: Both accuracy scripts repeat the duplicated Sanders rescale of clean_exps.R. Unlike
#   the meta-regressions, these results are not linear in the CATEs: a rescaled CATE can change which
#   of two ads counts as the winner, and each bootstrap draw re-estimates the CATEs. So this script
#   copies the two scripts, deletes the one duplicated line in each, and reruns them with the
#   deposit's own seed. It writes the published and corrected estimate for every accuracy statistic.

library(tidyverse)
library(boot)
library(here)
here::i_am("maintained/corrected_accuracy.R")

archive <- here::here("maintained", "archive_run", "replication_archive")
scratch <- here::here("maintained", "archive_run", "sanders_accuracy", "replication_archive")
scripts <- c(individual = "estimate_individual_accuracy_bootstrap.R", group = "estimate_group_accuracy_bootstrap.R")
results <- c(individual = "individual_bootstrap_results_1000.rds", group = "group_bootstrap_results_1000.rds")
rescale <- "favorBS_rev = (favorBS_rev - 1)/4"

finished <- all(file.exists(file.path(scratch, "data", "estimates", results)),
                file.mtime(file.path(scratch, "data", "estimates", results)) >
                  file.mtime(file.path(scratch, "code", scripts)))

if (!(Sys.getenv("MURPHY_REUSE_ARCHIVE_RUN") == "TRUE" && finished)) {
  unlink(dirname(scratch), recursive = TRUE)
  dir.create(scratch, recursive = TRUE)
  file.copy(file.path(archive, c("code", "data")), scratch, recursive = TRUE)
  file.create(file.path(scratch, ".here"))
  walk(scripts, function(script) {
    path <- file.path(scratch, "code", script)
    code <- read_lines(path)
    hits <- which(str_detect(code, fixed(rescale)))
    stopifnot(length(hits) == 2, hits[2] == hits[1] + 1)
    write_lines(code[-hits[2]], path)
    status <- withr::with_dir(scratch, system2("Rscript", c("--vanilla", file.path("code", script)),
                                               stdout = FALSE, stderr = FALSE))
    stopifnot(status == 0)
  })
}

read_boot <- function(root, kind) {
  fit <- read_rds(file.path(root, "data", "estimates", results[[kind]]))
  tibble(kind = kind, term = names(fit$t0), estimate = unname(fit$t0),
         std_error = apply(fit$t, 2, sd, na.rm = TRUE))
}

accuracy <-
  map(names(results), read_boot, root = archive) |>
  list_rbind() |>
  left_join(map(names(results), read_boot, root = scratch) |> list_rbind(),
            by = join_by(kind, term), suffix = c("_published", "_corrected"),
            relationship = "one-to-one") |>
  mutate(
    contains_half_published = abs(estimate_published - 0.5) <= 1.96 * std_error_published,
    contains_half_corrected = abs(estimate_corrected - 0.5) <= 1.96 * std_error_corrected
  )

stopifnot(nrow(accuracy) > 0, !anyNA(accuracy$term))
write_csv(accuracy, here::here("maintained", "output", "corrected_accuracy.csv"))
