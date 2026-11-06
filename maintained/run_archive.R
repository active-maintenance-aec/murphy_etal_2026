# murphy_etal_2026/maintained/run_archive.R
# Output: maintained/archive_run/ (gitignored), maintained/output/archive_claims.txt
# Depends on: original_extracted/ (rebuilt by download_original.R)
# Description: Copy the deposit to a scratch directory, strip every derived file, and run its own
#   run_all.R there. About 13 minutes. Set MURPHY_REUSE_ARCHIVE_RUN=TRUE to reuse a finished run.

library(tidyverse)
library(here)
here::i_am("maintained/run_archive.R")

run_dir <- here::here("maintained", "archive_run")
archive <- file.path(run_dir, "replication_archive")
finished <- file.exists(file.path(archive, "output", "session_info.txt"))

if (!(Sys.getenv("MURPHY_REUSE_ARCHIVE_RUN") == "TRUE" && finished)) {
  dir.create(run_dir, showWarnings = FALSE)
  unlink(archive, recursive = TRUE)
  file.copy(here::here("original_extracted", "replication_archive"), run_dir, recursive = TRUE)
  derived <- c(list.files(file.path(archive, "output"), recursive = TRUE, full.names = TRUE),
               list.files(file.path(archive, "data", "estimates"), full.names = TRUE),
               file.path(archive, "data", c("exps_clean.rds", "individual_ratings_clean.rds")))
  unlink(derived)
  log_lines <- withr::with_dir(archive, system2("Rscript", "run_all.R", stdout = TRUE, stderr = TRUE))
  stopifnot(is.null(attr(log_lines, "status")))
  write_lines(str_replace_all(log_lines, "/\\S*/replication_archive", "<deposit>"),
              here::here("maintained", "output", "archive_run.log"))
}
