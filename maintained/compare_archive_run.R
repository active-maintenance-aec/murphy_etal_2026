# murphy_etal_2026/maintained/compare_archive_run.R
# Output: maintained/output/archive_output_comparison.csv, maintained/output/archive_script_verdicts.csv
# Depends on: original_extracted/, maintained/archive_run/, maintained/output/archive_run.log,
#   maintained/output/archive_inplace_run.log
# Description: Compare every output file of the stripped rerun with the file the deposit ships, and
#   record each script's verdict from both passes' logs. A difference only in a timestamp or the R
#   version string does not count as a difference, and a CSV is compared number by number.

library(tidyverse)
library(here)

here::i_am("maintained/compare_archive_run.R")

deposit <- here::here("original_extracted", "replication_archive", "output")
rerun <- here::here("maintained", "archive_run", "replication_archive", "output")

files <- list.files(deposit, recursive = TRUE)
stopifnot(setequal(files, list.files(rerun, recursive = TRUE)))

read_raw <- \(path) readBin(path, "raw", file.size(path))

# Timestamps and the version string keep their byte width, so offsets stay valid.
blank_pdf <- function(bytes) {
  for (pattern in c("D:[0-9]{14}", "R [0-9]\\.[0-9]\\.[0-9]")) {
    starts <- grepRaw(pattern, bytes, all = TRUE)
    width <- if (str_starts(pattern, "D")) 15 else 6
    for (start in starts) bytes[start:(start + width)] <- as.raw(0)
  }
  bytes
}

compare_csv <- function(left, right) {
  a <- read_csv(left, show_col_types = FALSE)
  b <- read_csv(right, show_col_types = FALSE)
  stopifnot(identical(names(a), names(b)), nrow(a) == nrow(b))
  numeric_columns <- names(a)[map_lgl(a, is.numeric)]
  stopifnot(identical(select(a, !all_of(numeric_columns)), select(b, !all_of(numeric_columns))))
  max(abs(as.matrix(a[numeric_columns]) - as.matrix(b[numeric_columns])), na.rm = TRUE)
}

compare_png <- function(left, right) {
  a <- png::readPNG(left)
  b <- png::readPNG(right)
  stopifnot(identical(dim(a), dim(b)))
  mean(abs(a - b) > 0)
}

compare_file <- function(file) {
  left <- file.path(deposit, file)
  right <- file.path(rerun, file)
  if (identical(read_raw(left), read_raw(right))) return(tibble(file, verdict = "byte-identical", measure = NA_real_))
  extension <- tools::file_ext(file)
  if (extension == "csv") {
    return(tibble(file, verdict = "numbers differ by at most measure", measure = compare_csv(left, right)))
  }
  if (extension == "tex") {
    same <- identical(str_subset(read_lines(left), "^%", negate = TRUE), str_subset(read_lines(right), "^%", negate = TRUE))
    return(tibble(file, verdict = if (same) "identical but for its comment header" else "differs", measure = NA_real_))
  }
  if (extension == "pdf") {
    same <- identical(blank_pdf(read_raw(left)), blank_pdf(read_raw(right)))
    return(tibble(file, verdict = if (same) "identical but for timestamp and R version" else "differs", measure = NA_real_))
  }
  if (extension == "png") {
    return(tibble(file, verdict = "share of channel values that differ is measure", measure = compare_png(left, right)))
  }
  tibble(file, verdict = "environment record, not compared", measure = NA_real_)
}

comparison <- map(files, compare_file) |> list_rbind()
write_csv(comparison, here::here("maintained", "output", "archive_output_comparison.csv"))
print(count(comparison, verdict))

# One row per script, one column per pass ----
read_verdicts <- function(log_name, pass) {
  read_lines(here::here("maintained", "output", log_name)) |>
    str_subset("^\\s*Running code/") |>
    str_match("Running (code/\\S+) \\.\\.\\. (\\S+)") |>
    (\(m) tibble(script = m[, 2], verdict = m[, 3], pass = pass))()
}

script_verdicts <- bind_rows(read_verdicts("archive_run.log", "stripped"),
                             read_verdicts("archive_inplace_run.log", "in_place")) |>
  pivot_wider(names_from = pass, values_from = verdict)
stopifnot(!anyNA(script_verdicts))
write_csv(script_verdicts, here::here("maintained", "output", "archive_script_verdicts.csv"))
print(count(script_verdicts, stripped, in_place))
