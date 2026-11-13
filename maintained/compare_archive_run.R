# murphy_etal_2026/maintained/compare_archive_run.R
# Output: maintained/output/archive_output_comparison.csv, ground_truth/archive_run_status.csv,
#   ground_truth/archive_overwrites.csv
# Depends on: original_extracted/, maintained/archive_run/, maintained/output/archive_run.log,
#   maintained/output/archive_inplace_run.log
# Description: Compare every output file of the stripped rerun with the file the deposit ships, record
#   each script's outcome in both passes from their logs, and record what the stripped pass did to
#   each file of the deposit. A difference only in a timestamp or the R version string does not count
#   as a difference, and a CSV is compared number by number.

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

# One row per script per pass ----
# The deposit's run_all.R runs each script in its own R process and prints OK only on exit status
# 0; on a failure it prints FAILED, then the script's output, and stops. A script it never reached
# is not_run. helpers.R is only sourced by the other scripts, so it gets no row. The as_shipped pass
# is the in-place run, made once from an unzipped copy; run_all.R repeats only the stripped pass.
deposit_root <- here::here("original_extracted", "replication_archive")
scripts <- str_match_all(read_file(file.path(deposit_root, "run_all.R")), "run\\(\"(code/[^\"]+)\"\\)")[[1]][, 2]

read_status <- function(log_name, pass) {
  log_lines <- read_lines(here::here("maintained", "output", log_name))
  ran <- str_match(log_lines, "Running (code/\\S+) \\.\\.\\. (\\S+)") |>
    (\(m) tibble(script = m[, 2], verdict = m[, 3]))() |>
    drop_na()
  stopifnot(all(ran$verdict %in% c("OK", "FAILED")), all(ran$script %in% scripts))
  failed <- ran$script[ran$verdict == "FAILED"]
  error_line <- str_subset(log_lines, "^Error") |> first() |> str_trunc(150)
  tibble(pass = pass, script = scripts) |>
    left_join(ran, by = "script", relationship = "one-to-one") |>
    mutate(role = case_when(str_detect(script, "/clean_") ~ "cleaning",
                            str_ends(script, "in_text_claims.R") ~ "other",
                            .default = "analysis"),
           exit_status = if_else(verdict == "OK", 0L, NA_integer_),
           outcome = case_when(verdict == "OK" ~ "clean", verdict == "FAILED" ~ "error", .default = "not_run"),
           stopped_at = case_when(outcome == "error" ~ error_line,
                                  outcome == "not_run" ~ str_c("not reached: the run stopped at ", first(failed)),
                                  .default = NA_character_)) |>
    select(pass, script, role, exit_status, outcome, stopped_at)
}

run_status <- bind_rows(read_status("archive_inplace_run.log", "as_shipped"),
                        read_status("archive_run.log", "stripped"))
write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
print(count(run_status, pass, outcome))

# What the stripped pass did to each file of the deposit ----
# run_archive.R removes every file under output/ and data/estimates/ and the two cleaned data files
# before the run. Each comes back rebuilt or stays withheld. A file the deposit does not ship is
# added, a shipped input the run changed is overwritten, and one it removed is deleted. An output
# file matches the deposit on the verdicts above, with a CSV allowed a gap of 1e-6, far below the three
# decimals the article prints; a PNG must agree in every channel value. An rds file matches when its
# object agrees to the same tolerance.
rerun_root <- here::here("maintained", "archive_run", "replication_archive")
deposit_files <- list.files(deposit_root, recursive = TRUE, all.files = TRUE)
rerun_files <- list.files(rerun_root, recursive = TRUE, all.files = TRUE)
removed <- deposit_files |>
  str_subset("^output/|^data/estimates/|^data/(exps_clean|individual_ratings_clean)\\.rds$")

output_matches <- comparison |>
  transmute(file = str_c("output/", file),
            matches = case_when(
              verdict %in% c("byte-identical", "identical but for its comment header",
                             "identical but for timestamp and R version") ~ TRUE,
              str_ends(file, "csv") ~ measure < 1e-6,
              str_ends(file, "png") ~ measure == 0,
              .default = FALSE))

same_contents <- function(file) {
  if (file %in% output_matches$file) return(output_matches$matches[output_matches$file == file])
  deposited <- file.path(deposit_root, file)
  rebuilt <- file.path(rerun_root, file)
  if (str_ends(file, "\\.rds")) return(isTRUE(all.equal(read_rds(rebuilt), read_rds(deposited), tolerance = 1e-6)))
  unname(tools::md5sum(rebuilt) == tools::md5sum(deposited))
}

kept <- setdiff(deposit_files, removed)
archive_overwrites <- bind_rows(
  tibble(file = removed, effect = if_else(removed %in% rerun_files, "rebuilt", "withheld")),
  tibble(file = intersect(kept, rerun_files)) |>
    filter(tools::md5sum(file.path(rerun_root, file)) != tools::md5sum(file.path(deposit_root, file))) |>
    mutate(effect = "overwritten"),
  tibble(file = setdiff(kept, rerun_files), effect = "deleted"),
  tibble(file = setdiff(rerun_files, deposit_files), effect = "added")
) |>
  mutate(pass = "stripped",
         matches_deposit = map2_lgl(file, effect, \(f, e) if (e %in% c("rebuilt", "overwritten")) same_contents(f) else NA)) |>
  select(pass, file, effect, matches_deposit)
write_csv(archive_overwrites, here::here("ground_truth", "archive_overwrites.csv"))
print(count(archive_overwrites, effect, matches_deposit))
