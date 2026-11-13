# murphy_etal_2026/ground_truth/negative_controls.R
# Output: ground_truth/negative_controls.csv
# Depends on: ground_truth/build_ground_truth.R and its inputs, errata.qmd, errata_entries.csv
# Description: Break one input at a time in a scratch copy, run the ground truth's gates on it, and
#   halt unless every broken input stops the build and the unbroken copy passes. Then do the same for
#   the errata's values_in_spans gate. This tests that the gates fire on these nine faults, not that
#   they catch every fault.

library(tidyverse)
library(here)

here::i_am("ground_truth/negative_controls.R")

script <- read_lines(here::here("ground_truth", "build_ground_truth.R")) |>
  str_subset("^here::i_am", negate = TRUE) |>
  str_replace_all("here::here\\(", "scratch_path(")

run_control <- function(break_input) {
  scratch <- tempfile("control")
  dir.create(file.path(scratch, "ground_truth"), recursive = TRUE)
  dir.create(file.path(scratch, "maintained"))
  file.symlink(here::here("maintained", "archive_run"), file.path(scratch, "maintained", "archive_run"))
  file.symlink(here::here("maintained", "output"), file.path(scratch, "maintained", "output"))
  c("published_claims.csv", "published_appendix_values.csv", "published_table_d16.csv") |>
    walk(\(f) file.copy(here::here("ground_truth", f), file.path(scratch, "ground_truth", f)))
  file.copy(here::here("errata_entries.csv"), file.path(scratch, "errata_entries.csv"))
  break_input(scratch)
  sandbox <- new.env(parent = globalenv())
  sandbox$scratch_path <- \(...) file.path(scratch, ...)
  outcome <- tryCatch({
    capture.output(eval(parse(text = script), envir = sandbox))
    "passed"
  }, error = \(e) "halted")
  unlink(scratch, recursive = TRUE)
  outcome
}

edit_csv <- function(scratch, path, edit) {
  full <- do.call(file.path, as.list(c(scratch, path)))
  read_csv(full, col_types = cols(.default = col_character())) |> edit() |> write_csv(full, na = "")
}
edit_claims <- \(scratch, edit) edit_csv(scratch, c("ground_truth", "published_claims.csv"), edit)

controls <- list(
  "no fault" = \(scratch) NULL,
  "a page value mistyped" = \(scratch) edit_claims(scratch, \(d) mutate(d, value_paper = if_else(claim_id == "abstract_ads", "49", value_paper))),
  "a pipeline claim with no computed value" = \(scratch) edit_claims(scratch, \(d) add_row(d, claim_id = "bogus", location = "x", claim = "x", claim_type = "pipeline", value_paper = "1", digits = "0")),
  "a descriptive claim with no verdict" = \(scratch) edit_claims(scratch, \(d) add_row(d, claim_id = "bogus", location = "x", claim = "x", claim_type = "descriptive", value_paper = "1", digits = "0")),
  "a duplicated claim id" = \(scratch) edit_claims(scratch, \(d) bind_rows(d, slice(d, 1))),
  "a spine id with no extraction row" = \(scratch) edit_csv(scratch, "errata_entries.csv", \(d) mutate(d, claim_ids = if_else(entry == "1", str_c(claim_ids, ";bogus"), claim_ids))),
  "a mismatch the spine does not report" = \(scratch) edit_csv(scratch, "errata_entries.csv", \(d) filter(d, entry != "2")),
  "a paper_internal row the spine does not name" = \(scratch) edit_csv(scratch, "errata_entries.csv", \(d) filter(d, entry != "5")),
  "an appendix cell mistyped" = \(scratch) edit_csv(scratch, c("ground_truth", "published_appendix_values.csv"), \(d) mutate(d, value_paper = if_else(row_number() == 1, "9.999", value_paper))),
  "a Table D.16 code changed" = \(scratch) edit_csv(scratch, c("ground_truth", "published_table_d16.csv"), \(d) mutate(d, valence = if_else(row_number() == 1, "Pro Mondale", valence)))
)

ground_truth_results <- tibble(gate = "build_ground_truth.R", control = names(controls),
                               outcome = map_chr(controls, run_control),
                               expected = if_else(control == "no fault", "passed", "halted"))

# values_in_spans, taken from the errata source so that the control runs the gate as filed ----
errata_source <- read_file(here::here("errata.qmd"))
values_in_spans <- str_extract(errata_source, regex("values_in_spans <- function.*?\\n\\}", dotall = TRUE)) |>
  (\(code) eval(parse(text = code)))()
spine <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE) |>
  filter(class == "quantity") |>
  select(!values_in_spans) |>
  mutate(corrected_values = as.character(corrected_values))

span_results <- spine |>
  mutate(filed = pmap_lgl(list(corrected, corrected_values, note), values_in_spans),
         altered = pmap_lgl(list(corrected, str_c(corrected_values, ";987.654"), note), values_in_spans)) |>
  transmute(gate = "values_in_spans", control = str_c("entry ", entry), filed, altered) |>
  pivot_longer(c(filed, altered), names_to = "input", values_to = "holds") |>
  transmute(gate, control = str_c(control, ", ", input, " values"),
            outcome = if_else(holds, "passed", "halted"),
            expected = if_else(str_ends(control, "filed values"), "passed", "halted"))

results <- bind_rows(ground_truth_results, span_results)
write_csv(results, here::here("ground_truth", "negative_controls.csv"))
print(results, n = Inf)
stopifnot(all(results$outcome == results$expected))
