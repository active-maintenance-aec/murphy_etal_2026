# murphy_etal_2026/maintained/count_figure_quantities.R
# Output: maintained/output/figure_quantities.csv
# Depends on: maintained/archive_run/ (the deposit, rerun by maintained/run_archive.R)
# Description: Count what each of the deposit's figures prints and plots, from the plot object its
#   own script builds. Each figure script runs in a fresh R process on a scratch copy of the rerun,
#   with ggsave() replaced by a function that keeps the plot instead of writing it. A ggplot's
#   printed numbers are the numeric tokens in text layers whose labels are numbers alone (slope
#   labels and accuracy statistics, not ad titles), and its plotted quantities are the rows of its
#   point layers. A ggpairs matrix prints one correlation in each upper panel and plots a density on
#   each diagonal panel and a scatter in each lower one.

library(tidyverse)
library(here)

here::i_am("maintained/count_figure_quantities.R")

rerun <- here::here("maintained", "archive_run", "replication_archive")
scratch <- tempfile("figures")
dir.create(scratch)
file.copy(rerun, scratch, recursive = TRUE)
archive <- file.path(scratch, "replication_archive")

counter <- tempfile(fileext = ".R")
write_lines(c(
  "args <- commandArgs(trailingOnly = TRUE)",
  "kept <- list()",
  "ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {",
  "  kept[[tools::file_path_sans_ext(basename(filename))]] <<- plot",
  "  invisible(filename)",
  "}",
  "source(args[1])",
  "count_plot <- function(p) {",
  "  if (inherits(p, 'ggmatrix')) {",
  "    k <- p$ncol",
  "    return(c(printed = k * (k - 1) / 2, plotted = k + k * (k - 1) / 2))",
  "  }",
  "  built <- ggplot2::ggplot_build(p)",
  "  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))",
  "  labels <- unlist(lapply(built$data[geoms %in% c('GeomText', 'GeomLabel')], function(d) as.character(d$label)))",
  "  labels <- labels[grepl('^[-0-9.() *,]+$', labels)]",
  "  printed <- sum(lengths(regmatches(labels, gregexpr('-?[0-9]*\\\\.?[0-9]+', labels))))",
  "  plotted <- sum(vapply(built$data[geoms == 'GeomPoint'], nrow, integer(1)))",
  "  c(printed = printed, plotted = plotted)",
  "}",
  "counts <- t(vapply(kept, count_plot, numeric(2)))",
  "write.csv(data.frame(figure_file = rownames(counts), counts), args[2], row.names = FALSE)"
), counter)

count_script <- function(script) {
  result <- tempfile(fileext = ".csv")
  log_lines <- withr::with_dir(archive, system2("Rscript", c(counter, script, result), stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(log_lines, "status"))) stop("Counting ", script, " failed:\n", str_c(tail(log_lines, 10), collapse = "\n"))
  read_csv(result, show_col_types = FALSE) |> mutate(script = basename(script))
}

figure_scripts <- list.files(file.path(archive, "code"), "^figure_.*\\.R$", full.names = TRUE)
figure_quantities <- map(figure_scripts, count_script) |>
  list_rbind() |>
  mutate(id = str_match(figure_file, "^figure_([a-e]?)([0-9]+)_")[, 2:3] |>
           (\(m) str_c(str_to_upper(m[, 1]), if_else(m[, 1] == "", "", "."), m[, 2]))(),
         float = str_c("Figure ", id)) |>
  distinct(float, script, printed, plotted)

stopifnot(!anyDuplicated(figure_quantities$float), nrow(figure_quantities) == length(figure_scripts))
unlink(scratch, recursive = TRUE)
write_csv(figure_quantities, here::here("maintained", "output", "figure_quantities.csv"))
print(figure_quantities, n = Inf)
