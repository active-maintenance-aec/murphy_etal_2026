# murphy_etal_2026/download_original.R
# Output: original/, original_extracted/ (neither redistributed in this repo)
# Depends on: original_manifest.csv, original_members_manifest.csv
# Description: Fetch the deposit from Harvard Dataverse, verify the zip by MD5 and size, unpack it,
#   and verify every member. original/ must hold the zip and nothing else.

library(tidyverse)
library(here)

here::i_am("download_original.R")

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)
members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE)
stopifnot(nrow(manifest) == 1)

zip_path <- here::here("original", manifest$file)
dir.create(here::here("original"), showWarnings = FALSE)
if (is.na(tools::md5sum(zip_path)) || tools::md5sum(zip_path) != manifest$md5_served) {
  download.file(str_glue("https://dataverse.harvard.edu/api/access/datafile/{manifest$dataverse_file_id}?format=original"),
                destfile = zip_path, mode = "wb", quiet = TRUE)
}
stopifnot(unname(tools::md5sum(zip_path)) == manifest$md5_served, file.size(zip_path) == manifest$bytes)

extra <- setdiff(list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE), manifest$file)
if (length(extra) > 0) stop("original/ holds unlisted files: ", str_c(extra, collapse = ", "))

unlink(here::here("original_extracted"), recursive = TRUE)
unzip(zip_path, exdir = here::here("original_extracted"))
paths <- here::here("original_extracted", members$member)
stopifnot(all(unname(tools::md5sum(paths)) == members$md5), all(file.size(paths) == members$bytes))
on_disk <- list.files(here::here("original_extracted"), recursive = TRUE, all.files = TRUE, no.. = TRUE)
stopifnot(setequal(on_disk, members$member))
print(str_glue("Deposit verified: 1 zip and {nrow(members)} members match on MD5 and byte size."))
