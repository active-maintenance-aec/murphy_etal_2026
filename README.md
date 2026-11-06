# Active Maintenance Report: murphy_etal_2026


- [Summary](#summary)
- [Paper overview](#paper-overview)
- [Original archive reproducibility](#original-archive-reproducibility)
- [Errata](#errata)
  - [Read and not filed](#read-and-not-filed)
- [Ground truth](#ground-truth)
- [The extraction and the two
  instruments](#the-extraction-and-the-two-instruments)
- [Maintained rewrite](#maintained-rewrite)
- [Figure verification](#figure-verification)
- [R environment](#r-environment)

*Drafted by Claude Opus 5.5 under the supervision of Alex Coppock.*

# Summary

This repository reruns the deposit of an article on whether the
emotional content of campaign ads predicts their persuasive effects,
compares the article and its appendix with the rerun, and lists the
errors found.

- **The deposit runs.** All 27 of its scripts finish without error, both
  in place and after every derived file is removed.
- **The page matches the deposit.** 41 of 42 extracted numbers agree at
  the printed precision, and so do all 966 appendix table cells checked.
  The one disagreement is the two-ad sample size.
- **Matching the deposit does not make the numbers right.** Sanders
  favorability is rescaled twice in the deposit’s cleaning script, which
  the article prints faithfully. See [Errata](#errata).
- **The article’s central null holds under every correction computed
  here.** None of the 48 refits of the Figure 2, 3, and 4 slopes reaches
  p \< 0.05. These are the corrections listed in the errata and not all
  conceivable ones.
- **This repository holds no independent rewrite.** Every
  `value_rewrite` comes from rerunning the deposit’s own code, so an
  error in that code that is not reported below would pass here too.

| Link |  |
|----|----|
| Article | [10.1080/10584609.2026.2726266](https://doi.org/10.1080/10584609.2026.2726266) |
| Replication archive | [10.7910/DVN/CB3LP1](https://doi.org/10.7910/DVN/CB3LP1) |
| Errata | `murphy_etal_2026_errata.pdf` in this repository |

**Layout.** `original/` holds the deposit’s zip once fetched and
`original_extracted/` its contents; neither is redistributed.
`maintained/` holds the rerun, the comparison, and the refits.
`ground_truth/` holds the extraction from the published page and the
comparison with the rerun. `errata.qmd` is the source of the errata
note, and `errata_entries.csv` is its machine-readable copy.

**To reproduce.** Download the repository, open
`murphy_etal_2026.Rproj`, and run `source("run_all.R")`. The stripped
rerun and the accuracy rerun each took between 10 and 15 minutes here.
Set the environment variable `MURPHY_REUSE_ARCHIVE_RUN=TRUE` to reuse a
finished one.

# Paper overview

The article reports 29 waves of survey experiments fielded by YouGov
during the 2016 presidential campaign, in which subjects saw a campaign
ad or a placebo. It regresses each ad’s conditional average treatment
effect on favorability, vote choice, turnout, and issue support on
subjects’ average emotional reactions to that ad, and finds slopes that
are close to zero.

# Original archive reproducibility

The deposit is 1 zip with 107 members, all of which match the manifest
on MD5 and byte size. It ran in two passes.

| Pass | Starting point | Scripts OK | Scripts failed |
|----|----|----|----|
| In place | The deposit as shipped | 27 | 0 |
| Stripped | Every file in `output/`, `data/estimates/`, and both cleaned data files removed | 27 | 0 |

Each script’s verdict in both passes is in
`maintained/output/archive_script_verdicts.csv`. The in-place pass
overwrote every derived file, which leaves six input files as the only
ones older than the run. That pass was run once from an unzipped copy,
and its log is `maintained/output/archive_inplace_run.log`. `run_all.R`
repeats only the stripped pass.

The stripped rerun’s 60 output files compare with the deposited ones as
follows.

| verdict                                        | files |
|:-----------------------------------------------|------:|
| byte-identical                                 |    28 |
| differs                                        |     3 |
| environment record, not compared               |     1 |
| identical but for its comment header           |     1 |
| identical but for timestamp and R version      |    13 |
| numbers differ by at most measure              |     4 |
| share of channel values that differ is measure |    10 |

- The four CSVs that differ do so by at most 1.6e-09, which is numerical
  noise.
- Three PDFs differ beyond their timestamp and R version. Figures A.2
  and E.7, and their PNGs, use `position_jitter` with no seed, so they
  differ from one run to the next. Figure 1’s PDF differs in one text
  coordinate by 0.01 points, 65.35 against 65.36, read from its
  decompressed stream. The other PNGs differ in at most NA percent of
  channel values, from rasterization, and their PDFs agree.

The deposit’s audit script ends with this line:

> CLAIM SUMMARY: asserted=72 matched=67 mismatched=0 unasserted=6

Read it with care. “Asserted” counts every row, including the 5 that its
source marks `expect = "derived"`, which are printed and never compared.
Those rows and one evidence line make up “unasserted”, which is why
asserted exceeds matched with nothing mismatched. The two-ad sample size
is one of those rows, so the summary cannot fail on it.

# Errata

The note lists 13 entries: 2 corrected quantities, 7 brief corrections
to the text, and 4 notes that correct nothing because neither the page
nor the deposit settles what was intended.

| Entry | Item | Form |
|---:|:---|:---|
| 1 | Sanders favorability is rescaled twice | correction |
| 2 | The two-advertisement sample | correction |
| 3 | Turnout and issue estimates take the sign of the ad’s valence | note |
| 4 | Note 5 on the primary ads | note |
| 5 | Three attack ads are coded positive | note |
| 6 | Whose favorability | brief |
| 7 | The border item | brief |
| 8 | The note under Figure E.10 | brief |
| 9 | A dropped word | brief |
| 10 | A missing construction | brief |
| 11 | Spelling and number | brief |
| 12 | Two ads named Sacrifice | note |
| 13 | The estimator is weighted | brief |

## Read and not filed

These were read and left out of the errata. They are recorded so that a
later reader need not rediscover them.

- **“Each week” (p. 4).** The deposit has 29 dated waves spanning 34
  weeks, with six 14-day gaps. The sentence describes what happened in a
  week with a wave, and “29 weeks of experiments” counts those waves
  correctly, so no text is wrong.
- **`will_vote`.** `clean_exps.R` codes `match_DTHC` 1 and 2 as 1 and 9
  as 0, and leaves code 8 missing, which is 748 raw responses. The
  deposit holds no codebook to name that code, and the article prints no
  statement about it, so there is nothing on the page to correct.
- **“CORRECTED AT PROOF” comments in `in_text_claims.R`.** They sit
  beside stale quotations of earlier wording. The values checked here
  agree with the published PDF, so this concerns the deposit’s comments
  and not the article.

# Ground truth

`ground_truth/murphy_etal_2026_ground_truth.csv` holds 45 claims: 42
numbers compared with the rerun and 3 descriptive statements given a
computed verdict. `value_paper` is what the page prints, and
`value_rewrite` is the rerun’s value. Rows the errata reports carry a
`defect_locus`. The descriptive verdicts are 0 holding and 3 not
holding.

# The extraction and the two instruments

`published_claims.csv` covers the slopes printed in Figures 2, 3, and 4,
the Figure E.10 slope, and the counts and reliabilities in the text. It
leaves uncovered most of the matchup percentages and wave sizes that the
deposit’s own audit checks. `published_appendix_values.csv` and
`published_table_d16.csv` cover the appendix’s regression tables and its
ad-coding table.

The two instruments are the comparison in `build_ground_truth.R` and the
deposit’s own `in_text_claims.R`, which is reported above. Neither can
detect an error that the page and the code share, and the largest
entries in the errata are of that kind. They came from reading the code
and from a range table of the cleaned variables.

`build_ground_truth.R` halts on a broken join, a claim with no value or
verdict, a spine id with no extraction row, a number that disagrees with
the page and is absent from the spine, and any disagreeing appendix
cell.

| Float             | Published numbers | Agreeing |
|-------------------|-------------------|----------|
| Regression tables | 918               | 918      |
| Table D.16        | 48                | 48       |

`ground_truth/negative_controls.R` breaks one input at a time and
requires the matching gate to stop the build. All 13 of 13 runs behave
as required. These runs show that the gates fire on these faults and say
nothing about faults not tried.

# Maintained rewrite

There is none. The deposit is recent code in the same style this project
would write, so this repository reruns it and adds
`maintained/corrected_slopes.R`, which refits the published
meta-regressions on the deposit’s own estimates under the corrections.
That script’s published specification returns every printed cell of
Tables C.1, C.6, C.11, and E.17 before it applies a correction.
`maintained/corrected_accuracy.R` reruns the deposit’s two accuracy
bootstraps with the duplicated Sanders line deleted and the deposit’s
seed, since those results are not linear in the CATEs. An independent
rewrite remains undone, and it is what could catch a further error in
the estimation code.

# Figure verification

Figures are compared through the rerun’s files in the table above, and
through the slope labels on Figures 2, 3, and 4 in the ground truth.
Nobody compared the published figures with the rerun’s by eye.

# R environment

|          | Deposit                      | This rerun                   |
|----------|------------------------------|------------------------------|
| R        | R version 4.6.0 (2026-04-24) | R version 4.6.1 (2026-06-24) |
| estimatr | NA                           | NA                           |

The differences above are the only drift measured between these
environments.
