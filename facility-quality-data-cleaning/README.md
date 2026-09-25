# Facility & Quality Data Cleaning

An R data-engineering pipeline that merges two multi-year administrative
data feeds at the health-facility level — a facility directory/roster
extract and a quality-measure reporting extract — patches in a
late-arriving reporting year that ships as two separate summary workbooks
instead of the regular file drop, reshapes the result into a long panel,
and applies small-cell suppression to a low-count demographic subgroup.

Demonstrates: parsing a batch of similarly-named source files to extract
metadata (source system, reporting year) from the filename itself,
iteratively merging many yearly extracts with an audited join that tags
match status instead of silently dropping rows, reconciling two
differently-labeled measures from parallel workbooks into one weighted
average, wide-to-long reshaping across suffixed year columns, and a
small-cell demographic suppression routine that zeroes out a subgroup and
re-derives every dependent total and percentage so the identities still
hold.

**This is a synthetic demonstration.** The two source systems, the state,
every facility/health-system name, and all data are fictional, generated
by `R/generate_synthetic_source_files.R` with a fixed seed. Nothing here
reflects any real employer, client, or dataset.

## Structure

- `R/generate_synthetic_source_files.R` — writes synthetic raw files to
  `data/raw/`: five years of a facility-directory CSV, four years of a
  quality-reporting CSV, and two Excel workbooks standing in for a
  late-arriving reporting year.
- `R/utils.R` — `isid()` (Stata-style uniqueness assertion) and
  `full_join_check()` / `left_join_check()` (joins that tag every row
  matched/unmatched instead of dropping silently).
- `analysis/facility-quality-cleaning.R` — the main pipeline. It generates
  its own raw files on each run, then cleans and merges them.

## Running it

From this folder:

```r
source("analysis/facility-quality-cleaning.R")
```

Requires: `dplyr`, `tidyr`, `purrr`, `readr`, `readxl`, `assertr`, `glue`,
`tidylog`, `here`, `openxlsx`. Writes intermediate `.RDS` files to
`data/intermediate/`.
