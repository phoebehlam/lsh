# Rise to Health feedback report – adding it to lshlab

## Drop-in steps

1. Copy into your lshlab package:
   - `R/rth_feedback_*.R` → `lshlab/R/`
   - `inst/rth_feedback/` → `lshlab/inst/rth_feedback/` (template, CSS, logo, simulated example data)
   - `tests/testthat/test-rth_feedback.R` → `lshlab/tests/testthat/`
2. In `DESCRIPTION`, make sure these are listed:
   - `Depends: R (>= 4.1.0)` (the code uses the native pipe `|>`)
   - `Imports: dplyr, ggplot2, grid, haven, knitr, readr, rmarkdown, stats, tibble, utils`
3. Run `devtools::document()`. Two functions are exported: `render_rth_feedback()` and
   `rth_settings()`. Everything else is internal (`@keywords internal`).
4. `devtools::test()` runs the checks. One test also uses the pilot sleep file if you
   set `Sys.setenv(RTH_PILOT_CSV = "path/to/camilletest_coded_scored_sleep.csv")`.

The template is found with `system.file("rth_feedback", package = "lshlab")`, so
nothing is hard-coded to a folder on anyone's computer.

## Use

```r
library(lshlab)
render_rth_feedback(
  id = "1234", age = 16,
  night_sav   = "rth_adolescent_night_diary.sav",
  morning_sav = "rth_adolescent_morning_diary.sav",
  sleep_csv   = "1234_scored_sleep.csv",
  out_dir     = "feedback_reports"
)
```

Writes:

- `feedback_reports/rth_feedback_1234.html` – the teen's report (one self-contained file)
- `feedback_reports/staff/rth_pairing_1234.csv` – staff-only day/night pairing check
- `feedback_reports/staff/rth_feedback_staff_log.csv` – one row per report, incl. whether the resource box was shown

Change thresholds with `st <- rth_settings(); st$min_days <- 8; render_rth_feedback(..., settings = st)`.

## How days and nights are matched

- **Night diary:** started before noon → counts for the previous day (the lab rule for
  late "yesterday" diaries). If a date ends up with two diaries, the first finished one
  is kept and the pairing file flags it.
- **Morning diary:** describes the night before its date.
- **Watch:** an in-bed interval that starts before noon (e.g. 12:41 AM) belongs to the
  previous evening.
- **Watch off:** if the morning diary says the watch wasn't worn, that night's watch data
  are dropped and flagged.
- All matching uses dates and times, never row order.

## Working from source

`source("dev/load_dev.R")` from the package root loads the functions without installing.
`dev/render_examples.R` renders the two simulated example reports; `dev/section_options.R`
renders the design options for the events-and-sleep section.
