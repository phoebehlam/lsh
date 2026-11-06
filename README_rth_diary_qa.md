# Rise to Health weekly diary QA

Finds duplicate, binged and late diary entries in the three diaries (caregiver night,
adolescent night, adolescent morning), proposes what to do with each, and – once the
proposals are reviewed – writes clean data and keeps a log of every decision.

## Each week

```r
library(lsh)
cg <- "rth_caregiver night diary_November 13, 2026_13.14.sav"
an <- "rth_adolescent night diary_November 13, 2026_13.15.sav"
am <- "rth_adolescent morning diary_November 13, 2026_13.15.sav"

# step 1: report + proposals
rth_diary_qa(cg, an, am, out_dir = "diary qa")

# open diary qa/rth_diary_qa_proposals_2026-11-13.xlsx, change final_action /
# final_should_date where you disagree, fill in the "needs a decision" rows

# step 2: same exports + the reviewed proposals
rth_diary_qa(cg, an, am, proposals = "diary qa/rth_diary_qa_proposals_2026-11-13.xlsx",
             out_dir = "diary qa")
```

Always use the Qualtrics exports as downloaded (never edited) and the same `out_dir`
(or `log =`) every week: the decisions log is what carries decisions from week to week.

## Files

| File | Written by | What it is |
|---|---|---|
| `rth_diary_qa_<date>.html` | step 1 | The report: one section per diary, tagged participants with the data as recorded and after the proposals |
| `rth_diary_qa_proposals_<date>.xlsx` | step 1 | One tab per diary, one row per tagged submission. Edit only the shaded columns |
| `rth_diary_qa_decisions_log.xlsx` | step 2 | Every decision ever made, one tab per diary. Decisions here are applied first and never proposed again |
| `rth_<diary>_qa_<date>.sav` | step 2 | Clean data: every response (previews left out), one row per ID, with `id`, `diary_date`, `should_date`, `should_day`, `compliance`, `exclude`, `qa_rule`, `qa_action`, `qa_source` added at the end. Leave out rows with `exclude == 1` |

Step 2 stops without writing anything if a "needs a decision" row is still blank, a
date can't be read, a change puts two entries on the same night/day, or the export has
new tagged entries that aren't in the proposals file (then run step 1 again).

## Rules

- **Dates.** Night diaries started before 12:00 count for the previous night; morning
  diaries started before 04:00 count for the previous day.
- **Compliance.** Night: 1 = on time (by 04:00 next day), 2 = 04:00–11:59 next day,
  3 = 12:00 next day or later. Morning: 1 = before 14:00, 2 = 14:00–23:59, 3 = next day or later.
- **R0** unfinished entry and a finished one for the same date → drop the unfinished one.
- **R1** 2+ entries, the night/day before is empty → the earliest moves back one.
- **R2** 2+ entries, the night/day before is filled → keep the earliest finished one, drop the rest.
- **R3** (night diaries) one entry started 12:00–19:00, the night before is empty, not the
  participant's first entry → it moves back one night.
- **R99** anything that looks off (e.g. an entry that took 0 seconds, or a clash with an
  earlier week's decision) → needs a decision.

A caregiver with two teens (externalreference `1212, 1172`) is reviewed once; the
decision applies to both IDs and the clean data has one row per ID.

Change cut-offs with `s <- rth_diary_qa_settings(); s$r3_until <- 20; rth_diary_qa(..., settings = s)`.

## Example and tests

`inst/rth_diary_qa/example_data/` has simulated exports (made-up IDs 9011–9082; no real
data), built by `inst/rth_diary_qa/simulate_rth_diary_qa.R`. Each made-up participant
shows one situation. `tests/testthat/test-rth_diary_qa.R` runs both steps on them,
including a second week. From source: `source("dev/load_dev.R")`.
