# Tests for the weekly recruitment report (simulated data only)

rec_ex <- function(f) file.path(.rth_recruit_home(), "example_data", f)
rd <- as.Date("2026-10-31")

test_that("study weeks run Saturday-Friday from Apr 25, 2026", {
  st <- as.Date("2026-04-25")
  expect_equal(rth_study_week(as.Date(c("2026-04-25", "2026-05-01", "2026-05-02", "2026-10-24", "2026-10-30", "2026-10-31")), st),
               c(1L, 1L, 2L, 27L, 27L, 28L))
  expect_equal(.week_label(27, st), "10/24&ndash;10/30")
})

test_that("screener outcomes follow eligi_status, answers matched by label", {
  s <- read_screener(rec_ex("sim_prescreen.sav"))
  raw <- haven::read_sav(rec_ex("sim_prescreen.sav"))
  raw <- raw[as.numeric(raw$Status) %in% c(0, 4, 16), ]
  expect_equal(nrow(s), nrow(raw))                       # previews dropped
  st <- raw$eligi_status
  expect_equal(sum(s$outcome == "Eligible (child 14-17)"), sum(grepl("_eligible_under18$", st)))
  expect_equal(sum(s$outcome == "Needs review: diagnosis reported"), sum(grepl("_unsure_disorder$", st)))
  expect_equal(sum(s$screened_out), sum(raw$Q_TerminateFlag != ""))
  expect_false(any(c("contact_1", "contact_3") %in% names(s)))
  r <- attr(s, "reasons")
  expect_true(all(rowSums(r[s$outcome == "Ineligible", , drop = FALSE]) > 0))
  med <- as.character(haven::as_factor(raw$medication, levels = "labels")) == "Yes"   # "Yes" is coded 0
  expect_true(all(r[med & s$outcome == "Ineligible", "Anti-inflammatory / hormonal medication"]))
})

test_that("race/ethnicity is scored as monoracial / multiracial", {
  race <- matrix(FALSE, 7, 7, dimnames = list(NULL, c("American Indian or Alaska Native", "Asian", "Hispanic/Latino",
    "Native Hawaiian or Other Pacific Islander", "Black or African American", "White", "Other")))
  race[1, "White"] <- TRUE
  race[2, "Black or African American"] <- TRUE
  race[3, c("White", "Black or African American", "Asian")] <- TRUE        # Black first
  race[4, c("White", "Hispanic/Latino", "Asian")] <- TRUE                  # then Hispanic
  race[5, c("White", "Asian")] <- TRUE                                     # then Asian
  race[6, c("White", "American Indian or Alaska Native")] <- TRUE          # another race
  expect_equal(score_race(race), c("Monoracial White", "Monoracial Black", "Multiracial Black",
                                   "Multiracial Hispanic/Latino", "Multiracial Asian",
                                   "Multiracial, another race", "Not answered"))
})

test_that("tracker: RTH IDs give family and role; rows without an ID are dropped", {
  tr <- read_tracker(rec_ex("sim_tracking.xlsx"))
  expect_true(all(grepl("^[0-9]{4}$", tr$rth_id)))
  expect_equal(tr$family, substr(tr$rth_id, 1, 3))
  expect_equal(tr$role, ifelse(substr(tr$rth_id, 4, 4) == "1", "teen", "caregiver"))
  x <- openxlsx::read.xlsx(rec_ex("sim_tracking.xlsx"), check.names = FALSE, sep.names = " ")
  expect_equal(nrow(tr), sum(!is.na(x$`RTH ID`) & x$`RTH ID` != ""))
  fam <- tracker_families(tr, rd)
  expect_equal(nrow(fam), length(unique(tr$family)))
  cg <- tr[tr$role == "caregiver", ]
  expect_equal(fam$consent, cg$consent_date[match(fam$family, cg$family)])      # consent from caregiver row
  tn <- tr[tr$role == "teen", ]
  expect_equal(fam$lab_date, tn$lab_date[match(fam$family, tn$family)])         # lab visit from teen row
})

test_that("tracker dates must use slashes; bad cells are listed", {
  df <- data.frame(`RTH ID` = c("1231", "1232", "1241", "", "1252"), `adolescent or caregiver?` = "x",
                   `consent signed date` = c("10/24/2026", "10/24/26", "-", "garbage", "45950"),
                   `phone interview date` = c("", "2026-10-26", "9/1/26 & 9/3/2026", "", ""),
                   check.names = FALSE)
  e <- tryCatch(read_tracker(df), error = function(e) conditionMessage(e))
  expect_match(e, "write them like 10/24/2026")
  expect_match(e, 'row 3 \\(RTH ID 1232\\): "2026-10-26"')
  expect_match(e, 'row 4 \\(RTH ID 1241\\): "9/1/26 & 9/3/2026"')
  expect_false(grepl("garbage", e))                     # row without an RTH ID is ignored
  df$`phone interview date` <- c("", "10/26/2026", "9/1/2026", "", "")
  tr <- read_tracker(df)
  expect_equal(tr$consent_date, as.Date(c("2026-10-24", "2026-10-24", NA, "2025-10-20")))
  # Google Sheets can hand dates back as Date values: fine too
  df$`consent signed date` <- list(as.Date("2026-10-24"), "10/24/2026", "-", NA, "10/1/2026")
  expect_equal(read_tracker(df)$consent_date[1], as.Date("2026-10-24"))
})

test_that("conversion links screeners to the tracker and never passes 100%", {
  s <- read_screener(rec_ex("sim_prescreen.sav"))
  tr <- read_tracker(rec_ex("sim_tracking.xlsx")); fam <- tracker_families(tr, rd)
  st <- link_steps(s, fam)
  expect_equal(sum(st$consent), sum(!is.na(fam$consent) & fam$screener_id %in% s$screener_id))
  expect_equal(sum(st$lab), sum(fam$lab_done & fam$screener_id %in% s$screener_id))
  sc <- source_conversion(s, st)
  for (v in c("pct_eligible", "pct_consent", "pct_interview", "pct_survey", "pct_lab"))
    expect_true(all(is.na(sc[[v]]) | sc[[v]] <= 100))
  wc <- weekly_conversion(s, st, as.Date("2026-04-25"), 1:27)
  expect_equal(sum(wc$consent), sum(st$consent[!s$screened_out]))
  # the typed ID can also be the Qualtrics Response ID
  f2 <- fam[1, ]; f2$screener_id <- s$response_id[5]
  expect_true(link_steps(s, f2)$consent[5])
})

test_that("tracker checks flag ID problems and follow-ups", {
  s <- read_screener(rec_ex("sim_prescreen.sav"))
  df <- data.frame(`RTH ID` = c("1231", "1232", "1241", "1242", "12345"),
                   `adolescent or caregiver?` = c("adolescent", "adolescent", "adolescent", "caregiver", "caregiver"),
                   `screener ID` = c(s$screener_id[1], "", "999999", "", ""),
                   `consent signed date` = c("", "9/1/2026", "", "9/1/2026", ""),
                   `phone interview date` = c("", "", "", "8/30/2026", ""),
                   `lab visit scheduled date` = "", `lab visit date` = "", check.names = FALSE)
  tr <- read_tracker(df); fam <- tracker_families(tr, rd)
  ck <- tracker_checks(s, tr, fam, link_steps(s, fam), rd)
  n <- stats::setNames(vapply(ck, `[[`, 0L, "n"), vapply(ck, `[[`, "", "label"))
  expect_equal(n[["RTH ID cells that aren't a single 4-digit ID ending in 1 or 2"]], 1L)
  expect_equal(n[["Role column doesn't match the RTH ID's last digit (1 = teen, 2 = caregiver)"]], 1L)
  expect_equal(n[["Screener IDs in the tracker that aren't in the screener export"]], 1L)
  expect_equal(n[["Caregiver consented 14+ days ago, no interview date"]], 2L)       # family 123: 1231 + 1232
  expect_equal(n[["Interview date is before the consent date"]], 2L)                 # family 124
  df$`consent signed date`[1] <- "12/1/2026"
  tr <- read_tracker(df); fam <- tracker_families(tr, rd)
  ck <- tracker_checks(s, tr, fam, link_steps(s, fam), rd)
  n <- stats::setNames(vapply(ck, `[[`, 0L, "n"), vapply(ck, `[[`, "", "label"))
  expect_equal(n[["Consent, interview or caregiver survey date is in the future"]], 0L)  # teen row's date isn't used
  df$`consent signed date`[2] <- "12/1/2026"
  tr <- read_tracker(df); fam <- tracker_families(tr, rd)
  ck <- tracker_checks(s, tr, fam, link_steps(s, fam), rd)
  n <- stats::setNames(vapply(ck, `[[`, 0L, "n"), vapply(ck, `[[`, "", "label"))
  expect_equal(n[["Consent, interview or caregiver survey date is in the future"]], 2L)
})

test_that("interview: highest education of either caregiver, income exact or bracket", {
  ses <- read_interview_ses(rec_ex("sim_cg_interview.sav"))
  raw <- haven::read_sav(rec_ex("sim_cg_interview.sav"))
  expect_equal(nrow(ses), nrow(raw))
  expect_true(all(ses$education %in% .edu_order))
  i <- which(!is.na(raw$ParentSESv1_19))[1]
  r1 <- match(sub(" \\(.*", "", as.character(haven::as_factor(raw$ParentSESv1_7[i]))), .edu_order)
  r2 <- match(sub(" \\(.*", "", as.character(haven::as_factor(raw$ParentSESv1_19[i]))), .edu_order)
  expect_equal(ses$education[i], .edu_order[max(r1, r2)])
  j <- which(raw$ParentSESv1_53 != "")[1]
  expect_equal(ses$income[j], as.numeric(raw$ParentSESv1_53[j]))
})

test_that("full report renders, without contact info; handles an empty screener and tracker", {
  d <- file.path(tempdir(), "rec_full"); unlink(d, recursive = TRUE)
  p <- suppressMessages(render_rth_recruitment(rec_ex("sim_prescreen.sav"), rec_ex("sim_tracking.xlsx"),
                                               rec_ex("sim_cg_interview.sav"), out_dir = d, report_date = rd,
                                               seed_sheet = rec_ex("sim_weekly_sheet.txt")))
  expect_equal(basename(p), "RTH_recruitment_week27_2026-10-30.html")
  h <- paste(readLines(p, warn = FALSE), collapse = " ")
  expect_match(h, "This week \\(Week 27: 10/24&ndash;10/30\\)")
  expect_false(grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", h))   # no email addresses
  for (id in c("checks", "t-weekly", "conv-week", "conv-src", "t-src", "demo-scr", "demo-enr", "daily"))
    expect_match(h, sprintf('id="%s"', id))
  expect_true(regexpr('id="status"', h) < regexpr('id="conv"', h))           # conversion right after outcomes
  expect_true(regexpr('id="conv"', h) < regexpr('id="sources"', h))
  expect_true(regexpr('id="demo-enr"', h) < regexpr('id="daily"', h))         # daily assessments last
  expect_match(h, 'alt="Daily assessment completion"')
  expect_false(grepl('id="t-daily"', h))
  expect_match(h, 'id="t-weekly"><thead><tr>.*?</tr><tr class="total"><td>Total</td>')
  expect_false(grepl("Eligible after review", h))
  # empty screener and empty tracker (e.g. the very first week): no error
  e <- tempfile(fileext = ".sav"); haven::write_sav(haven::read_sav(rec_ex("sim_prescreen.sav"))[0, ], e)
  et <- data.frame(`RTH ID` = character(), check.names = FALSE)
  p0 <- suppressMessages(render_rth_recruitment(e, et, out_dir = file.path(tempdir(), "rec_empty"),
                                                report_date = as.Date("2026-05-09")))
  expect_true(file.exists(p0))
})

rates <- c("Lab manager" = 21.50 * 1.256, "Grad student" = 30000 / 520)

test_that("costs: expenses at face value, staff time at the hourly rate, other people by amount", {
  cst <- read_costs(rec_ex("sim_costs.xlsx"), "costs", rates)
  raw <- openxlsx::read.xlsx(rec_ex("sim_costs.xlsx"), sheet = "costs", check.names = FALSE, sep.names = " ")
  expect_equal(nrow(cst), nrow(raw))
  ex <- cst$type == "Expense"
  expect_equal(cst$cost[ex], cst$amount[ex])
  lm <- cst$type == "Time" & cst$person %in% "Lab manager"
  expect_equal(cst$cost[lm], cst$hours[lm] * 27.004)
  oth <- cst$type == "Time" & cst$person %in% "Other"
  expect_equal(cst$cost[oth], cst$amount[oth])
  expect_true(all(is.na(cst$date[1:9])))                       # before-tracking totals have no date
  expect_error(read_costs(rec_ex("sim_costs.xlsx"), "nope", rates), "No 'nope' tab")
  df <- data.frame(date = c("10/5/2026", "2026-10-06", ""), type = c("Expense", "Time", "Bought"),
                   source = "Facebook", hours = c("", "1,5", ""), amount = c("$1,200.50", "", "3"), check.names = FALSE)
  e <- tryCatch(read_costs(df, "costs", rates), error = function(e) conditionMessage(e))
  expect_match(e, 'row 3: "2026-10-06"'); expect_match(e, '"hours", row 3: "1,5"'); expect_match(e, '"type", row 4: "Bought"')
  df <- df[1, ]; expect_equal(read_costs(df, "costs", rates)$cost, 1200.5)
})

test_that("cost per step: fractional credit, General only in the total, cheapest skips thin sources", {
  s <- read_screener(rec_ex("sim_prescreen.sav"))
  tr <- read_tracker(rec_ex("sim_tracking.xlsx")); fam <- tracker_families(tr, rd); st <- link_steps(s, fam)
  cst <- read_costs(rec_ex("sim_costs.xlsx"), "costs", rates)
  cs <- cost_summary(cst, s, st, 27, as.Date("2026-04-25"))
  src <- attr(s, "sources")[!s$screened_out, , drop = FALSE]
  # every screener with a source hands out exactly 1 in total
  share <- src / pmax(rowSums(src), 1)
  expect_equal(sum(colSums(share)), sum(rowSums(src) > 0))
  ig <- cs$by_src[cs$by_src$source == "Instagram", ]
  expect_equal(ig$cost, 1400 + 350 + 350 + 1.5 * 27.004)
  expect_equal(ig$screeners, sum(share[, "Instagram"]))
  gen <- sum(cst$cost[cst$source == "General"])
  expect_equal(cs$total$cost, sum(cs$by_src$cost) + gen)
  expect_equal(cs$total$screeners, sum(!s$screened_out))
  h <- cost_html(cs, 27, function(k) .week_label(k, as.Date("2026-04-25")))
  # a cell is never both cheapest and too thin
  expect_false(grepl('class="best">[^<]*<span class="cnt">[0-2](\\.[0-9])?</span>', h))
  expect_match(h, 'class="thin"')
  # weekly costs add up to the total
  expect_equal(sum(cs$by_week$cost) + sum(cs$before$cost), cs$total$cost)
  # a source name that isn't a screener choice is flagged and kept out of the table
  cst$source[10] <- "Tik Tok"
  cs2 <- cost_summary(cst, s, st, 27, as.Date("2026-04-25"))
  expect_equal(cs2$flags$unmatched, "Tik Tok")
  expect_equal(cs2$total$cost, cs$total$cost)
})

test_that("report has a Costs section at the end, without printing hourly rates", {
  d <- file.path(tempdir(), "rec_cost"); unlink(d, recursive = TRUE)
  p <- suppressMessages(render_rth_recruitment(rec_ex("sim_prescreen.sav"), rec_ex("sim_tracking.xlsx"), out_dir = d,
                                               report_date = rd, costs = TRUE, costs_sheet = rec_ex("sim_costs.xlsx")))
  h <- paste(readLines(p, warn = FALSE), collapse = " ")
  for (id in c("costs", "t-util", "t-costweek", "cost-hours", "lm-week")) expect_match(h, sprintf('id="%s"', id))
  expect_true(regexpr('id="daily"', h) < regexpr('id="costs"', h))
  expect_false(grepl("27\\.00|57\\.69|21\\.50", h))
  expect_match(h, 'id="t-util"><thead><tr>.*?</tr><tr class="total"><td>All strategies</td>')
  p0 <- suppressMessages(render_rth_recruitment(rec_ex("sim_prescreen.sav"), rec_ex("sim_tracking.xlsx"), out_dir = d, report_date = rd))
  h0 <- paste(readLines(p0, warn = FALSE), collapse = " ")
  expect_false(grepl('id="costs"|href="#costs"', h0))                      # off by default: no section, no link
  expect_error(render_rth_recruitment(rec_ex("sim_prescreen.sav"), rec_ex("sim_tracking.xlsx"), out_dir = d, report_date = rd,
                                      costs = TRUE), "needs costs_sheet")
  expect_error(render_rth_recruitment(rec_ex("sim_prescreen.sav"), rec_ex("sim_tracking.xlsx"), out_dir = d, report_date = rd,
                                      costs = rec_ex("sim_costs.xlsx")), "TRUE or FALSE")
})

test_that("lab manager's week: 37.5-hour bar, recruitment from the costs tab, unlogged time white", {
  s <- read_screener(rec_ex("sim_prescreen.sav"))
  tr <- read_tracker(rec_ex("sim_tracking.xlsx")); st <- link_steps(s, tracker_families(tr, rd))
  ss <- as.Date("2026-04-25")
  cst <- read_costs(rec_ex("sim_costs.xlsx"), "costs", rates)
  cs <- cost_summary(cst, s, st, 27, ss)
  lmh <- read_lm_hours(rec_ex("sim_costs.xlsx"))
  expect_false(any(grepl("recruit", levels(lmh$category))))              # formula columns skipped
  rec <- sum(cst$hours[cst$type == "Time" & cst$person %in% "Lab manager" & rth_study_week(cst$date, ss) %in% 27])
  other <- sum(lmh$hours[rth_study_week(lmh$date, ss) %in% 27])
  h <- lm_week_html(cs, lmh, 27, ss)
  expect_match(h, sprintf("Recruitment: <b>%s h &middot; %.0f%%", .hrs(rec), 100 * rec / 37.5))
  gap <- 37.5 - rec - other
  expect_match(h, sprintf('class="seg empty" style="width:%.3f%%"', 100 * gap / 37.5))
  expect_match(h, sprintf("Not logged: <b>%s h &middot; %.0f%%", .hrs(gap), 100 * gap / 37.5))
  expect_false(grepl("mark375", h))
  # over 37.5: no white part, a line at 37.5
  big <- lmh; big$hours <- big$hours * 3
  h2 <- lm_week_html(cs, big, 27, ss)
  expect_false(grepl("seg empty", h2)); expect_match(h2, "mark375"); expect_match(h2, "h over")
  # no hours tab: recruitment only, with a note
  expect_match(lm_week_html(cs, NULL, 27, ss), "No &ldquo;lab manager hours&rdquo; tab")
  expect_null(read_lm_hours(rec_ex("sim_tracking.xlsx")))
  # the recruitment-by-source visual leaves out the grad student and others
  r <- lm_rec_html(cs, 27)
  expect_match(r, sprintf("Total: %s h", .hrs(rec)))
  expect_false(grepl("RA helped|Posted in groups</span>.*Grad", r))
  bad <- data.frame(date = c("10/26/2026", "10/27/2026"), `participant tracking` = c("2", "two"), check.names = FALSE)
  expect_error(read_lm_hours(bad), '"participant tracking", row 3: "two"')
})
