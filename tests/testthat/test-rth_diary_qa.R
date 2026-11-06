# Tests for the weekly diary QA (simulated data only)

qa_ex <- function(f) file.path(.qa_home(), "example_data", f)
qa_files <- function() list(caregiver_night = qa_ex("sim_caregiver_night.sav"),
                            adolescent_night = qa_ex("sim_adolescent_night.sav"),
                            adolescent_morning = qa_ex("sim_adolescent_morning.sav"))
qa_run <- function(out, files = qa_files(), ...)
  suppressMessages(rth_diary_qa(files$caregiver_night, files$adolescent_night, files$adolescent_morning,
                                out_dir = out, ...))
qa_props <- function(path, tab) {
  p <- openxlsx::read.xlsx(path, tab, detectDates = FALSE)
  for (v in c("diary_date", "proposed_should_date", "final_should_date")) p[[v]] <- .qa_xl_date(p[[v]])
  p
}
# Fill in decisions in the proposals workbook: rows = proposal row numbers
qa_edit <- function(path, tab, rows, action, date = NULL) {
  wb <- openxlsx::loadWorkbook(path)
  hd <- names(openxlsx::read.xlsx(path, tab))
  for (k in seq_along(rows)) {
    openxlsx::writeData(wb, tab, action[k], startCol = match("final_action", hd), startRow = rows[k] + 1, colNames = FALSE)
    if (!is.null(date)) openxlsx::writeData(wb, tab, if (is.na(date[k])) "" else date[k],
                                            startCol = match("final_should_date", hd), startRow = rows[k] + 1, colNames = FALSE)
  }
  suppressWarnings(openxlsx::saveWorkbook(wb, path, overwrite = TRUE))
}
at <- function(x) as.POSIXct(x, tz = "UTC")
s <- rth_diary_qa_settings()

test_that("diary dates: night before 12:00 -> previous night; morning before 04:00 -> previous day", {
  t <- at(c("2026-07-07 23:59:00", "2026-07-08 03:59:00", "2026-07-08 11:59:59", "2026-07-08 12:00:00"))
  expect_equal(.qa_diary_date(t, "night", s), as.Date(c("2026-07-07", "2026-07-07", "2026-07-07", "2026-07-08")))
  expect_equal(.qa_diary_date(t, "morning", s), as.Date(c("2026-07-07", "2026-07-07", "2026-07-08", "2026-07-08")))
})

test_that("compliance follows the agreed cut-offs", {
  sd <- as.Date("2026-07-07")
  t <- at(c("2026-07-07 21:00:00", "2026-07-08 00:30:00", "2026-07-08 03:59:00", "2026-07-08 04:00:00",
            "2026-07-08 11:59:00", "2026-07-08 12:00:00", "2026-07-09 20:00:00"))
  expect_equal(.qa_compliance(t, rep(sd, 7), "night", s), c(1L, 1L, 1L, 2L, 2L, 3L, 3L))
  m <- at(c("2026-07-07 07:00:00", "2026-07-07 13:59:00", "2026-07-07 14:00:00", "2026-07-07 23:59:00",
            "2026-07-08 01:00:00"))
  expect_equal(.qa_compliance(m, rep(sd, 5), "morning", s), c(1L, 1L, 2L, 2L, 3L))
  expect_true(is.na(.qa_compliance(m[1], as.Date(NA), "morning", s)))
})

test_that("shared caregiver IDs are tidied but kept together for review", {
  expect_equal(.qa_person(c("1212, 1172", "1212,1172 ", "1062.0", "", NA)),
               c("1212, 1172", "1212, 1172", "1062", NA, NA))
})

test_that("step 1 tags each simulated situation with the agreed rule", {
  out <- tempfile("qa1_"); on.exit(unlink(out, recursive = TRUE))
  rep <- qa_run(out, run_date = "2026-11-06")
  expect_true(file.exists(rep))
  pp <- file.path(out, "rth_diary_qa_proposals_2026-11-06.xlsx")
  expect_true(file.exists(pp))
  cg <- qa_props(pp, "caregiver_night")
  get <- function(id, t) cg[cg$id == id & cg$startdate == t, ]
  # R99 first: 0-second entry
  expect_equal(cg$id[1:2], c("9072", "9072"))
  expect_true(all(cg$rule[1:2] == "R99") && all(is.na(cg$proposed_action[1:2])))
  # 9012: R1 + R2 + R3
  expect_equal(get("9012", "2026-07-07 16:01:41")$proposed_should_date, as.Date("2026-07-06"))
  expect_equal(get("9012", "2026-07-08 08:20:16")[, c("rule", "proposed_action")], data.frame(rule = "R2", proposed_action = "keep"), ignore_attr = TRUE)
  expect_equal(get("9012", "2026-07-08 08:44:03")$proposed_action, "drop")
  r3 <- get("9012", "2026-07-18 14:36:03")
  expect_equal(c(r3$rule, r3$proposed_action), c("R3", "reassign"))
  expect_equal(r3$proposed_should_date, as.Date("2026-07-17"))
  # 9032: R1 can move an entry before the first diary date
  expect_equal(get("9032", "2026-10-05 12:48:28")$proposed_should_date, as.Date("2026-10-04"))
  # one caregiver, two teens: one set of rows
  expect_equal(sum(cg$id == "9042, 9052"), 2)
  expect_equal(get("9042, 9052", "2026-07-13 21:29:37")$proposed_action, "drop")
  # R0: unfinished restarts dropped
  expect_equal(cg$proposed_action[cg$id == "9062" & cg$finished == 0], c("drop", "drop", "drop"))
  # untouched caregiver isn't listed
  expect_false("9082" %in% cg$id)
  # morning: 01:31 entry counts for the previous day and isn't tagged; R1 + R2
  am <- qa_props(pp, "adolescent_morning")
  expect_equal(am$rule, c("R2", "R2", "R1", "R1"))
  expect_equal(am$proposed_should_date[am$startdate == "2026-10-10 06:58:14"], as.Date("2026-10-09"))
  an <- qa_props(pp, "adolescent_night")
  expect_equal(sort(unique(an$rule)), c("R0", "R1"))
  # report lists previews, rows without an ID, and a caregiver ID in a teen diary
  html <- paste(readLines(rep, warn = FALSE), collapse = "\n")
  expect_match(html, "1 preview or test response is left out")
  expect_match(html, "without an id")
  expect_match(html, "9042")
  expect_match(html, "Please open the proposals excel sheet")
})

test_that("step 2 stops on blanks, clashes and bad dates, and writes nothing", {
  out <- tempfile("qa2_"); on.exit(unlink(out, recursive = TRUE))
  qa_run(out, run_date = "2026-11-06")
  pp <- file.path(out, "rth_diary_qa_proposals_2026-11-06.xlsx")
  expect_error(qa_run(out, proposals = pp, run_date = "2026-11-06"), "final_action is empty for row 1, row 2")
  qa_edit(pp, "caregiver_night", 1:2, c("keep", "drop"))
  # keeping 08:44 on 07/08 clashes with the entry already there
  qa_edit(pp, "caregiver_night", 5, "reassign", "2026-07-08")
  expect_error(qa_run(out, proposals = pp, run_date = "2026-11-06"), "puts two entries on the same date[^$]*9012: 2 entries for 07/08")
  qa_edit(pp, "caregiver_night", 5, "reassign", "next tuesday")
  expect_error(qa_run(out, proposals = pp, run_date = "2026-11-06"), "can't read the date in row 5")
  expect_false(file.exists(file.path(out, "rth_diary_qa_decisions_log.xlsx")))
  expect_length(list.files(out, pattern = "\\.sav$"), 0)
})

test_that("step 2 writes the log and clean data; week 2 only proposes new entries", {
  out <- tempfile("qa3_"); on.exit(unlink(out, recursive = TRUE))
  qa_run(out, run_date = "2026-11-06")
  pp <- file.path(out, "rth_diary_qa_proposals_2026-11-06.xlsx")
  qa_edit(pp, "caregiver_night", 1:2, c("keep", "drop"))
  qa_edit(pp, "caregiver_night", 6, "keep", "7/18/2026")      # override R3: keep on 07/18 (US date typed)
  clean <- qa_run(out, proposals = pp, run_date = "2026-11-06")
  logp <- file.path(out, "rth_diary_qa_decisions_log.xlsx")
  expect_true(file.exists(logp))
  lg <- .qa_read_book(logp, .qa_log_cols)
  expect_equal(nrow(lg$caregiver_night), 17)
  expect_equal(lg$caregiver_night$source[lg$caregiver_night$startdate == "2026-07-18 14:36:03"], "override")
  expect_equal(sum(lg$caregiver_night$source == "override"), 3)            # 2 needed a decision + 1 changed
  for (tp in names(clean)) expect_true(file.exists(file.path(out, sprintf("rth_%s_qa_2026-11-06.sav", tp))))

  cg <- clean$caregiver_night
  expect_true(all(c("id", "diary_date", "should_date", "should_day", "compliance", "exclude", "qa_rule") %in% names(cg)))
  # shared caregiver: every entry once per teen
  raw <- haven::read_sav(qa_ex("sim_caregiver_night.sav"))
  n_shared <- sum(raw$ExternalReference %in% "9042, 9052")
  expect_equal(sum(cg$id %in% "9042"), n_shared)
  expect_equal(sum(cg$id %in% "9052"), n_shared)
  # one kept entry per id and date
  k <- cg[cg$exclude == 0 & !is.na(cg$id), ]
  expect_false(anyDuplicated(paste(k$id, k$should_date)) > 0)
  r1 <- cg[format(cg$startdate, tz = "UTC") == "2026-10-05 12:48:28", ]
  expect_equal(r1$should_date, as.Date("2026-10-04"))
  expect_equal(r1$should_day, 1)
  expect_equal(as.numeric(r1$compliance), 3)
  ov <- cg[format(cg$startdate, tz = "UTC") == "2026-07-18 14:36:03", ]
  expect_equal(c(as.character(ov$should_date), ov$qa_source), c("2026-07-18", "override"))
  # dropped: 08:44:03, 21:29:37 (once per teen), 3 unfinished restarts, 20:00:00, and the row without an ID
  expect_equal(sum(cg$exclude == 1), 8)
  expect_true(all(is.na(cg$compliance[cg$exclude == 1])))

  # Week 2: a new export with one more entry; earlier decisions are not proposed again
  raw <- haven::read_sav(qa_ex("sim_caregiver_night.sav"))
  new <- raw[raw$ExternalReference %in% "9082" & as.numeric(raw$Status) == 0, ][1, ]
  new$StartDate <- at("2026-10-31 23:30:00"); new$EndDate <- at("2026-10-31 23:37:00")
  new$ResponseId <- "R_week2newentry"
  wk2 <- file.path(out, "cg_week2.sav")
  haven::write_sav(rbind(raw, new), wk2)
  f2 <- qa_files(); f2$caregiver_night <- wk2
  # last week's proposals don't cover the new entry
  expect_error(qa_run(out, files = f2, proposals = pp, run_date = "2026-11-13"), "not in the proposals file")
  qa_run(out, files = f2, run_date = "2026-11-13")
  p2 <- file.path(out, "rth_diary_qa_proposals_2026-11-13.xlsx")
  cg2 <- qa_props(p2, "caregiver_night")
  expect_equal(cg2$id, c("9082", "9082"))
  expect_equal(cg2$proposed_action, c("keep", "drop"))
  expect_equal(nrow(qa_props(p2, "adolescent_morning")), 0)
  clean2 <- qa_run(out, files = f2, proposals = p2, run_date = "2026-11-13")
  lg2 <- .qa_read_book(logp, .qa_log_cols)
  expect_equal(nrow(lg2$caregiver_night), 19)
  expect_equal(sum(clean2$caregiver_night$exclude == 1), 9)
})

test_that("step 1 with no diaries or a missing file stops with a clear message", {
  expect_error(rth_diary_qa(), "at least one diary export")
  expect_error(rth_diary_qa(caregiver_night = "nope.sav"), "File not found")
})
