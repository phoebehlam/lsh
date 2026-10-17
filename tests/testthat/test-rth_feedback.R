# Tests for the Rise to Health feedback report (lshlab).
# When run from source: source("dev/load_dev.R") first. Inside lshlab, testthat loads the package.
if (!exists("rth_settings")) source(file.path("..", "..", "dev", "load_dev.R"))
library(testthat)

test_that("multi-select blocks: NA if never answered, 0 if 'none' ticked", {
  df <- data.frame(a = c(1, NA, NA), b = c(1, NA, NA), none = c(NA, 1, NA))
  expect_equal(.block_sum(df, c("a", "b"), c("a", "b", "none")), c(2, 0, NA))
})

test_that("POMP runs 0 to 100 and tolerates one skipped item", {
  df <- data.frame(x = c(1, 5, 3), y = c(1, 5, NA))
  expect_equal(.pomp_mean(df, c("x", "y"), 1, 5), c(0, 100, 50))
})

test_that("clock times average across midnight", {
  t <- as.POSIXct(c("2026-01-01 23:30:00", "2026-01-02 00:30:00"), tz = "UTC")
  expect_equal(fmt_clock(mean(.clock_min(t))), "12:00 AM")
})

test_that("within-person slope is scaled to the effect unit and honours the minimum", {
  x <- 1:10; y <- 2 * x
  expect_equal(within_slope(x, y, unit = 25, same_band = 1)$est, 50)
  expect_equal(within_slope(1:6, 1:6, 1, 1)$status, "too_few_days")
  expect_equal(within_slope(rep(1:2, 5), 1:10, 1, 1)$status, "no_variation")
  expect_equal(within_slope(1:10, rep(5, 10) + (1:10) * .01, 1, 1)$status, "about_same")
})

test_that("pilot actigraphy export: night date and columns map correctly", {
  pilot <- Sys.getenv("RTH_PILOT_CSV")
  skip_if(pilot == "" || !file.exists(pilot), "set RTH_PILOT_CSV to the pilot sleep file")
  a <- read_actigraphy(pilot)
  expect_equal(a$night, as.Date("2026-02-09"))   # in bed 12:41 AM -> previous evening
  expect_equal(a$tst, 445); expect_equal(a$tib, 528)
  expect_equal(round(a$se, 2), 84.28)           # time-in-bed efficiency ("Sleep Percent")
  expect_equal(a$sol, 15); expect_equal(a$awakenings, 11)
})

test_that("simulated participant: report numbers match a direct calculation", {
  skip_if(!file.exists(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv")), "run dev/simulate_data.R first")
  nd <- read_night_diary(file.path(.rth_home(), "example_data", "sim_night_diary.sav"), "1234")
  md <- read_morning_diary(file.path(.rth_home(), "example_data", "sim_morning_diary.sav"), "1234")
  ac <- read_actigraphy(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv"))
  rep <- compute_report("1234", 16, nd, md, ac)

  raw <- read.csv(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv"), check.names = FALSE)
  off_night <- md$night[md$watch_worn == 0]
  keep <- as.Date(raw$`Start Date`, "%m/%d/%Y") != off_night
  expect_equal(rep$n_nights, sum(keep))
  expect_equal(rep$sleep$mean[rep$sleep$var == "tst"], mean(raw$`Sleep Time (mins)`[keep]))
  expect_equal(nrow(nd), 15)                 # one diary per day, morning entries re-dated
  expect_false(any(duplicated(nd$date)))

  # Hand-fit one slope: stressful events -> that night's sleep time, on the
  # values after unusual ones are pulled in (median +/- 3 MAD over all nights)
  cd <- rep$days
  cd$tst <- winsorize_mad(cd$tst, 3)
  dd <- cd[!is.na(cd$stress_n) & !is.na(cd$tst), ]
  b <- coef(lm(tst ~ stress_n, data = dd))[["stress_n"]]
  got <- rep$sleep_links$est[rep$sleep_links$predictor == "stress_n" & rep$sleep_links$outcome == "tst"]
  expect_equal(got, b)
})

test_that("resource trigger needs both the level and the minimum days", {
  s <- rth_settings()
  expect_true(resource_trigger(data.frame(neg_pomp = c(rep(70, 4), rep(10, 3))), s)$triggered)
  expect_false(resource_trigger(data.frame(neg_pomp = rep(70, 6)), s)$triggered)
  expect_false(resource_trigger(data.frame(neg_pomp = c(rep(70, 3), rep(10, 4))), s)$triggered)
})


test_that("pairing table flags watch-off nights and late diaries are re-dated", {
  skip_if(!file.exists(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv")), "simulated data not available")
  nd <- read_night_diary(file.path(.rth_home(), "example_data", "sim_night_diary.sav"), "1234")
  md <- read_morning_diary(file.path(.rth_home(), "example_data", "sim_morning_diary.sav"), "1234")
  ac <- read_actigraphy(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv"))
  pt <- pairing_table(build_days(nd, md, ac))
  expect_true(any(pt$check == "watch reported off that night"))
  late <- pt[!is.na(pt$night_diary_started) & substr(pt$night_diary_started, 12, 13) < "12", ]
  expect_true(all(as.Date(substr(late$night_diary_started, 1, 10)) - late$date == 1))
})

test_that("sleep dates re-saved by Excel (m/d/yy, AM/PM, ISO) are read correctly", {
  f <- file.path(.rth_home(), "example_data", "sim_sleep_1234.csv")
  ref <- read_actigraphy(f)
  raw <- readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c"), name_repair = "minimal")
  dcols <- grep("Date$", names(raw)); tcols <- grep("Time$", names(raw))
  tcols <- tcols[!grepl("Total Time|Sleep Time|Wake Time", names(raw)[tcols])]
  reformat <- function(fd, ft) {
    x <- raw
    for (j in dcols) x[[j]] <- fd(as.Date(x[[j]], "%m/%d/%Y"))
    for (j in tcols) x[[j]] <- ft(x[[j]])
    tmp <- tempfile(fileext = ".csv"); readr::write_csv(x, tmp, na = ""); tmp
  }
  short <- function(d) ifelse(is.na(d), NA, sprintf("%d/%d/%s", as.integer(format(d, "%m")), as.integer(format(d, "%d")), format(d, "%y")))
  ampm  <- function(t) ifelse(is.na(t), NA, format(as.POSIXct(paste("2000-01-01", t), tz = "UTC"), "%I:%M:%S %p"))
  for (tmp in c(reformat(short, identity), reformat(short, ampm), reformat(function(d) format(d), identity))) {
    got <- read_actigraphy(tmp)
    expect_equal(got$night, ref$night)
    expect_equal(got$bed, ref$bed)
    expect_equal(got$tst, ref$tst)
  }
})

test_that("a sleep file from the wrong participant stops with a staff message", {
  ex <- file.path(.rth_home(), "example_data")
  out <- file.path(tempdir(), "rth_wrongfile_test")
  nd <- file.path(ex, "sim_night_diary.sav"); md <- file.path(ex, "sim_morning_diary.sav")
  # Another teen's file (ID inside the file)
  expect_error(render_rth_feedback("1234", 16, nd, md, file.path(ex, "sim_sleep_5678.csv"), out_dir = out),
               "belongs to participant 5678")
  # Another teen's file (ID in the file name only)
  tmp_dir <- file.path(tempdir(), "rth_names"); dir.create(tmp_dir, showWarnings = FALSE)
  x <- readr::read_csv(file.path(ex, "sim_sleep_1234.csv"), show_col_types = FALSE, name_repair = "minimal",
                       col_types = readr::cols(.default = "c"))
  x[["File Name"]] <- NULL
  readr::write_csv(x, file.path(tmp_dir, "1171_coded_scored_sleep_consensus.csv"), na = "")
  expect_error(render_rth_feedback("1234", 16, nd, md, file.path(tmp_dir, "1171_coded_scored_sleep_consensus.csv"),
                                   out_dir = out), "belongs to participant 1171")
  # Right ID, but no dates in common with the diaries
  tmp <- tempfile(fileext = ".csv")
  writeLines(gsub("/2026", "/2025", readLines(file.path(ex, "sim_sleep_1234.csv"))), tmp)
  expect_error(render_rth_feedback("1234", 16, nd, md, tmp, out_dir = out), "no matching days")
  # Correct file renders, with no warning
  expect_no_warning(render_rth_feedback("1234", 16, nd, md, file.path(ex, "sim_sleep_1234.csv"), out_dir = out))
  expect_equal(.lead_id(c("1171_coded.csv", "/a/b/0042-x.csv", "camilletest.csv", "12345_x.csv")),
               c("1171", "0042", NA, NA))
})

test_that("unusual values are pulled in (median +/- 3 MAD) before links, not in charts", {
  expect_equal(winsorize_mad(c(1, 2, 3, 4, 100)), c(1, 2, 3, 4, 3 + 3 * stats::mad(c(1, 2, 3, 4, 100))))
  expect_equal(winsorize_mad(c(0, 0, 0, 0, 40)), c(0, 0, 0, 0, 40))   # MAD 0: left alone
  expect_equal(winsorize_mad(c(5, NA, 6, 7)), c(5, NA, 6, 7))
  expect_equal(winsorize_mad(c(1, 2, 3, 4, 100), Inf), c(1, 2, 3, 4, 100))

  ex <- file.path(.rth_home(), "example_data")
  nd <- read_night_diary(file.path(ex, "sim_night_diary.sav"), "1234")
  md <- read_morning_diary(file.path(ex, "sim_morning_diary.sav"), "1234")
  ac <- read_actigraphy(file.path(ex, "sim_sleep_1234.csv"))
  # One implausible night on the day with the highest negative mood
  d0 <- build_days(nd, md, ac)
  hi <- d0$date[which.max(ifelse(is.na(d0$tst), NA, d0$neg_pomp))]
  ac$tst[ac$night == hi] <- 900
  r_on  <- compute_report("1234", 16, nd, md, ac)
  st <- rth_settings(); st$clip_mad_k <- Inf
  r_off <- compute_report("1234", 16, nd, md, ac, st)
  pick <- function(r) r$sleep_links$est[r$sleep_links$predictor == "neg_pomp" & r$sleep_links$outcome == "tst"]
  expect_lt(abs(pick(r_on)), abs(pick(r_off)))
  expect_true(any(r_on$clipped$var == "tst" & r_on$clipped$date %in% hi))
  expect_equal(max(r_on$days$tst, na.rm = TRUE), 900)          # charts keep the real value
  pt <- pairing_table(r_on$days, r_on$clipped)
  expect_match(pt$unusual_value_pulled_in[which(pt$date == hi)], "^tst 900->")
  expect_equal(nrow(r_off$clipped), 0)
})

test_that("overview cards show the standout link", {
  s <- rth_settings()
  l <- tibble::tibble(predictor = c("stress_n", "pos_n"), outcome = c("tst", "tst"), n = 14,
                      est = c(-24, 12), status = c("shown", "shown"))
  h <- link_highlight(l, s, "sleep")
  expect_equal(h$value, "&minus;24 min")
  expect_equal(h$sub, "sleep after days with 1 more unpleasant event")
  l2 <- rbind(l, tibble::tibble(predictor = "pos_pomp", outcome = "awakenings", n = 14, est = -2.5, status = "shown"))
  expect_equal(link_highlight(l2, s, "sleep")$value, "&minus;24 min")     # events first
  expect_equal(link_highlight(l2[3, ], s, "sleep")$sub, "wake-ups after days with 25-pt higher positive mood")
  l$status <- "about_same"
  expect_equal(link_highlight(l, s, "sleep")$value, "About the same")
  l$status <- "too_few_days"
  expect_equal(link_highlight(l, s, "mood")$value, "&mdash;")
  m <- tibble::tibble(predictor = "help_n", outcome = "pos_pomp", n = 14, est = 7.4, status = "shown")
  expect_equal(link_highlight(m, s, "mood")$sub, "positive mood on days with more helping")
})

test_that("very large differences are shown capped (e.g. + >60 min), efficiency in %", {
  set.seed(4)
  x <- rep(0:2, length.out = 14)
  d <- tibble::tibble(date = Sys.Date() + 0:13, stress_n = x, pos_n = 1, help_n = 1,
                      neg_pomp = 10 + stats::rnorm(14, 0, .5),
                      pos_pomp = 50, tst = 420 + x * 120 + stats::rnorm(14, 0, 5),
                      se = 88, sol = 15, awakenings = 2)
  s <- rth_settings()
  lt <- link_table(d, "stress_n", "tst", s)
  expect_true(lt$capped)
  expect_equal(lt$est_show, 60)
  expect_gt(lt$est, 60)                                # raw estimate kept for staff
  expect_equal(link_value("tst", 60, TRUE), "+ &gt;60 min")
  expect_equal(link_value("tst", -60, TRUE), "&minus; &gt;60 min")
  expect_equal(link_value("se", -4.4), "&minus;4%")
  expect_equal(link_value("se", 30, TRUE), "+ &gt;30%")
  expect_equal(link_value("awakenings", 10, TRUE), "+ &gt;10 times")
  expect_equal(link_value("tst", 24), "+24 min")
  h <- link_highlight(lt, s, "sleep")
  expect_equal(h$value, "+ &gt;60 min")
  expect_equal(h$sub, "sleep after days with 1 more unpleasant event")
  s2 <- s; s2$link_cap <- NULL                          # cap can be turned off
  expect_false(link_table(d, "stress_n", "tst", s2)$capped)
})

test_that("'Most of them involved' only appears with 5 or more unpleasant events", {
  expect_match(sources_html(c(family = 3, friends = 1, school = 1, unfair = 0)), "Most of them involved")
  expect_false(grepl("Most of them", sources_html(c(family = 2, friends = 1, school = 1, unfair = 0))))
  expect_match(sources_html(c(family = 1, friends = 0, school = 0, unfair = 0)), "1 unpleasant event<")
})
