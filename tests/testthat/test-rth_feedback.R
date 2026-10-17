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

  # Hand-fit one slope: stressful events -> that night's sleep time
  dd <- rep$days[!is.na(rep$days$stress_n) & !is.na(rep$days$tst), ]
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
