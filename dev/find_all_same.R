# Build a simulated participant whose day-to-night links are all "about the same":
# shuffle participant 1234's nights so sleep no longer tracks the diaries.
source("dev/load_dev.R")
h <- .rth_home(); ex <- file.path(h, "example_data")
raw <- read.csv(file.path(ex, "sim_sleep_1234.csv"), check.names = FALSE)
nd <- read_night_diary(file.path(ex, "sim_night_diary.sav"), "1234")
md <- read_morning_diary(file.path(ex, "sim_morning_diary.sav"), "1234")
cols <- c("Total Time (mins)", "Sleep Time (mins)", "Wake Time (mins)", "Sleep Percent", "Wake Percent",
          "Sleep Ratio", "Time Onset to Offset (mins)", "Sleep Onset to Offset (mins)",
          "Wake Onset to Offset (mins)", "Sleep Percent Onset to Offset", "Wake Percent Onset to Offset",
          "Sleep Ratio Onset to Offset", "Number of Awakenings", "Mean Awakening (mins)", "Sleep Onset Latency (mins)")
for (seed in 1:500) {
  set.seed(seed); x <- raw; x[cols] <- raw[sample(nrow(raw)), cols]
  f <- tempfile(fileext = ".csv"); write.csv(x, f, row.names = FALSE)
  r <- compute_report("1234", 16, nd, md, read_actigraphy(f))
  if (all(r$sleep_links$status == "about_same")) {
    write.csv(x, "dev/sim_sleep_1234_allsame.csv", row.names = FALSE)
    cat("seed", seed, "\n"); break
  }
}
