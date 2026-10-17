# Simulated test data shaped exactly like the Qualtrics shells and the
# Motionlogger export. No real participant data is used.
#
# Usage: Rscript dev/simulate_data.R <night_shell.sav> <morning_shell.sav> <out_dir>
# Writes: sim_night_diary.sav, sim_morning_diary.sav, sim_sleep_<id>.csv

suppressPackageStartupMessages({ library(haven); library(dplyr) })

args <- commandArgs(trailingOnly = TRUE)
night_shell   <- read_sav(args[1])
morning_shell <- read_sav(args[2])
out_dir <- args[3]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(20261016)

# Coerce a list of simulated columns into the shell's exact structure.
conform <- function(shell, sim) {
  n <- nrow(sim)
  out <- lapply(names(shell), function(col) {
    proto <- shell[[col]]
    x <- if (col %in% names(sim)) sim[[col]] else rep(NA, n)
    if (inherits(proto, "haven_labelled")) {
      labelled(as.numeric(x), labels = attr(proto, "labels"), label = attr(proto, "label"))
    } else if (inherits(proto, "POSIXct")) {
      structure(as.POSIXct(x, tz = "UTC"), label = attr(proto, "label"))
    } else if (is.character(proto)) {
      structure(as.character(x), label = attr(proto, "label"))
    } else {
      structure(as.numeric(x), label = attr(proto, "label"))
    }
  })
  names(out) <- names(shell)
  tibble::as_tibble(out)
}

tick <- function(n_items, which_items) { v <- rep(NA_real_, n_items); v[which_items] <- 1; v }

simulate_person <- function(id, start_date, n_days, skip = integer()) {
  night_rows <- list(); morning_rows <- list(); sleep_rows <- list()
  for (d in seq_len(n_days)) {
    date <- start_date + d - 1
    stress_lat <- rnorm(1)
    n_neg <- rpois(1, exp(0.4 + 0.6 * stress_lat))
    n_pos <- rpois(1, 2.2)
    n_help <- rpois(1, 1.5)
    school <- as.numeric(format(date, "%u")) <= 5

    if (!(d %in% skip)) {
      # Split negative events across family / peer / school / unfair treatment.
      dom <- sample(c("fam", "peer", "school", "dis"), n_neg, replace = TRUE,
                    prob = c(.35, .3, if (school) .3 else 0, .05))
      fam_neg  <- min(sum(dom == "fam"), 9);  peer_neg <- min(sum(dom == "peer"), 7)
      sch_neg  <- min(sum(dom == "school"), 5); dis_n <- min(sum(dom == "dis"), 6)
      fam_pos  <- min(rbinom(1, n_pos, .5), 3); peer_pos <- min(n_pos - fam_pos, 3)

      fam  <- tick(13, c(sample(1:3, fam_pos), sample(4:12, fam_neg)))
      if (fam_pos + fam_neg == 0) fam[13] <- 1
      peer <- tick(11, c(sample(1:3, peer_pos), sample(4:10, peer_neg)))
      if (peer_pos + peer_neg == 0) peer[11] <- 1
      sch  <- if (school) { s <- tick(7, sample(1:6, sch_neg)); if (sch_neg == 0) s[7] <- 1; s } else rep(NA, 7)
      dis  <- tick(8, sample(1:6, dis_n)); if (dis_n == 0) dis[8] <- 1
      hf_n <- min(rbinom(1, n_help, .5), 3); hp_n <- min(n_help - hf_n, 8)
      hfam <- tick(4, sample(1:3, hf_n)); if (hf_n == 0) hfam[4] <- 1
      hpeer <- tick(9, sample(1:8, hp_n)); if (hp_n == 0) hpeer[9] <- 1

      neg_lvl <- 1.6 + 0.35 * n_neg + rnorm(1, 0, .3)
      pos_lvl <- 3.2 + 0.25 * n_pos - 0.15 * n_neg + rnorm(1, 0, .3)
      lik <- function(m) pmin(pmax(round(rnorm(1, m, .6)), 1), 5)

      # Most diaries start in the evening; a few are finished the next morning
      # (before noon, so they count for the previous day).
      start_time <- if (runif(1) < .15) as.POSIXct(date + 1, tz = "UTC") + 9.5 * 3600
                    else as.POSIXct(date, tz = "UTC") + (20.5 + runif(1, 0, 2)) * 3600

      row <- list(StartDate = start_time, EndDate = start_time + 600,
                  Status = 0, Progress = 100, Finished = 1,
                  RecordedDate = start_time + 600,
                  ResponseId = paste0("R_sim", id, d), ExternalReference = id,
                  DistributionChannel = "email", UserLanguage = "EN",
                  go_school = as.numeric(school))
      for (i in 1:13) row[[paste0("events_family_", i)]] <- fam[i]
      for (i in 1:11) row[[paste0("events_peers_", i)]] <- peer[i]
      for (i in 1:7)  row[[paste0("events_school_", i)]] <- sch[i]
      for (i in 1:8)  row[[paste0("events_discrim_", i)]] <- dis[i]
      for (i in 1:4)  row[[paste0("help_family_", i)]] <- hfam[i]
      for (i in 1:9)  row[[paste0("help_peers_", i)]] <- hpeer[i]
      for (f in c("lonely","angry","embarrass","judged","rejected","nervous","sad","worried"))
        row[[paste0("feeling_", f)]] <- lik(neg_lvl)
      for (f in c("calm","excited","joyful","happy","close","understood"))
        row[[paste0("feeling_", f)]] <- lik(pos_lvl)
      row$attn_1 <- lik(3); row$attn_2 <- lik(3)
      row$ssc_tempt <- lik(3.5); row$ssc_getgoing <- lik(3.5); row$ssc_stickwith <- lik(3.5)
      row$food_oth <- 0
      night_rows[[length(night_rows) + 1]] <- row
    }

    # Sleep that night: shorter and more broken after stressful days.
    bed  <- as.POSIXct(date, tz = "UTC") + (22.75 + rnorm(1, 0, .6) + 0.1 * n_neg) * 3600
    sol  <- max(0, round(rnorm(1, 12 + 3 * n_neg, 5)))
    tst  <- round(rnorm(1, 500 - 14 * n_neg, 25))
    waso <- max(5, round(rnorm(1, 30 + 4 * n_neg, 10)))
    awak <- max(1, round(waso / rnorm(1, 3.5, .5)))
    onset <- bed + sol * 60
    offset <- onset + (tst + waso) * 60
    end <- offset + round(runif(1, 2, 15)) * 60
    total <- round(as.numeric(difftime(end, bed, units = "mins")))
    o2o <- tst + waso
    worn <- if (d == 6) 0 else 1   # one night the watch was off
    f_d <- function(x) format(x, "%m/%d/%Y", tz = "UTC")
    f_t <- function(x) format(x, "%H:%M:%S", tz = "UTC")
    sleep_rows[[length(sleep_rows) + 1]] <- tibble::tibble(
      `File Name` = sprintf("%s_%s.ami", id, format(start_date, "%m-%d-%Y")),
      `ID String` = "AMI File", `Sleep Channel` = "zcmsadeh",
      `Interval Channel` = "inbed", Interval = d,
      `Start Date` = f_d(bed), `Start Time` = f_t(bed),
      `End Date` = f_d(end), `End Time` = f_t(end),
      `Sample Rate (secs)` = 60, `Total Time (mins)` = total,
      `Sleep Time (mins)` = tst, `Wake Time (mins)` = total - tst,
      `Sleep Percent` = round(tst / total * 100, 4),
      `Wake Percent` = round((total - tst) / total * 100, 4),
      `Sleep Ratio` = round(tst / (total - tst), 6),
      `First Onset Date` = f_d(onset), `First Onset Time` = f_t(onset),
      `Last Offset Date` = f_d(offset), `Last Offset Time` = f_t(offset),
      `Time Onset to Offset (mins)` = o2o, `Sleep Onset to Offset (mins)` = tst,
      `Wake Onset to Offset (mins)` = waso,
      `Sleep Percent Onset to Offset` = round(tst / o2o * 100, 5),
      `Wake Percent Onset to Offset` = round(waso / o2o * 100, 6),
      `Sleep Ratio Onset to Offset` = round(tst / waso, 5),
      `Number of Awakenings` = awak, `Mean Awakening (mins)` = round(waso / awak),
      `Sleep Onset Latency (mins)` = sol)

    # Morning diary the next day, with typical self-report error.
    if (!(d %in% skip)) {
      sr_bed  <- bed + round(rnorm(1, -10, 12)) * 60
      sr_wake <- end + round(rnorm(1, 5, 10)) * 60
      h12 <- function(x) { h <- as.numeric(format(x, "%H", tz = "UTC")); ifelse(h %% 12 == 0, 12, h %% 12) }
      ampm <- function(x) ifelse(as.numeric(format(x, "%H", tz = "UTC")) >= 12, 2, 1)
      mt <- as.POSIXct(date + 1, tz = "UTC") + 8 * 3600
      morning_rows[[length(morning_rows) + 1]] <- list(
        StartDate = mt, EndDate = mt + 120, Status = 0, Progress = 100,
        Finished = 1, RecordedDate = mt + 120, ResponseId = paste0("R_simm", id, d),
        ExternalReference = id, DistributionChannel = "email", UserLanguage = "EN",
        bedtime_hr = h12(sr_bed), bedtime_min = as.numeric(format(sr_bed, "%M", tz = "UTC")),
        bedtime_am.pm = ampm(sr_bed), latency = max(0, sol + round(rnorm(1, 5, 6))),
        waketime_hr = h12(sr_wake), waketime_min = as.numeric(format(sr_wake, "%M", tz = "UTC")),
        waketime_am.pm = ampm(sr_wake),
        rested = pmin(pmax(round(rnorm(1, 3.4 - 0.2 * n_neg, .7)), 1), 5),
        quality = pmin(pmax(round(rnorm(1, 3.6 - 0.2 * n_neg, .7)), 1), 5),
        watchwear = worn, remove_watch = rpois(1, 1), complete = "1")
    }
  }
  list(night = bind_rows(lapply(night_rows, tibble::as_tibble)),
       morning = bind_rows(lapply(morning_rows, tibble::as_tibble)),
       sleep = bind_rows(sleep_rows))
}

p1 <- simulate_person("1234", as.Date("2026-09-14"), 15)
p2 <- simulate_person("5678", as.Date("2026-09-21"), 15, skip = c(2:4, 7:12))  # sparse diaries

write_sav(conform(night_shell, bind_rows(p1$night, p2$night)),
          file.path(out_dir, "sim_night_diary.sav"))
write_sav(conform(morning_shell, bind_rows(p1$morning, p2$morning)),
          file.path(out_dir, "sim_morning_diary.sav"))
readr::write_csv(p1$sleep, file.path(out_dir, "sim_sleep_1234.csv"))
readr::write_csv(p2$sleep, file.path(out_dir, "sim_sleep_5678.csv"))
cat("Simulated data written to", out_dir, "\n")
