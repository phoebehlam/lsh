# Alignment, summaries, within-person slopes, and the resource trigger -----------

#' Settings for the Rise to Health feedback report
#'
#' Every threshold in one place. Modify the returned list and pass it to
#' \code{render_rth_feedback(settings = )}.
#' @export
rth_settings <- function() {
  list(
    min_days       = 7,     # minimum paired days for a slope / correlation
    min_distinct_x = 3,     # predictor must take at least this many values
    pomp_unit      = 25,    # effect unit for 0-100 scales
    count_unit     = 1,     # effect unit for event counts
    clock_anchor_h = 18,    # clock times averaged on a 6 PM - 6 PM day
    # "About the same" bands: differences smaller than these are described as
    # no real difference (descriptive rounding, not a statistical test).
    same_band = c(tst = 10, se = 2, sol = 5, awakenings = 1,
                  neg_pomp = 5, pos_pomp = 5),
    # Resource list trigger: negative mood POMP >= cutoff on >= share of days,
    # with at least min_days completed diaries.
    # When every day-to-night link is "about the same", show this many of the
    # largest ones anyway (0 = text only).
    same_show_top = 3,
    trigger_cutoff = 60,
    trigger_share  = 0.5,
    # Published reference values shown on the "at a glance" bars
    # (National Sleep Foundation sleep quality recommendations, Ohayon 2017).
    ref_sol_max = 30,       # minutes to fall asleep
    ref_se_min  = 85        # % of time in bed asleep (time-in-bed definition)
  )
}

#' @keywords internal
rth_vars <- function() {
  list(
    predictors = tibble::tribble(
      ~var,       ~short,              ~when_more,                                        ~type,
      "stress_n", "Unpleasant events",  "reported one more unpleasant event",              "count",
      "pos_n",    "Positive events",   "reported one more positive event",               "count",
      "help_n",   "Helping others",    "helped someone one more time",                   "count",
      "neg_pomp", "Negative mood",     "rated your negative mood 25 points higher (out of 100)", "pomp",
      "pos_pomp", "Positive mood",     "rated your positive mood 25 points higher (out of 100)", "pomp"
    ),
    sleep_outcomes = tibble::tribble(
      ~var,          ~short,            ~unit,
      "tst",         "Sleep time",      "min",
      "se",          "Sleep efficiency", "points",
      "sol",         "Time to fall asleep", "min",
      "awakenings",  "Awakenings",      "times"
    ),
    mood_outcomes = tibble::tribble(
      ~var,        ~short,          ~unit,
      "neg_pomp",  "Negative mood", "points",
      "pos_pomp",  "Positive mood", "points"
    )
  )
}

# Age-appropriate sleep-duration guidance (National Sleep Foundation).
sleep_guideline <- function(age) {
  if (is.null(age) || is.na(age)) return(NULL)
  if (age < 14) return(list(lo = 9, hi = 11, group = "ages 6-13"))
  if (age < 18) return(list(lo = 8, hi = 10, group = "ages 14-17"))
  list(lo = 7, hi = 9, group = "ages 18-25")
}

# Join diary days with the night that follows them --------------------------------
build_days <- function(night_diary, morning_diary, actigraphy) {
  nights <- dplyr::full_join(actigraphy, morning_diary, by = "night")
  # Drop watch data for nights the teen said the watch was off.
  off <- !is.na(nights$watch_worn) & nights$watch_worn == 0
  acti_cols <- c("bed", "wake", "onset", "offset", "midpoint", "tib", "tst",
                 "se", "sol", "awakenings")
  for (v in acti_cols) {
    if (inherits(nights[[v]], "POSIXct")) nights[[v]][off] <- as.POSIXct(NA) else nights[[v]][off] <- NA
  }
  nights$watch_off_flag <- off
  dplyr::full_join(night_diary, dplyr::rename(nights, date = night), by = "date") |>
    dplyr::arrange(date)
}

# Clock helpers ------------------------------------------------------------------

# Minutes since the anchor hour (default 6 PM), so 11:30 PM and 12:30 AM average
# to midnight rather than noon.
.clock_min <- function(x, anchor_h = 18) {
  m <- as.numeric(format(x, "%H", tz = "UTC")) * 60 + as.numeric(format(x, "%M", tz = "UTC"))
  (m - anchor_h * 60) %% 1440
}
fmt_clock <- function(min_since_anchor, anchor_h = 18) {
  if (is.na(min_since_anchor)) return(NA_character_)
  m <- round((min_since_anchor + anchor_h * 60) %% 1440)
  h <- m %/% 60; mm <- m %% 60
  ap <- ifelse(h >= 12, "PM", "AM"); h12 <- ifelse(h %% 12 == 0, 12, h %% 12)
  sprintf("%d:%02d %s", h12, mm, ap)
}
fmt_hm <- function(minutes) {
  if (is.na(minutes)) return(NA_character_)
  minutes <- round(minutes); h <- minutes %/% 60; m <- minutes %% 60
  if (h == 0) return(sprintf("%d min", m))
  sprintf("%d h %02d min", h, m)
}

# Summaries ----------------------------------------------------------------------

.summ <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(c(mean = NA, min = NA, max = NA, sd = NA, n = 0))
  c(mean = mean(x), min = min(x), max = max(x),
    sd = if (length(x) > 1) stats::sd(x) else NA, n = length(x))
}

sleep_summary <- function(days, s = rth_settings()) {
  meas <- list(
    tib = "Time in bed", tst = "Sleep time", se = "Sleep efficiency",
    sol = "Time to fall asleep", awakenings = "Awakenings")
  rows <- lapply(names(meas), function(v) {
    x <- .summ(days[[v]])
    tibble::tibble(var = v, label = meas[[v]], mean = x[["mean"]], min = x[["min"]],
                   max = x[["max"]], sd = x[["sd"]], n = x[["n"]])
  })
  dplyr::bind_rows(rows)
}

timing_summary <- function(days, s = rth_settings()) {
  f <- function(v) {
    m <- .clock_min(days[[v]], s$clock_anchor_h)
    x <- .summ(m)
    tibble::tibble(var = v, mean_clock = fmt_clock(x[["mean"]], s$clock_anchor_h),
                   earliest = fmt_clock(x[["min"]], s$clock_anchor_h),
                   latest = fmt_clock(x[["max"]], s$clock_anchor_h),
                   sd_min = x[["sd"]], n = x[["n"]], mean_min = x[["mean"]])
  }
  dplyr::bind_rows(f("bed"), f("onset"), f("midpoint"), f("offset"), f("wake")) |>
    dplyr::mutate(label = c(bed = "Got into bed", onset = "Fell asleep",
                            midpoint = "Middle of sleep", offset = "Woke up",
                            wake = "Got out of bed")[var])
}

days_summary <- function(days) {
  vars <- c("stress_n", "pos_n", "help_n", "neg_pomp", "pos_pomp")
  rows <- lapply(vars, function(v) {
    x <- .summ(days[[v]])
    tibble::tibble(var = v, mean = x[["mean"]], min = x[["min"]], max = x[["max"]],
                   sd = x[["sd"]], n = x[["n"]])
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "domains") <- c(
    family = sum(days$stress_family, na.rm = TRUE),
    friends = sum(days$stress_peer, na.rm = TRUE),
    school = sum(days$stress_school, na.rm = TRUE),
    unfair = sum(days$stress_unfair, na.rm = TRUE))
  out
}

# Within-person slope --------------------------------------------------------------

within_slope <- function(x, y, unit, same_band, s = rth_settings()) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  res <- list(n = n, est = NA_real_, status = NA_character_,
              x_min = if (n) min(x) else NA, x_max = if (n) max(x) else NA)
  if (n < s$min_days) { res$status <- "too_few_days"; return(res) }
  if (length(unique(x)) < s$min_distinct_x) { res$status <- "no_variation"; return(res) }
  xc <- x - mean(x)
  b <- unname(stats::coef(stats::lm(y ~ xc))[2])
  res$est <- b * unit
  res$status <- if (abs(res$est) < same_band) "about_same" else "shown"
  res
}

link_table <- function(days, predictors, outcomes, s = rth_settings()) {
  pv <- rth_vars()$predictors
  out <- list()
  for (p in predictors) {
    type <- pv$type[pv$var == p]
    unit <- if (type == "pomp") s$pomp_unit else s$count_unit
    for (o in outcomes) {
      if (p == o) next
      r <- within_slope(days[[p]], days[[o]], unit, s$same_band[[o]], s)
      out[[length(out) + 1]] <- tibble::tibble(
        predictor = p, outcome = o, n = r$n, est = r$est, status = r$status)
    }
  }
  dplyr::bind_rows(out)
}

# Self-report vs. watch -------------------------------------------------------------

coherence <- function(days, s = rth_settings()) {
  pair <- function(label, sr, wa, kind) {
    if (kind == "clock") {
      d <- as.numeric(difftime(sr, wa, units = "mins"))
      a <- .clock_min(sr, s$clock_anchor_h); b <- .clock_min(wa, s$clock_anchor_h)
    } else {
      d <- sr - wa; a <- sr; b <- wa
    }
    ok <- !is.na(d)
    n <- sum(ok)
    r <- if (n >= s$min_days && stats::sd(a[ok]) > 0 && stats::sd(b[ok]) > 0)
      stats::cor(a[ok], b[ok]) else NA_real_
    tibble::tibble(measure = label, n = n,
                   mean_diff = if (n) mean(d[ok]) else NA_real_, r = r)
  }
  dplyr::bind_rows(
    pair("bedtime", days$sr_bed, days$bed, "clock"),
    pair("wake time", days$sr_wake, days$wake, "clock"),
    pair("time in bed", days$sr_tib, days$tib, "num"),
    pair("time to fall asleep", days$sr_sol, days$sol, "num"))
}

# Resource list trigger ---------------------------------------------------------------

resource_trigger <- function(days, s = rth_settings()) {
  x <- days$neg_pomp[!is.na(days$neg_pomp)]
  n <- length(x)
  share <- if (n) mean(x >= s$trigger_cutoff) else NA_real_
  list(triggered = n >= s$min_days && !is.na(share) && share >= s$trigger_share,
       n_days = n, share_high = share)
}

# Everything the report needs, in one object -----------------------------------------

compute_report <- function(id, age, night_diary, morning_diary, actigraphy,
                           s = rth_settings()) {
  days <- build_days(night_diary, morning_diary, actigraphy)
  v <- rth_vars()
  list(
    id = id, age = age, settings = s, days = days,
    n_diary   = sum(!is.na(days$stress_n) | !is.na(days$neg_pomp)),
    n_nights  = sum(!is.na(days$tst)),
    n_morning = sum(!is.na(days$sr_quality) | !is.na(days$sr_tib)),
    guideline = sleep_guideline(age),
    sleep     = sleep_summary(days, s),
    timing    = timing_summary(days, s),
    daysum    = days_summary(days),
    mood_links  = link_table(days, c("stress_n", "pos_n", "help_n"),
                             v$mood_outcomes$var, s),
    sleep_links = link_table(days, v$predictors$var, v$sleep_outcomes$var, s),
    coherence = coherence(days, s),
    trigger   = resource_trigger(days, s)
  )
}

#' Day-by-day pairing check for staff
#'
#' One row per date: when each diary was started, which date it was counted
#' for, whether there were duplicate entries, and whether watch data exist.
#' Written next to the report so staff can verify the day/night matching.
#' @keywords internal
pairing_table <- function(days) {
  f <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M", tz = "UTC"))
  tibble::tibble(
    date                 = days$date,
    night_diary_started  = f(days$night_started),
    night_diary_entries  = days$night_entries,
    morning_diary_started = f(days$morning_started),
    morning_diary_entries = days$morning_entries,
    watch_in_bed         = f(days$bed),
    watch_out_of_bed     = f(days$wake),
    watch_off_reported   = days$watch_off_flag,
    used_for_day_night_links = !is.na(days$stress_n) & !is.na(days$tst),
    check = dplyr::case_when(
      days$night_entries > 1 | days$morning_entries > 1 ~ "more than one diary for this date; first finished kept",
      days$watch_off_flag %in% TRUE ~ "watch reported off that night",
      !is.na(days$stress_n) & is.na(days$bed) ~ "diary but no watch night",
      is.na(days$night_started) & !is.na(days$bed) ~ "watch night but no night diary",
      TRUE ~ "")
  )
}
