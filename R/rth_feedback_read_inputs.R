# Readers for the three raw inputs -------------------------------------------
# Each reader returns one tidy tibble for ONE participant. Scoring follows the
# lab's scoring scripts (rth_adolescent_night_diaries_10.15.26.R and
# rth_adolescent_morning_diaries_10.15.26.R); differences are noted inline.

# Helpers ----------------------------------------------------------------------

# Clock hour of a POSIXct as stored in the Qualtrics export (wall-clock time;
# haven tags it UTC, so read it in UTC to keep the wall-clock value).
.wall_hour <- function(x) as.numeric(format(x, "%H", tz = "UTC"))
.wall_date <- function(x) as.Date(format(x, "%Y-%m-%d", tz = "UTC"))

# Sum of 0/1 items for a multi-select block. Returns NA for rows where the block
# was never answered (no option ticked at all), following the lab rule that
# non-endorsement becomes 0 only when the block was answered.
.block_sum <- function(df, items, answered_items = items) {
  m <- as.matrix(sapply(df[items], function(v) as.numeric(v)))
  if (nrow(df) == 1) m <- matrix(m, nrow = 1)
  a <- as.matrix(sapply(df[answered_items], function(v) as.numeric(v)))
  if (nrow(df) == 1) a <- matrix(a, nrow = 1)
  answered <- rowSums(!is.na(a)) > 0
  s <- rowSums(m == 1, na.rm = TRUE)
  ifelse(answered, s, NA_real_)
}

# Mean of Likert items, rescaled to POMP (0-100) against the theoretical range.
# Requires at least `min_prop` of items answered; otherwise NA.
.pomp_mean <- function(df, items, lo, hi, min_prop = 0.5) {
  m <- as.matrix(sapply(df[items], function(v) as.numeric(v)))
  if (nrow(df) == 1) m <- matrix(m, nrow = 1)
  n_ok <- rowSums(!is.na(m))
  mu <- rowMeans(m, na.rm = TRUE)
  out <- (mu - lo) / (hi - lo) * 100
  out[n_ok < ceiling(min_prop * length(items))] <- NA_real_
  out
}

.filter_id <- function(dat, id, drop_previews) {
  dat <- dplyr::filter(dat, trimws(as.character(externalreference)) == as.character(id))
  if (drop_previews && "status" %in% names(dat)) {
    # Qualtrics Status: 0 = IP Address (real response), 1 = Survey Preview,
    # 2 = Survey Test, 8 = Spam ...
    dat <- dplyr::filter(dat, as.numeric(status) %in% c(0, 4, 16))
  }
  dat
}

# Keep one diary per person per day: the first finished one (the rule written
# but commented out in the lab scripts).
.dedupe_days <- function(dat) {
  dat |>
    dplyr::add_count(diary_date, name = "n_entries") |>
    dplyr::arrange(diary_date, dplyr::desc(as.numeric(finished)), startdate) |>
    dplyr::distinct(diary_date, .keep_all = TRUE)
}

# Night diary ------------------------------------------------------------------

#' Read and score the night diary for one participant
#' @keywords internal
read_night_diary <- function(path, id, drop_previews = TRUE) {
  dat <- haven::read_sav(path)
  names(dat) <- tolower(names(dat))
  dat <- .filter_id(dat, id, drop_previews)
  if (nrow(dat) == 0) {
    return(tibble::tibble(date = as.Date(character()), night_started = as.POSIXct(character()),
                          night_entries = integer(), finished = numeric(),
                          stress_n = numeric(), stress_family = numeric(),
                          stress_peer = numeric(), stress_school = numeric(),
                          stress_unfair = numeric(), pos_n = numeric(),
                          help_n = numeric(), neg_pomp = numeric(),
                          pos_pomp = numeric()))
  }

  # Diary date: a diary started before noon counts as the previous day's entry.
  dat$diary_date <- ifelse(.wall_hour(dat$startdate) < 12,
                           .wall_date(dat$startdate) - 1,
                           .wall_date(dat$startdate))
  dat$diary_date <- as.Date(dat$diary_date, origin = "1970-01-01")
  dat <- .dedupe_days(dat)

  fam  <- paste0("events_family_", 1:13)
  peer <- paste0("events_peers_", 1:11)
  sch  <- paste0("events_school_", 1:7)
  dis  <- paste0("events_discrim_", 1:8)
  hfam <- paste0("help_family_", 1:4)
  hpeer <- paste0("help_peers_", 1:9)

  fam_pos  <- .block_sum(dat, fam[1:3],  fam)
  fam_neg  <- .block_sum(dat, fam[4:12], fam)
  peer_pos <- .block_sum(dat, peer[1:3], peer)
  peer_neg <- .block_sum(dat, peer[4:10], peer)

  # School: days without school have no school stressors (0).
  sch_neg <- .block_sum(dat, sch[1:6], sch)
  sch_neg[!is.na(dat$go_school) & as.numeric(dat$go_school) == 0] <- 0

  dis_sum   <- .block_sum(dat, dis[1:7], dis)
  help_fam  <- .block_sum(dat, hfam[1:3], hfam)
  help_peer <- .block_sum(dat, hpeer[1:8], hpeer)

  neg_items <- paste0("feeling_", c("lonely", "angry", "embarrass", "judged",
                                    "rejected", "nervous", "sad", "worried"))
  pos_items <- paste0("feeling_", c("calm", "excited", "joyful", "happy",
                                    "close", "understood"))

  tibble::tibble(
    date          = dat$diary_date,
    night_started = dat$startdate,
    night_entries = dat$n_entries,
    finished      = as.numeric(dat$finished),
    stress_family = fam_neg,
    stress_peer   = peer_neg,
    stress_school = sch_neg,
    stress_unfair = dis_sum,
    # Total stressful events: family + peer + school + unfair treatment.
    # NA unless all four blocks were answered, so partial diaries don't
    # look like low-stress days.
    stress_n      = fam_neg + peer_neg + sch_neg + dis_sum,
    pos_n         = fam_pos + peer_pos,
    help_n        = help_fam + help_peer,
    # Overall negative (8 items) and positive (6 items) emotion, 1-5 scale.
    # The lab script sums items; the mean rescaled to 0-100 is identical when
    # all items are answered and is not biased downward when one is skipped.
    neg_pomp      = .pomp_mean(dat, neg_items, 1, 5),
    pos_pomp      = .pomp_mean(dat, pos_items, 1, 5)
  )
}

# Morning diary ----------------------------------------------------------------

#' Read and score the morning diary for one participant
#' @keywords internal
read_morning_diary <- function(path, id, drop_previews = TRUE) {
  dat <- haven::read_sav(path)
  names(dat) <- tolower(names(dat))
  dat <- .filter_id(dat, id, drop_previews)
  empty <- tibble::tibble(night = as.Date(character()), morning_started = as.POSIXct(character()),
                          morning_entries = integer(), sr_bed = as.POSIXct(character()),
                          sr_wake = as.POSIXct(character()), sr_tib = numeric(),
                          sr_sol = numeric(), sr_rested = numeric(),
                          sr_quality = numeric(), watch_worn = numeric())
  if (nrow(dat) == 0) return(empty)

  # The morning diary describes the night before.
  dat$diary_date <- .wall_date(dat$startdate)
  dat <- .dedupe_days(dat)

  # Convert from value labels so the result doesn't depend on dropdown codes.
  lab_num <- function(v) suppressWarnings(as.numeric(as.character(haven::as_factor(v))))
  lab_chr <- function(v) tolower(as.character(haven::as_factor(v)))
  dec <- function(h, m, ap) {
    h <- lab_num(h); m <- lab_num(m); ap <- lab_chr(ap)
    ifelse(h == 12, 0, h) + ifelse(ap == "pm", 12, 0) + m / 60
  }
  bed_dec  <- dec(dat$bedtime_hr, dat$bedtime_min, dat$`bedtime_am.pm`)
  wake_dec <- dec(dat$waketime_hr, dat$waketime_min, dat$`waketime_am.pm`)

  night <- dat$diary_date - 1
  # Bedtimes from noon onward fall on the night's date; earlier ones are after
  # midnight. Wake times fall on the morning of the diary.
  bed_day <- ifelse(bed_dec >= 12, night, night + 1)
  sr_bed  <- as.POSIXct(as.Date(bed_day, origin = "1970-01-01"), tz = "UTC") + bed_dec * 3600
  sr_wake <- as.POSIXct(dat$diary_date, tz = "UTC") + wake_dec * 3600

  tibble::tibble(
    night      = night,
    morning_started = dat$startdate,
    morning_entries = dat$n_entries,
    sr_bed     = sr_bed,
    sr_wake    = sr_wake,
    sr_tib     = as.numeric(difftime(sr_wake, sr_bed, units = "mins")),
    sr_sol     = suppressWarnings(as.numeric(dat$latency)),
    sr_rested  = as.numeric(dat$rested),   # 1-5
    sr_quality = as.numeric(dat$quality),  # 1-5
    watch_worn = as.numeric(dat$watchwear) # 0 = no, 1 = yes
  ) |>
    dplyr::mutate(sr_tib = ifelse(sr_tib <= 0 | sr_tib > 20 * 60, NA_real_, sr_tib))
}

# Actigraphy (Motionlogger in-bed interval export) -------------------------------

#' Read a Motionlogger in-bed interval export
#' @keywords internal
# Motionlogger exports write dates as 09/14/2026, but a CSV re-saved in Excel
# becomes 9/14/26 (and sometimes 2026-09-14 or 11:41 PM). Try each format and
# keep the first one that parses every value into a plausible year; a 4-digit
# year format alone would read "26" as the year 0026.
.parse_acti_dt <- function(d, t) {
  x <- trimws(paste(as.character(d), as.character(t)))
  x[is.na(d) | is.na(t)] <- NA
  ok <- !is.na(x)
  if (!any(ok)) return(as.POSIXct(x, tz = "UTC"))
  fmts <- c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M",
            "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
            "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M %p", "%m/%d/%y %I:%M:%S %p", "%m/%d/%y %I:%M %p")
  for (f in fmts) {
    out <- as.POSIXct(x, format = f, tz = "UTC")
    yr <- as.integer(format(out[ok], "%Y"))
    if (!anyNA(out[ok]) && all(yr >= 2000 & yr <= 2100)) return(out)
  }
  stop("Can't read the dates/times in the sleep file (e.g. '", x[ok][1],
       "'). Expected month/day/year with a time, like 09/14/2026 22:24:10.", call. = FALSE)
}

read_actigraphy <- function(path) {
  raw <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  raw <- raw[!is.na(raw[["Start Date"]]), , drop = FALSE]
  if ("Interval Channel" %in% names(raw)) {
    raw <- raw[tolower(raw[["Interval Channel"]]) == "inbed", , drop = FALSE]
  }
  dt <- .parse_acti_dt
  start  <- dt(raw[["Start Date"]], raw[["Start Time"]])
  end    <- dt(raw[["End Date"]], raw[["End Time"]])
  onset  <- dt(raw[["First Onset Date"]], raw[["First Onset Time"]])
  offset <- dt(raw[["Last Offset Date"]], raw[["Last Offset Time"]])

  out <- tibble::tibble(
    # A night belongs to the evening it started: in-bed times before noon
    # (e.g. 12:41 AM) belong to the previous date.
    night       = ifelse(.wall_hour(start) < 12, .wall_date(start) - 1, .wall_date(start)),
    bed         = start,
    wake        = end,
    onset       = onset,
    offset      = offset,
    tib         = as.numeric(raw[["Total Time (mins)"]]),
    tst         = as.numeric(raw[["Sleep Time (mins)"]]),
    # Sleep efficiency: sleep as a share of time in bed ("Sleep Percent"), the
    # definition behind the National Sleep Foundation's 85% marker.
    se          = as.numeric(raw[["Sleep Percent"]]),
    sol         = as.numeric(raw[["Sleep Onset Latency (mins)"]]),
    awakenings  = as.numeric(raw[["Number of Awakenings"]])
  )
  out$night <- as.Date(out$night, origin = "1970-01-01")
  out$midpoint <- out$onset + (out$offset - out$onset) / 2

  # If a night has more than one in-bed interval, keep the longest.
  out |>
    dplyr::arrange(night, dplyr::desc(tib)) |>
    dplyr::distinct(night, .keep_all = TRUE)
}
