# Simulated diary exports for the diary QA (no real participant data).
#
# Usage, from the package root:
#   Rscript inst/rth_diary_qa/simulate_rth_diary_qa.R
# Writes inst/rth_diary_qa/example_data/sim_caregiver_night.sav,
#        sim_adolescent_night.sav and sim_adolescent_morning.sav.
# The adolescent files use the structure of the feedback report's simulated
# exports (inst/rth_feedback/example_data); the caregiver file is built here.
#
# Each made-up participant shows one situation the QA handles:
#   caregiver 9012: 3 entries on one night (R1 + R2) and a late afternoon entry (R3)
#   caregiver 9032: afternoon + later entry on the same date, twice (R1), incl. the first night
#   caregiver "9042, 9052": one caregiver, two teens; an identical resubmission (R2)
#   caregiver 9062: restarts left unfinished (R0)
#   caregiver 9072: two entries on the last night, one took 0 seconds (R99)
#   caregiver 9082: nothing to fix
#   + one preview response and one response without an ID
#   adolescent night 9011: two entries, night before empty (R1); a restart (R0)
#   adolescent night 9031: nothing to fix
#   adolescent morning 9011: identical resubmission (R2)
#   adolescent morning 9031: two entries, day before empty (R1); a 01:31 entry (counts for the day before)
#   adolescent morning 9042: a caregiver ID in the teen diary (listed under rows to fix)

suppressPackageStartupMessages({ library(haven) })
set.seed(20261106)
out_dir <- "inst/rth_diary_qa/example_data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

feel <- paste0("feeling_", c("lonely", "angry", "calm", "close", "embarrass", "excited", "happy", "joyful",
                             "judged", "nervous", "rejected", "sad", "understood", "worried"))
morn <- c("bedtime_hr", "bedtime_min", "bedtime_am.pm", "latency", "waketime_hr", "waketime_min",
          "waketime_am.pm", "rested", "quality", "watchwear", "remove_watch")

answers <- function(kind) {
  if (kind == "night") return(as.list(stats::setNames(sample(1:5, length(feel), replace = TRUE), feel)))
  list(bedtime_hr = sample(9:11, 1), bedtime_min = sample(0:59, 1), bedtime_am.pm = 2, latency = sample(5:40, 1),
       waketime_hr = sample(6:8, 1), waketime_min = sample(0:59, 1), waketime_am.pm = 1,
       rested = sample(1:5, 1), quality = sample(1:5, 1), watchwear = 1, remove_watch = sample(0:2, 1))
}

# One response. start = "2026-08-07 21:05:12"
resp <- function(id, start, kind, dur = 420, finished = 1, progress = 100, status = 0, same_as = NULL) {
  st <- as.POSIXct(start, tz = "UTC")
  a <- if (is.null(same_as)) answers(kind) else same_as
  if (!finished) a[sample(names(a), ceiling(length(a) / 2))] <- NA
  c(list(StartDate = st, EndDate = st + dur, Status = status, Progress = progress, Finished = finished,
         Duration__in_seconds_ = dur, RecordedDate = st + dur,
         ResponseId = paste0("R_", paste(sample(c(letters, LETTERS, 0:9), 15, TRUE), collapse = "")),
         ExternalReference = id, DistributionChannel = "email", UserLanguage = "EN"), a)
}
# A regular diary: one entry per date at a usual time ("21:00" = same evening,
# "+08:30" = next morning), skipping some days.
regular <- function(id, first, n, kind, at, skip = integer()) {
  out <- list()
  for (k in setdiff(seq_len(n), skip)) {
    d <- as.Date(first) + k - 1
    next_day <- startsWith(at, "+")
    hm <- sub("^\\+", "", at)
    t <- as.POSIXct(paste(d + next_day, hm), tz = "UTC") + sample(-40:40, 1) * 60 + sample(0:59, 1)
    out[[length(out) + 1]] <- resp(id, format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC"), kind)
  }
  out
}

# Caregiver night diary -------------------------------------------------------------
cg <- c(
  # 9012: usually the next morning. 07/06 missing; 3 entries count for 07/07; 07/17 missing,
  # one afternoon entry on 07/18.
  regular("9012", "2026-07-04", 15, "night", "+09:00", skip = c(3, 4, 14, 15)),
  list(resp("9012", "2026-07-07 16:01:41", "night"), resp("9012", "2026-07-08 08:20:16", "night"),
       resp("9012", "2026-07-08 08:44:03", "night"), resp("9012", "2026-07-18 14:36:03", "night")),
  # 9032: evenings. Afternoon + late-night pairs on 10/05 (first date) and 10/11.
  regular("9032", "2026-10-05", 14, "night", "21:30", skip = c(1, 6, 7)),
  list(resp("9032", "2026-10-05 12:48:28", "night"), resp("9032", "2026-10-06 00:22:44", "night"),
       resp("9032", "2026-10-11 14:41:24", "night"), resp("9032", "2026-10-12 08:09:38", "night")),
  # "9042, 9052": one caregiver, two teens; 07/13 resubmitted with the same answers.
  regular("9042, 9052", "2026-07-07", 15, "night", "21:00", skip = 7))
a <- answers("night")
cg <- c(cg, list(resp("9042, 9052", "2026-07-13 17:36:46", "night", same_as = a),
                 resp("9042, 9052", "2026-07-13 21:29:37", "night", same_as = a)),
  # 9062: restarts on 09/27 and 10/04
  regular("9062", "2026-09-27", 15, "night", "21:15", skip = c(1, 8)),
  list(resp("9062", "2026-09-27 22:59:14", "night", finished = 0, progress = 34),
       resp("9062", "2026-09-27 23:03:41", "night"),
       resp("9062", "2026-10-04 22:28:51", "night", finished = 0, progress = 12),
       resp("9062", "2026-10-04 22:34:23", "night", finished = 0, progress = 61),
       resp("9062", "2026-10-04 22:37:52", "night")),
  # 9072: last night has a 0-second entry
  regular("9072", "2026-10-10", 14, "night", "21:00"),
  list(resp("9072", "2026-10-24 12:05:53", "night"), resp("9072", "2026-10-24 20:00:00", "night", dur = 0)),
  regular("9082", "2026-10-17", 15, "night", "20:45"),
  list(resp("9082", "2026-10-20 10:00:00", "night", status = 1),   # survey preview
       resp(NA, "2026-10-21 21:10:00", "night")))                    # no ID

# Adolescent night diary --------------------------------------------------------------
an <- c(
  regular("9011", "2026-08-01", 15, "night", "22:00", skip = c(6, 7, 12)),
  list(resp("9011", "2026-08-07 21:05:12", "night"), resp("9011", "2026-08-07 23:40:51", "night"),
       resp("9011", "2026-08-12 22:10:33", "night", finished = 0, progress = 40),
       resp("9011", "2026-08-12 22:14:02", "night")),
  regular("9031", "2026-10-05", 15, "night", "22:30"))

# Adolescent morning diary ------------------------------------------------------------
b <- answers("morning")
am <- c(
  regular("9011", "2026-08-02", 15, "morning", "07:10", skip = 10),
  list(resp("9011", "2026-08-11 07:20:09", "morning", same_as = b),
       resp("9011", "2026-08-11 07:24:55", "morning", same_as = b)),
  regular("9031", "2026-10-06", 15, "morning", "07:40", skip = c(4, 5, 12)),
  list(resp("9031", "2026-10-10 06:58:14", "morning"), resp("9031", "2026-10-10 07:01:38", "morning"),
       resp("9031", "2026-10-18 01:31:20", "morning")),
  regular("9042", "2026-10-06", 3, "morning", "07:30"))

# Write, shaped like the Qualtrics exports -----------------------------------------------
conform <- function(shell, rows) {
  sim <- dplyr::bind_rows(lapply(rows, tibble::as_tibble))
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
night_shell <- read_sav("inst/rth_feedback/example_data/sim_night_diary.sav")[0, ]
morning_shell <- read_sav("inst/rth_feedback/example_data/sim_morning_diary.sav")[0, ]
# Caregiver night diary: same Qualtrics columns, the feelings block and a few others
cg_cols <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration__in_seconds_", "Finished",
             "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
             "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")
cg_shell <- night_shell[, c(cg_cols, feel)]

write_sav(conform(cg_shell, cg), file.path(out_dir, "sim_caregiver_night.sav"))
write_sav(conform(night_shell, an), file.path(out_dir, "sim_adolescent_night.sav"))
write_sav(conform(morning_shell, am), file.path(out_dir, "sim_adolescent_morning.sav"))
cat("Simulated diary exports written to", out_dir, "\n")
