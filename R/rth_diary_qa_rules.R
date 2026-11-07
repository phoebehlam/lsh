# Diary QA: reading the exports and tagging entries ---------------------------
#
# One row per Qualtrics response. Each response is tagged for the participant
# who did it (for the caregiver diary that is the externalreference as typed,
# e.g. "1212, 1172"); the split into one row per ID happens only in the clean
# data, so a decision about a shared caregiver's entry covers both IDs.
#
# Times: haven reads Qualtrics times as UTC-tagged wall-clock times, so every
# hour and date is read in UTC to keep the clock time participants saw.

.qa_hour <- function(x) as.numeric(format(x, "%H", tz = "UTC"))
.qa_date <- function(x) as.Date(format(x, "%Y-%m-%d", tz = "UTC"))
.qa_md   <- function(d) format(d, "%m/%d")
.qa_ts   <- function(x) format(x, "%m/%d %H:%M:%S", tz = "UTC")
.qa_hms  <- function(x) format(x, "%H:%M:%S", tz = "UTC")

.qa_types <- list(
  caregiver_night    = list(kind = "night",   title = "caregiver night diary",    who = "caregiver",
                            whos = "caregivers",  role = "2"),
  adolescent_night   = list(kind = "night",   title = "adolescent night diary",   who = "adolescent",
                            whos = "adolescents", role = "1"),
  adolescent_morning = list(kind = "morning", title = "adolescent morning diary", who = "adolescent",
                            whos = "adolescents", role = "1"))
.qa_unit <- function(kind) if (kind == "night") "night" else "day"

# Date an entry is for, before any QA decision:
#   night diaries: started before 12:00 -> the previous night
#   morning diary: started before 04:00 -> the previous day
.qa_diary_date <- function(start, kind, s) {
  cut <- if (kind == "night") s$night_cutoff else s$morning_cutoff
  h <- .qa_hour(start)
  .qa_date(start) - ifelse(!is.na(h) & h < cut, 1, 0)
}

# Compliance, from the start time and the date the entry is for (should_date):
#   night:   1 = on or before should_date, or the next day before 04:00
#            2 = the next day 04:00-11:59;  3 = the next day from 12:00, or later
#   morning: 1 = should_date before 14:00 (or earlier);  2 = should_date from 14:00
#            3 = the next day or later
.qa_compliance <- function(start, sd, kind, s) {
  lag <- as.numeric(.qa_date(start) - sd)
  h <- .qa_hour(start)
  out <- rep(NA_integer_, length(start))
  set <- function(cond, v) { out[which(cond)] <<- v }
  if (kind == "night") {
    set(lag <= 0, 1L)
    set(lag == 1 & h < s$night_ontime_until, 1L)
    set(lag == 1 & h >= s$night_ontime_until & h < s$night_cutoff, 2L)
    set(lag == 1 & h >= s$night_cutoff, 3L)
    set(lag >= 2, 3L)
  } else {
    set(lag < 0, 1L)
    set(lag == 0 & h < s$morning_ontime_until, 1L)
    set(lag == 0 & h >= s$morning_ontime_until, 2L)
    set(lag >= 1, 3L)
  }
  out[is.na(sd)] <- NA_integer_
  out
}

# Binged entries kept: entries for the same date done in one sitting (each
# started within `hours` of the previous one), 2 or more, all kept (spread over
# different dates). They get compliance 4. If any entry of the sitting is
# dropped, the kept one keeps its usual code.
.qa_binge <- function(person, dd, start, kept, hours) {
  out <- rep(FALSE, length(person))
  for (g in split(seq_along(person), paste(person, dd))) {
    g <- g[order(start[g])]
    if (length(g) < 2) next
    gap <- c(Inf, as.numeric(difftime(start[g][-1], start[g][-length(g)], units = "hours")))
    for (sit in split(g, cumsum(gap > hours)))
      if (length(sit) >= 2 && all(kept[sit])) out[sit] <- TRUE
  }
  out
}

# "1212, 1172" / "1212,1172 " / "1062.0" -> "1212, 1172" / "1062"
.qa_person <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  vapply(strsplit(x, ","), function(p) {
    p <- sub("\\.0+$", "", trimws(p))
    p <- p[nzchar(p) & p != "NA"]
    if (length(p)) paste(p, collapse = ", ") else NA_character_
  }, "")
}
.qa_split <- function(person) trimws(strsplit(person, ",")[[1]])

# Number of survey questions answered (Qualtrics metadata, page timers and
# embedded data don't count), to tell a partial entry with usable answers from
# one that was only opened.
.qa_meta <- c("startdate", "enddate", "status", "ipaddress", "progress", "duration__in_seconds_", "finished",
              "recordeddate", "responseid", "recipientlastname", "recipientfirstname", "recipientemail",
              "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", "userlanguage",
              "complete", "id")
.qa_n_answers <- function(raw) {
  keep <- !(names(raw) %in% .qa_meta) &
    !grepl("_(first_click|last_click|page_submit|click_count)$|^q_|^nback|_trialdata$", names(raw))
  if (!any(keep)) return(rep(0L, nrow(raw)))
  m <- vapply(raw[keep], function(v) {
    v <- haven::zap_labels(v)
    !is.na(v) & trimws(as.character(v)) != ""
  }, logical(nrow(raw)))
  if (is.null(dim(m))) m <- matrix(m, nrow = nrow(raw))
  as.integer(rowSums(m))
}

# Read one export ---------------------------------------------------------------

.qa_read <- function(path, type, s) {
  kind <- .qa_types[[type]]$kind
  raw <- haven::read_sav(path)
  names(raw) <- tolower(names(raw))
  need <- c("startdate", "enddate", "responseid", "externalreference", "finished")
  miss <- setdiff(need, names(raw))
  if (length(miss))
    stop(sprintf("%s (%s) is missing column(s): %s. Is this the right Qualtrics export?",
                 .qa_types[[type]]$title, basename(path), paste(miss, collapse = ", ")), call. = FALSE)

  # Preview / test responses are left out (remove them in Qualtrics).
  st <- if ("status" %in% names(raw)) as.numeric(raw$status) else rep(0, nrow(raw))
  prev <- !(st %in% s$keep_status)
  previews <- data.frame(responseid = as.character(raw$responseid[prev]),
                         id = .qa_person(raw$externalreference[prev]),
                         started = .qa_ts(raw$startdate[prev]), stringsAsFactors = FALSE)
  raw <- raw[!prev, , drop = FALSE]

  rid <- as.character(raw$responseid)
  if (anyDuplicated(rid[!is.na(rid)]))
    stop(sprintf("%s (%s) has repeated response IDs: %s", .qa_types[[type]]$title, basename(path),
                 paste(unique(rid[duplicated(rid)]), collapse = ", ")), call. = FALSE)

  start <- raw$startdate
  x <- data.frame(.row = seq_len(nrow(raw)), responseid = rid, person = .qa_person(raw$externalreference),
                  stringsAsFactors = FALSE)
  x$start <- start
  x$end <- raw$enddate
  x$progress <- if ("progress" %in% names(raw)) suppressWarnings(as.numeric(raw$progress)) else NA_real_
  x$finished <- as.numeric(raw$finished) %in% 1
  x$diary_date <- .qa_diary_date(start, kind, s)
  x$use <- !is.na(x$person) & !is.na(start) & !is.na(rid)
  x$n_answers <- .qa_n_answers(raw)

  noid <- x[!x$use, c("responseid", "person"), drop = FALSE]
  noid$started <- ifelse(is.na(start[!x$use]), "no start time", .qa_ts(start[!x$use]))

  # IDs that don't look like this diary's 4-digit RTH IDs (caregivers end in 2, teens in 1)
  role <- .qa_types[[type]]$role
  ids <- unique(unlist(lapply(stats::na.omit(unique(x$person)), .qa_split)))
  bad <- ids[!grepl(sprintf("^[0-9]{3}%s$", role), ids)]

  # Answers used to tell a resubmission from a separate attempt
  items <- intersect(s$compare_items[[kind]], names(raw))
  cmp <- as.data.frame(lapply(raw[items], function(v) as.character(haven::zap_labels(v))),
                       stringsAsFactors = FALSE, check.names = FALSE)

  list(type = type, kind = kind, file = basename(path), raw = raw, x = x, cmp = cmp,
       previews = previews, noid = noid, bad_ids = bad)
}

# Tagging -----------------------------------------------------------------------

# logged: earlier decisions (responseid, final_action, final_should_date, rule).
# Logged decisions are applied first and never re-proposed; they still count
# as context (e.g. a night filled by an entry kept last week).
.qa_tag <- function(d, logged, s) {
  x <- d$x[d$x$use, , drop = FALSE]
  x <- x[order(x$person, x$start), , drop = FALSE]
  n <- nrow(x)
  x$decided <- FALSE; x$d_action <- NA_character_; x$d_sd <- as.Date(rep(NA, n)); x$d_rule <- NA_character_
  if (!is.null(logged) && nrow(logged)) {
    m <- match(x$responseid, logged$responseid)
    hit <- !is.na(m)
    x$decided[hit] <- TRUE
    x$d_action[hit] <- logged$final_action[m[hit]]
    x$d_sd[hit] <- logged$final_should_date[m[hit]]
    x$d_rule[hit] <- logged$rule[m[hit]]
    blank <- hit & x$d_action %in% "keep" & is.na(x$d_sd)
    x$d_sd[blank] <- x$diary_date[blank]
  }
  x$rule <- NA_character_; x$action <- NA_character_; x$p_sd <- as.Date(rep(NA, n)); x$cur <- as.Date(rep(NA, n))
  events <- list()
  for (p in unique(x$person)) {
    i <- which(x$person == p)
    out <- .qa_tag_person(x[i, , drop = FALSE], d$kind, s, d$cmp)
    x[i, c("rule", "action", "p_sd", "cur")] <- out$y[, c("rule", "action", "p_sd", "cur")]
    events <- c(events, out$events)
  }
  x$tagged <- !is.na(x$rule)
  x$compliance <- .qa_compliance(x$start, x$cur, d$kind, s)
  x$compliance[x$rule %in% "R99"] <- NA_integer_
  x$compliance[.qa_binge(x$person, x$diary_date, x$start, !is.na(x$cur) & !(x$rule %in% "R99"), s$binge_hours)] <- 4L

  # Row numbers in the proposals file: participants needing a decision first,
  # then by ID; within a participant, by start time.
  tp <- unique(x$person[x$tagged])
  needs <- vapply(tp, function(p) any(x$rule[x$person == p] %in% "R99"), TRUE)
  key <- suppressWarnings(as.numeric(sub(",.*", "", tp)))
  tp <- tp[order(!needs, key, tp)]
  x$row <- NA_integer_
  ord <- unlist(lapply(tp, function(p) which(x$person == p & x$tagged)))
  x$row[ord] <- seq_along(ord)
  list(type = d$type, kind = d$kind, x = x, events = events, persons = tp, d = d)
}

.qa_cmp_text <- function(cmp, a, b) {
  if (!ncol(cmp)) return("")
  va <- unlist(cmp[a, ]); vb <- unlist(cmp[b, ])
  same <- sum((va == vb & !is.na(va) & !is.na(vb)) | (is.na(va) & is.na(vb)))
  k <- ncol(cmp)
  if (same == k) sprintf(" answers identical on %d of %d items (looks like a resubmission).", k, k)
  else sprintf(" answers differ on %d of %d items.", k - same, k)
}

.qa_tag_person <- function(y, kind, s, cmp) {
  n <- nrow(y)
  unit <- .qa_unit(kind)
  dec <- y$decided
  cur <- y$diary_date
  cur[dec] <- y$d_sd[dec]
  cur[dec & y$d_action %in% "drop"] <- NA
  rule <- rep(NA_character_, n); act <- rep(NA_character_, n); psd <- as.Date(rep(NA, n))
  first <- min(y$diary_date)
  filled <- function(dt) any(!is.na(cur) & cur == dt)
  ev <- list()
  add_ev <- function(dd, r, txt, needs = FALSE)
    ev[[length(ev) + 1]] <<- list(person = y$person[1], date = dd, rule = r, text = txt, needs = needs)
  ts <- function(i) .qa_ts(y$start[i]); hms <- function(i) .qa_hms(y$start[i])
  offset <- function(i, dt) as.numeric(difftime(y$start[i], as.POSIXct(dt, tz = "UTC"), units = "hours"))
  clock <- function(h) sprintf("%02d:%02d", floor(h %% 24), round((h %% 1) * 60) %% 60)
  # Usual start time: median hours after midnight of the date, over entries
  # that are alone on their date (night diaries done the next morning are > 24).
  alone <- !duplicated(y$diary_date) & !duplicated(y$diary_date, fromLast = TRUE) & y$n_answers > 0
  typical <- if (sum(alone) >= s$typical_min_n) stats::median(offset(which(alone), y$diary_date[alone])) else NA
  rng <- range(c(y$diary_date, cur), na.rm = TRUE)

  for (dd in as.list(seq(rng[1], rng[2], by = "day"))) {
    idx <- which(!is.na(cur) & cur == dd)
    if (!length(idx)) next
    idx <- idx[order(y$start[idx])]
    und <- idx[!dec[idx]]
    if (!length(und)) next

    # R0: opened but nothing answered -> drop
    empty <- und[y$n_answers[und] == 0]
    if (length(empty)) {
      rule[empty] <- "R0"; act[empty] <- "drop"; cur[empty] <- NA
      add_ev(dd, "R0", sprintf("%s: %s opened but nothing answered, dropped.", .qa_md(dd), paste(hms(empty), collapse = " and ")))
      idx <- setdiff(idx, empty); und <- setdiff(und, empty)
      if (!length(und)) next
    }

    # One entry: R3 (night diaries) -------------------------------------------
    if (length(idx) == 1) {
      h <- .qa_hour(y$start[idx])
      if (kind == "night" && .qa_date(y$start[idx]) == dd && h >= s$r3_from && h < s$r3_until &&
          (dd - 1) >= first && !filled(dd - 1)) {
        rule[idx] <- "R3"; act[idx] <- "move"; psd[idx] <- dd - 1; cur[idx] <- dd - 1
        add_ev(dd, "R3", sprintf("%s has no entry and %s was started between %02d:00 and %02d:00, so it moves to %s.",
                                 .qa_md(dd - 1), ts(idx), s$r3_from, s$r3_until, .qa_md(dd - 1)))
      }
      next
    }

    # Two or more entries -------------------------------------------------------
    plan <- list(); txt <- list(); why <- NULL
    put <- function(i, r, a, sd) plan[[length(plan) + 1]] <<- list(i = i, r = r, a = a, sd = sd)
    g <- idx
    dur <- as.numeric(difftime(y$end[g], y$start[g], units = "secs"))
    off <- g[!is.na(dur) & dur <= 0]
    if (length(off))
      why <- sprintf("%s took 0 seconds (start and end times are the same), so it may have been entered or edited by hand.",
                     paste(hms(off), collapse = " and "))

    # R0: restarts (unfinished, and another entry started within restart_minutes)
    if (is.null(why)) {
      rs <- g[vapply(g, function(i) !y$finished[i] && any(g != i & y$start[g] > y$start[i] &
                       difftime(y$start[g], y$start[i], units = "mins") <= s$restart_minutes), TRUE)]
      if (length(rs)) {
        if (any(dec[rs])) {
          why <- "an unfinished entry kept in an earlier week looks like a restart."
        } else {
          for (i in rs) put(i, "R0", "drop", NA)
          g <- setdiff(g, rs)
          desc <- sprintf("%s (unfinished%s)", hms(rs),
                          ifelse(is.na(y$progress[rs]), "", sprintf(", %s%%", format(y$progress[rs]))))
          rest <- if (length(g) == 1) sprintf("%s kept.", hms(g)) else ""
          txt[[length(txt) + 1]] <- c("R0", sprintf("%s: %s restarted within %d minutes, dropped. %s", .qa_md(dd),
                                                    paste(desc, collapse = " and "), s$restart_minutes, rest))
          if (length(g) == 1 && !dec[g]) put(g, "R0", "keep", dd)
        }
      }
    }

    gap_note <- ""
    if (is.null(why) && length(g) >= 2) {
      # R1: empty nights/days right before -> the earliest entries fill them
      m <- 0
      while (!filled(dd - m - 1) && (dd - m - 1) >= first) m <- m + 1
      reaches_first <- (dd - m) == first
      n_move <- 0; targets <- as.Date(character())
      if (dd == first) {
        # first date: one entry may go to the night before, if it was started in the afternoon
        h <- .qa_hour(y$start[g[1]])
        if (kind == "night" && .qa_date(y$start[g[1]]) == dd && h >= s$r3_from && h < s$r3_until) {
          n_move <- 1; targets <- dd - 1
        }
      } else if (m >= 1 && m <= s$r1_max_gap && !reaches_first) {
        n_move <- min(m, length(g) - 1)
        targets <- dd - rev(seq_len(n_move))
      } else if (m > s$r1_max_gap || (m >= 1 && reaches_first)) {
        gap_note <- sprintf(" (the %d %ss before are empty: too long a gap to move entries back.)", m, unit)
      }
      if (n_move > 0) {
        mv <- g[seq_len(n_move)]
        if (any(dec[mv])) {
          why <- "an entry decided in an earlier week would have to move back."
        } else {
          n_on <- length(g)
          for (k in seq_along(mv)) put(mv[k], "R1", "move", targets[k])
          g <- g[-seq_len(n_move)]
          span <- if (n_move == 1) sprintf("none on %s", .qa_md(targets)) else
            sprintf("none on %s-%s", .qa_md(targets[1]), .qa_md(targets[n_move]))
          t1 <- if (n_move == 1) sprintf("%d entries on %s and %s. the earliest (%s) moves to %s.", n_on, .qa_md(dd),
                                         span, ts(mv), .qa_md(targets))
                else sprintf("%d entries on %s and %s. the earliest %d move back one each: %s.", n_on, .qa_md(dd), span,
                             n_move, paste(sprintf("%s to %s", ts(mv), .qa_md(targets)), collapse = ", "))
          if (length(g) == 1) {
            if (n_move == 1) t1 <- paste0(t1, .qa_cmp_text(cmp, y$.row[mv], y$.row[g]))
            if (!dec[g]) put(g, "R1", "keep", dd)
          }
          txt[[length(txt) + 1]] <- c("R1", t1)
        }
      }
      # R2: still 2+ -> keep one. Night entries all started from r2_evening_from
      # on the date itself: the one closest to the usual start time; otherwise
      # the earliest. Finished entries are preferred over unfinished ones.
      if (is.null(why) && length(g) >= 2) {
        cand <- g[y$finished[g]]
        if (!length(cand)) cand <- g
        evening <- kind == "night" && all(.qa_date(y$start[g]) == dd & .qa_hour(y$start[g]) >= s$r2_evening_from)
        by_typ <- evening && !is.na(typical)
        keeper <- if (by_typ) cand[which.min(abs(offset(cand, dd) - typical))] else cand[1]
        drops <- setdiff(g, keeper)
        if (any(dec[drops])) {
          why <- "an entry decided in an earlier week would be dropped."
        } else {
          if (!dec[keeper]) put(keeper, "R2", "keep", dd)
          for (i in drops) put(i, "R2", "drop", NA)
          how <- if (by_typ) sprintf(" all were started after %02d:00 and this %s usually starts around %s, so keep the closest.",
                                     s$r2_evening_from, if (grepl(",", y$person[1]) || grepl("2$", y$person[1])) "caregiver" else "participant",
                                     clock(typical)) else ""
          t2 <- if (length(plan) && any(vapply(plan, function(p) p$r == "R1", TRUE)))
                  sprintf("of the %d left:%s keep %s for %s and drop %s.", length(g), how, ts(keeper), .qa_md(dd),
                          paste(ts(drops), collapse = " and "))
                else sprintf("%d entries on %s and %s.%s keep %s, drop %s.", length(g), .qa_md(dd),
                             if (filled(dd - 1)) sprintf("%s is filled", .qa_md(dd - 1)) else "nothing can move back",
                             how, hms(keeper), paste(hms(drops), collapse = " and "))
          t2 <- paste0(t2, gap_note)
          if (length(drops) == 1) t2 <- paste0(t2, .qa_cmp_text(cmp, y$.row[keeper], y$.row[drops]))
          txt[[length(txt) + 1]] <- c("R2", t2)
        }
      }
    }

    if (!is.null(why)) {
      rule[und] <- "R99"
      add_ev(dd, "R99", sprintf("%d entries on %s: %s", length(idx), .qa_md(dd), why), needs = TRUE)
    } else {
      for (p in plan) {
        rule[p$i] <- p$r; act[p$i] <- p$a
        if (p$a == "drop") { cur[p$i] <- NA } else { psd[p$i] <- p$sd; cur[p$i] <- p$sd }
      }
      for (t in txt) add_ev(dd, t[1], gsub("  +", " ", trimws(t[2])))
    }
  }
  y$rule <- rule; y$action <- act; y$p_sd <- psd; y$cur <- cur
  list(y = y, events = ev)
}
