# Main entry point -------------------------------------------------------------

#' Render one participant's feedback report as a self-contained HTML file.
#'
#' @param id          4-digit study ID (matches ExternalReference in Qualtrics).
#' @param age         Participant age in years (numeric, entered manually).
#' @param night_sav   Path to the night diary .sav exported from Qualtrics.
#' @param morning_sav Path to the morning diary .sav exported from Qualtrics.
#' @param sleep_csv   Path to this participant's Motionlogger in-bed export (.csv).
#' @param out_dir     Folder for the report and the staff log.
#' @param log_file    Staff-only CSV log (one row per report; never shown to the teen).
#' @param contact_email Study contact shown in the report.
#' @param settings    rth_settings() list; change values there, not in the code.
#' @param drop_previews Drop Qualtrics preview/test responses (Status != 0).
#' @param file_name   Name of the teen's report file. Default:
#'   \code{RiseToHealth_YourDailyPatterns_<id>.html}.
#' @return Path to the HTML report (invisibly). Also writes a staff-only
#'   pairing check (\code{staff/rth_pairing_<id>.csv}) and appends to the staff log.
#' @export
render_rth_feedback <- function(id, age, night_sav, morning_sav, sleep_csv,
                                out_dir = ".",
                                log_file = file.path(out_dir, "staff", "rth_feedback_staff_log.csv"),
                                contact_email = "risetohealth@cmu.edu",
                                settings = rth_settings(),
                                drop_previews = TRUE,
                                file_name = sprintf("RiseToHealth_YourDailyPatterns_%s.html", id)) {
  id <- as.character(id)
  stopifnot(grepl("^[0-9]{4}$", id), is.numeric(age), length(age) == 1)
  for (f in c(night_sav, morning_sav, sleep_csv)) if (!file.exists(f)) stop("File not found: ", f)

  night   <- read_night_diary(night_sav, id, drop_previews)
  morning <- read_morning_diary(morning_sav, id, drop_previews)
  acti    <- read_actigraphy(sleep_csv)
  if (!nrow(night))   warning("No night diaries found for ID ", id, call. = FALSE)
  if (!nrow(morning)) warning("No morning diaries found for ID ", id, call. = FALSE)
  if (!nrow(acti))    warning("No in-bed intervals found in ", basename(sleep_csv), call. = FALSE)

  # Guard against a sleep file from the wrong participant. These stop before
  # anything is written, with a message meant for lab staff.
  .rth_check_sleep_file(id, sleep_csv, acti, night, morning)

  rep <- compute_report(id, age, night, morning, acti, settings)

  # Render from a temporary copy so nothing is written into the package folder.
  home <- .rth_home()
  work <- file.path(tempdir(), paste0("rth_", id, "_", as.integer(Sys.time())))
  dir.create(work, recursive = TRUE)
  file.copy(file.path(home, c("feedback_template.Rmd", "report.css", "rth_logo.png")), work)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- if (grepl("\\.html?$", file_name, ignore.case = TRUE)) file_name else paste0(file_name, ".html")
  rmarkdown::render(file.path(work, "feedback_template.Rmd"),
                    output_file = out_file, output_dir = normalizePath(out_dir),
                    params = list(report = rep, contact = contact_email),
                    envir = new.env(parent = environment(render_rth_feedback)), quiet = TRUE)

  # Staff-only outputs: day/night pairing check and the resource-box log.
  staff_dir <- dirname(log_file)
  dir.create(staff_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(pairing_table(rep$days, rep$clipped), file.path(staff_dir, sprintf("rth_pairing_%s.csv", id)),
                   row.names = FALSE, na = "")
  row <- data.frame(id = id, rendered = format(Sys.time(), "%Y-%m-%d %H:%M"),
                    diary_days = rep$n_diary, watch_nights = rep$n_nights,
                    neg_mood_days = rep$trigger$n_days,
                    share_neg_mood_high = round(rep$trigger$share_high, 2),
                    resource_box = rep$trigger$triggered,
                    redo_offer = sum(!is.na(rep$days$stress_n) & !is.na(rep$days$tst)) < settings$min_days)
  # Re-write the log with all columns, so logs started before a column was
  # added (e.g. redo_offer) stay lined up; old rows get blanks there.
  if (file.exists(log_file)) {
    old <- utils::read.csv(log_file, colClasses = "character", check.names = FALSE)
    row <- dplyr::bind_rows(old, dplyr::mutate(row, dplyr::across(dplyr::everything(), as.character)))
  }
  utils::write.csv(row, log_file, row.names = FALSE, na = "")

  path <- file.path(normalizePath(out_dir), out_file)
  n_both <- sum(!is.na(rep$days$stress_n) & !is.na(rep$days$tst))
  message("Report written: ", path,
          sprintf("\n  %d diary days, %d watch nights, %d days with both.",
                  rep$n_diary, rep$n_nights, n_both),
          if (rep$trigger$triggered) "\n  Resource box included - see staff log." else "")
  if (n_both < settings$min_days && min(rep$n_diary, rep$n_nights) < settings$min_days) {
    message(sprintf("  Note: fewer than %d diary days or watch nights, so the events-and-sleep section is not shown.",
                    settings$min_days))
  } else if (n_both < settings$min_days) {
    warning(sprintf(paste0(
      "ID %s: only %d day(s) matched between the diaries and the sleep file, so the ",
      "events-and-sleep section is not shown (needs %d).\n",
      "This is not common. Please check that you read in the correct sleep file for this participant, ",
      "and look at staff/rth_pairing_%s.csv."), id, n_both, settings$min_days, id), call. = FALSE)
  }
  invisible(path)
}

# Where the template, CSS and logo live: inst/rth_feedback in the installed
# package (whatever it is named), or the option set by dev/load_dev.R when
# working from source.
.rth_home <- function() {
  pkg <- tryCatch(utils::packageName(environment(.rth_home)), error = function(e) NULL)
  p <- if (is.null(pkg)) "" else system.file("rth_feedback", package = pkg)
  if (nzchar(p) && file.exists(file.path(p, "feedback_template.Rmd"))) return(p)
  p <- getOption("rth_feedback.home")
  if (is.null(p)) stop("Can't find the report template: install the lsh package or run dev/load_dev.R")
  p
}

# Stops with a plain-language message if the sleep file looks like it belongs
# to someone else: a different ID in its name, or no dates in common with the
# diaries.
.rth_check_sleep_file <- function(id, sleep_csv, acti, night, morning) {
  head <- sprintf("No report written for ID %s.\n", id)
  tail <- "Please check that you read in the correct sleep file for this participant."
  ids <- unique(stats::na.omit(c(.lead_id(sleep_csv), attr(acti, "file_ids"))))
  if (length(ids) && !id %in% ids) {
    stop(head, sprintf("The sleep file looks like it belongs to participant %s:\n  %s\n",
                       paste(ids, collapse = "/"), basename(sleep_csv)), tail, call. = FALSE)
  }
  diary_dates <- c(night$date, morning$night)
  if (length(diary_dates) && nrow(acti) &&
      !any(acti$night %in% diary_dates) && !any(acti$night %in% (diary_dates - 1))) {
    f <- function(x) format(range(x), "%b %d, %Y")
    stop(head, "There are no matching days between the diaries and the sleep file:\n",
         sprintf("  diaries:    %s to %s\n", f(diary_dates)[1], f(diary_dates)[2]),
         sprintf("  sleep file: %s to %s\n", f(acti$night)[1], f(acti$night)[2]),
         "This is not common. ", tail, call. = FALSE)
  }
  invisible(TRUE)
}

#' Text for one "went together" difference, e.g. "+24 min", "-3%",
#' "+1.3 times", or "+ >60 min" when it was bigger than the display cap.
#' @keywords internal
link_value <- function(o, e, capped = FALSE) {
  a <- abs(e); up <- e > 0
  sg <- if (up) "+" else "&minus;"
  if (isTRUE(capped)) {
    # Same look as other values, with ">" for "more than": "+ >60 min"
    return(switch(o,
      tst = , sol = sprintf("%s &gt;%d min", sg, round(a)),
      se = sprintf("%s &gt;%d%%", sg, round(a)),
      awakenings = sprintf("%s &gt;%s times", sg, format(a)),
      sprintf("%s &gt;%d%%", sg, round(a))))
  }
  # No sign when the difference rounds to zero ("0 min", not "-0 min")
  if ((o == "awakenings" && round(a, 1) == 0) || (o != "awakenings" && round(a) == 0)) sg <- ""
  switch(o,
    tst = , sol = paste0(sg, round(a), " min"),
    se = paste0(sg, round(a), "%"),
    awakenings = if (sg == "") "0 times" else paste0(sg, format(round(a, 1), nsmall = 1), if (round(a, 1) == 1) " time" else " times"),
    paste0(sg, round(a), "%"))
}

cap_note <- '<p class="hint">A &ldquo;&gt;&rdquo; before a number (like &gt;60 min) means the difference was bigger than that. With only about two weeks of days, numbers that large can&rsquo;t be pinned down exactly.</p>'

#' Overview-card highlight: the link that stood out most (relative to its
#' "about the same" band), phrased to fit a card.
#' @keywords internal
link_highlight <- function(links, s, kind = c("sleep", "mood"), n_days = NA) {
  kind <- match.arg(kind)
  u <- s$mood_pct_unit
  pred <- c(stress_n = "1 more unpleasant event", pos_n = "1 more positive event",
            help_n = "more helping", neg_pomp = sprintf("%d%% higher negative mood", u),
            pos_pomp = sprintf("%d%% higher positive mood", u))
  outc <- c(tst = "sleep", se = "sleep efficiency", sol = "time to fall asleep",
            awakenings = "wake-ups", neg_pomp = "negative mood", pos_pomp = "positive mood")
  if (is.null(links) || !nrow(links) || !any(links$status %in% c("shown", "about_same"))) {
    return(list(value = "&mdash;", sub = sprintf("needs at least %d days with %s", s$min_days,
                                                if (kind == "sleep") "a diary and sleep data" else "a diary")))
  }
  if (!"est_show" %in% names(links)) { links$est_show <- links$est; links$capped <- FALSE }
  sh <- links[links$status == "shown", ]
  if (!nrow(sh)) {
    return(list(value = "About the same",
                sub = if (kind == "sleep") "your sleep stayed steady across your days"
                      else "your mood stayed steady across your events"))
  }
  sh$rel <- abs(sh$est) / unlist(s$same_band[sh$outcome])
  # The card is about events, so prefer an event link; mood links only if
  # none of the events stood out.
  ev <- sh[sh$predictor %in% c("stress_n", "pos_n", "help_n"), ]
  if (nrow(ev)) sh <- ev
  x <- sh[which.max(sh$rel), ]
  e <- x$est_show; a <- abs(e); up <- e > 0; sg <- if (up) "+" else "&minus;"
  joiner <- if (kind == "sleep") "after days with" else "on days with"
  if (isTRUE(x$capped)) {
    value <- switch(x$outcome, tst = , sol = sprintf("%s &gt;%d min", sg, round(a)),
                    se = sprintf("%s &gt;%d%%", sg, round(a)),
                    awakenings = sprintf("%s &gt;%s", sg, format(a)), sprintf("%s &gt;%d%%", sg, round(a)))
    return(list(value = value, sub = sprintf("%s %s %s", outc[[x$outcome]], joiner, pred[[x$predictor]])))
  }
  value <- switch(x$outcome,
    tst = , sol = paste0(sg, round(a), " min"),
    se = paste0(sg, round(a), "%"),
    awakenings = paste0(sg, format(round(a, 1), nsmall = 1)),
    paste0(sg, round(a), "%"))
  list(value = value, sub = sprintf("%s %s %s", outc[[x$outcome]], joiner, pred[[x$predictor]]))
}

#' "Your highlights" recap at the top of the report
#'
#' Short, upbeat tiles (participation, positive events, helping, typical
#' sleep, and the standout events-mood and events-sleep links). Every number
#' comes straight from the sections below; tiles with nothing to show are
#' left out.
#' @keywords internal
highlights_html <- function(r) {
  s <- r$settings; d <- r$days
  tile <- function(href, kicker, value, line, col)
    sprintf('<a class="hl" href="#%s" style="--c:%s"><span class="hl-kicker">%s</span><span class="hl-value">%s</span><span class="hl-line">%s</span></a>',
            href, col, kicker, value, line)
  tiles <- character()
  if (r$n_diary > 0) {
    pos <- round(sum(d$pos_n, na.rm = TRUE)); hlp <- round(sum(d$help_n, na.rm = TRUE))
    if (pos > 0) tiles <- c(tiles, tile("days", "Good stuff", pos,
      sprintf("positive %s, like quality time or feeling cared for", if (pos == 1) "event" else "events"), "#2FB5A6"))
    if (hlp > 0) tiles <- c(tiles, tile("days", "Helping hand", hlp,
      sprintf("%s you helped someone, like with chores or giving advice", if (hlp == 1) "time" else "times"), "#9A8AE6"))
  }
  if (r$n_nights > 0) {
    tst <- round(r$sleep$mean[r$sleep$var == "tst"]); se <- r$sleep$mean[r$sleep$var == "se"]
    tiles <- c(tiles, tile("sleep", "Your sleep", sprintf("%dh %02dm", tst %/% 60, tst %% 60),
                           "That&rsquo;s your typical night of shut-eye. Zzz!", "#F7A35C"))
    if (!is.na(se)) tiles <- c(tiles, tile("sleep", "Sleep efficiency", sprintf("%d%%", round(se)),
      "of your time in bed, you were actually snoozing", "#7D93B5"))
  }
  hm <- link_plain(r$mood_links, s, "mood")
  if (!is.null(hm)) tiles <- c(tiles, tile("mood-links", "Events &amp; mood", hm$value, hm$line, "#EE8A72"))
  hs <- link_plain(r$sleep_links, s, "sleep")
  if (!is.null(hs)) tiles <- c(tiles, tile("sleep-links", "Events &amp; sleep", hs$value, hs$line, "#E8879C"))
  paste0('<div class="hl-grid">', paste(tiles, collapse = ""), '</div>')
}

#' Plain-language version of the standout link, for the highlights tiles
#'
#' Same pick as link_highlight() (events before mood, biggest relative to its
#' "about the same" zone), but worded for teens: a size ("20%", "11 min") and
#' a short sentence with the direction ("more negative feelings on days with
#' one extra unpleasant event"). NULL when there aren't enough days.
#' @keywords internal
link_plain <- function(links, s, kind = c("sleep", "mood")) {
  kind <- match.arg(kind)
  if (is.null(links) || !nrow(links) || !any(links$status %in% c("shown", "about_same"))) return(NULL)
  if (!"est_show" %in% names(links)) { links$est_show <- links$est; links$capped <- FALSE }
  sh <- links[links$status == "shown", ]
  if (!nrow(sh)) {
    return(list(value = "Steady",
                line = if (kind == "sleep") "your sleep stayed about the same no matter what kind of day you had"
                       else "your mood stayed about the same no matter what happened in your day"))
  }
  sh$rel <- abs(sh$est) / unlist(s$same_band[sh$outcome])
  ev <- sh[sh$predictor %in% c("stress_n", "pos_n", "help_n"), ]
  if (nrow(ev)) sh <- ev
  x <- sh[which.max(sh$rel), ]
  o <- x$outcome; e <- x$est_show; up <- e > 0; a <- abs(e)
  gt <- if (isTRUE(x$capped)) "&gt;" else ""
  value <- switch(o,
    tst = , sol = sprintf("%s%d min", gt, round(a)),
    awakenings = if (gt == "") format(round(a, 1), nsmall = 1) else sprintf("&gt;%s", format(a)),
    sprintf("%s%d%%", gt, round(a)))
  what <- switch(o,
    tst = if (up) "more sleep" else "less sleep",
    sol = if (up) "longer to fall asleep" else "faster to fall asleep",
    se = if (up) "higher sleep efficiency" else "lower sleep efficiency",
    awakenings = if (up) "more wake-ups" else "fewer wake-ups",
    neg_pomp = if (up) "more negative feelings" else "fewer negative feelings",
    pos_pomp = if (up) "more positive feelings" else "fewer positive feelings")
  when <- if (kind == "sleep") "after days" else "on days"
  who <- switch(x$predictor,
    stress_n = "with one extra unpleasant event, like an argument",
    pos_n = "with one extra positive event, like quality time with friends",
    help_n = "you helped someone one extra time",
    neg_pomp = sprintf("you felt %d%% more negative", s$mood_pct_unit),
    pos_pomp = sprintf("you felt %d%% more positive", s$mood_pct_unit))
  list(value = value, line = paste(what, when, who))
}

#' "Your events" overview card: positive events or helping a day, whichever
#' was more common (positive events if tied).
#' @keywords internal
events_card <- function(dsm) {
  p <- dsm$pos_n; h <- dsm$help_n
  if (is.null(p) || is.na(p)) p <- NA_real_
  if (is.null(h) || is.na(h)) h <- NA_real_
  if (is.na(p) && is.na(h)) return(list(value = "&mdash;", sub = "events a day"))
  if (is.na(h) || (!is.na(p) && p >= h)) list(value = fmt_num(p), sub = "positive events a day")
  else list(value = fmt_num(h), sub = "times you helped others a day")
}

#' Offer of another paid round of daily assessments, shown when there are too
#' few diary + watch days for the events-and-sleep section.
#' @keywords internal
redo_offer_html <- function(contact = "risetohealth@cmu.edu") {
  sprintf(paste0('<div class="callout"><p><strong>Want to see this part of your report?</strong> ',
                 'If you&rsquo;d like, we&rsquo;re happy to set up another round of paid daily assessments: ',
                 'the morning and night diaries, wearing the sleep watch, and photos of your meals. ',
                 'You can earn up to <strong>$40</strong>: $25 for completing at least 12 of the 15 days, ',
                 'plus a $15 bonus for completing all 15. Just email us at <a href="mailto:%1$s">%1$s</a> to get started.</p></div>'),
          contact)
}

# Template helpers -------------------------------------------------------------

# Embed a ggplot as a base64 PNG so the HTML stays a single file.
#' @keywords internal
img_tag <- function(p, height = 3, width = 4.4, alt = "") {
  if (is.null(p)) return("")
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = width, height = height, dpi = 300, bg = "white")
  sprintf('<img class="chart" src="%s" alt="%s">', knitr::image_uri(f), alt)
}

card <- function(href, title, value, sub) {
  sprintf('<a class="card" href="#%s" data-target="%s"><span class="card-title">%s<span class="card-go" aria-hidden="true">&rsaquo;</span></span><span class="card-value">%s</span><span class="card-sub">%s</span></a>',
          href, href, title, value, sub)
}

fmt_num <- function(x, d = 1) if (is.na(x)) "&mdash;" else format(round(x, d), nsmall = d)

#' Oura-style "at a glance" rows: average (dot), night-to-night range (bar) and
#' a published reference zone where one applies.
#' @keywords internal
glance_html <- function(r) {
  sm <- r$sleep; s <- r$settings; g <- r$guideline
  get <- function(v, k) sm[[k]][sm$var == v]
  pct <- function(x, lo, hi) max(0, min(100, (x - lo) / (hi - lo) * 100))
  row <- function(name, def, val_txt, rng_txt, v, lo, hi, ref = NULL, ref_txt = "", div = 1) {
    mn <- get(v, "min") / div; mx <- get(v, "max") / div; av <- get(v, "mean") / div
    if (is.na(av)) return("")
    ref_div <- if (is.null(ref)) "" else sprintf('<div class="g-ref" style="left:%.1f%%;width:%.1f%%"></div>',
                                                  pct(ref[1], lo, hi), pct(ref[2], lo, hi) - pct(ref[1], lo, hi))
    sprintf(paste0(
      '<div class="g-row"><div class="g-top"><span class="g-name">%s%s</span><span class="g-val">%s</span></div>',
      '<div class="g-track">%s<div class="g-range" style="left:%.1f%%;width:%.1f%%"></div>',
      '<div class="g-avg" style="left:%.1f%%"></div></div>',
      '<div class="g-foot"><span>%s</span><span class="g-reflab">%s</span></div></div>'),
      name, if (nzchar(def)) sprintf('<span class="hint">%s</span>', def) else "",
      val_txt, ref_div, pct(mn, lo, hi), max(.8, pct(mx, lo, hi) - pct(mn, lo, hi)),
      pct(av, lo, hi), rng_txt, ref_txt)
  }
  h_lo <- min(4, floor(min(get("tst", "min"), get("tib", "min")) / 60))
  h_hi <- max(11, ceiling(max(get("tst", "max"), get("tib", "max")) / 60))
  se_lo <- min(70, floor(get("se", "min") / 5) * 5)
  sol_hi <- max(60, ceiling(get("sol", "max") / 10) * 10)
  aw_hi <- max(20, ceiling(get("awakenings", "max") / 5) * 5)
  rng <- function(v, f) sprintf("Range across nights: %s to %s", f(get(v, "min")), f(get(v, "max")))
  paste0(
    '<p class="g-legend"><span class="lg lg-avg"></span>your average <span class="lg lg-range"></span>your range across nights',
    '<span class="lg lg-ref"></span>NSF reference range for good or typical sleep</p>',
    row("Sleep time", "time the watch recorded you as asleep", fmt_hm(get("tst", "mean")),
        rng("tst", fmt_hm), "tst", h_lo, h_hi, div = 60,
        ref = if (!is.null(g)) c(g$lo, g$hi), ref_txt = if (!is.null(g)) sprintf("Recommended (NSF): %d&ndash;%d h", g$lo, g$hi) else ""),
    row("Time in bed", "from getting into bed to getting up", fmt_hm(get("tib", "mean")),
        rng("tib", fmt_hm), "tib", h_lo, h_hi, div = 60),
    row("Sleep efficiency", "the % of your time in bed that you spent asleep",
        paste0(round(get("se", "mean")), "%"), rng("se", function(x) paste0(round(x), "%")),
        "se", se_lo, 100, ref = c(s$ref_se_min, 100),
        ref_txt = sprintf("Typical for good sleep (NSF): %d%% or more", s$ref_se_min)),
    row("Time to fall asleep", "how long it took you to fall asleep after getting into bed", fmt_hm(get("sol", "mean")),
        rng("sol", fmt_hm), "sol", 0, sol_hi, ref = c(0, s$ref_sol_max),
        ref_txt = sprintf("Typical for good sleep (NSF): %d min or less", s$ref_sol_max)),
    row("Times you woke up", "how often the watch picked up you being awake during the night, even briefly",
        fmt_num(get("awakenings", "mean")), rng("awakenings", function(x) as.character(round(x))),
        "awakenings", 0, aw_hi)
  )
}

#' Small "read more" line with links
#' @keywords internal
refs_html <- function(...) {
  x <- list(...)
  sprintf('<p class="refs"><span>Want to read more?</span> %s</p>',
          paste(vapply(x, function(r) sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', r[2], r[1]), ""), collapse = " &middot; "))
}

#' Events & sleep section: change cards for the links that stood out, then a
#' single statement that everything else stayed about the same.
#' @keywords internal
events_sleep_html <- function(r) {
  sk <- r$sleep_links; s <- r$settings
  preds <- c(stress_n = "On days with 1 more unpleasant event",
             pos_n    = "On days with 1 more positive event",
             help_n   = "On days you helped others 1 more time",
             neg_pomp = sprintf("On days your negative mood was %d%% higher than your typical", s$mood_pct_unit),
             pos_pomp = sprintf("On days your positive mood was %d%% higher than your typical", s$mood_pct_unit))
  short <- c(stress_n = "unpleasant events", pos_n = "positive events", help_n = "helping others",
             neg_pomp = "negative mood", pos_pomp = "positive mood")
  col <- c(stress_n = rth_pal$stress, pos_n = rth_pal$positive, help_n = rth_pal$help,
           neg_pomp = rth_pal$stress, pos_pomp = rth_pal$positive)
  outs <- c(tst = "Sleep time", se = "Sleep efficiency", sol = "Time to fall asleep", awakenings = "Times you woke up")
  unit_txt <- c(tst = "min", se = "points", sol = "min", awakenings = "times")
  if (!"est_show" %in% names(sk)) { sk$est_show <- sk$est; sk$capped <- FALSE }
  shown <- sk[sk$status == "shown", ]
  same  <- sk[sk$status == "about_same", ]
  missing <- sk[sk$status %in% c("too_few_days", "no_variation"), ]

  # One card per kind of day, one row per sleep measure.
  cards <- function(x) {
    out <- ""
    for (p in names(preds)) {
      xp <- x[x$predictor == p, ]
      if (!nrow(xp)) next
      rows <- vapply(seq_len(nrow(xp)), function(i) {
        o <- xp$outcome[i]; e <- xp$est_show[i]; band <- s$same_band[[o]]
        lim <- max(3 * band, abs(e) * 1.25)
        pos <- function(v) 50 + 50 * v / lim
        sprintf(paste0('<div class="c-row"><div class="g-top"><span>%s</span><span class="g-val">%s</span></div>',
                       '<div class="c-track"><div class="c-same" style="left:%.1f%%;width:%.1f%%"></div>',
                       '<div class="c-bar" style="left:%.1f%%;width:%.1f%%;background:%s"></div>',
                       '<div class="g-avg" style="left:50%%"></div></div>',
                       '<div class="c-foot"><span>less than usual</span><span class="c-mid">your typical</span><span>more than usual</span></div></div>'),
                outs[[o]], link_value(o, e, xp$capped[i]), pos(-band), pos(band) - pos(-band),
                min(pos(0), pos(e)), max(.6, abs(pos(e) - pos(0))), col[[p]])
      }, "")
      out <- paste0(out, sprintf('<div class="c-card" style="border-top-color:%s"><div class="c-title">%s&hellip;</div>%s</div>',
                                 col[[p]], preds[[p]], paste(rows, collapse = "")))
    }
    out
  }
  how_to_read <- 'The dot in the middle is your typical night. Bar to the right? More than usual. To the left? Less. Stays in the grey zone? That&rsquo;s &ldquo;about the same.&rdquo;'

  out <- ""
  if (!nrow(shown)) {
    # Nothing stood out: still show the mood cards so the teen can see their
    # bars sitting inside "about the same", then say so for every comparison.
    mood_cards <- same[same$predictor %in% c("neg_pomp", "pos_pomp"), ]
    k <- s$same_show_top
    if (nrow(mood_cards)) {
      out <- paste0('<p>Did your days follow you to bed? Each card shows how your sleep <em>that night</em> tended to look after different kinds of days. ', how_to_read, '</p>',
                    '<p>None of your comparisons stood out, so here are the ones for your mood. Every bar stays inside the grey zone.</p>',
                    cards(mood_cards))
    } else if (!is.null(k) && k > 0 && nrow(same)) {
      same$rel <- abs(same$est) / unlist(s$same_band[same$outcome])
      top <- same[order(-same$rel), ][seq_len(min(k, nrow(same))), ]
      top <- top[order(match(top$predictor, names(preds))), ]
      out <- paste0(out, sprintf('<p>Here are your %d largest differences, so you can see what &ldquo;about the same&rdquo; looks like. %s Notice that all of these bars stay inside the grey zone.</p>',
                                 nrow(top), how_to_read), cards(top))
    }
    if (nrow(same)) {
      chips <- paste(sprintf('<span class="chip">%s &rarr; %s</span>', short[same$predictor],
                             tolower(outs[same$outcome])), collapse = "")
      out <- paste0(out, sprintf('<p><strong>Your sleep stayed about the same regardless of your events or mood</strong>, for all %d comparisons%s.</p><details><summary>See which ones</summary><p class="chips">%s</p></details>',
                                 nrow(same), if (nrow(same) < nrow(sk)) " we could look at" else "", chips))
    }
  } else {
    out <- paste0('<p>Did your days follow you to bed? Each card shows how your sleep <em>that night</em> tended to look after different kinds of days. We only show the ones that stood out. ', how_to_read, '</p>', cards(shown),
                  if (any(shown$capped)) cap_note else "")
    if (nrow(same)) {
      chips <- paste(sprintf('<span class="chip">%s &rarr; %s</span>', short[same$predictor],
                             tolower(outs[same$outcome])), collapse = "")
      out <- paste0(out, sprintf('<p><strong>Everything else stayed about the same</strong> (%d of the %d comparisons).</p><details><summary>See which ones</summary><p class="chips">%s</p></details>',
                                 nrow(same), nrow(sk), chips))
    }
  }
  out <- paste0(out, '<div class="callout"><p><strong>What this can and can&rsquo;t tell you.</strong> The day came before the night, but that doesn&rsquo;t mean the day caused how you slept. Something else could explain both: for example, a late practice could make a day harder <em>and</em> push your bedtime later.</p></div>')
  if (nrow(missing)) {
    out <- paste0(out, sprintf('<p class="hint">Not enough days to look at: %s.</p>',
                               paste(unique(sprintf("%s and %s", short[missing$predictor], tolower(outs[missing$outcome]))), collapse = "; ")))
  }
  out
}

#' "At a glance" rows for daily events: average per day (dot) and the range
#' across days (bar), in the same style as the sleep rows.
#' @keywords internal
events_glance_html <- function(r) {
  ds <- r$daysum
  get <- function(v, k) ds[[k]][ds$var == v]
  pct <- function(x, hi) max(0, min(100, x / hi * 100))
  row <- function(v, name, def, col) {
    av <- get(v, "mean"); if (is.na(av)) return("")
    mn <- get(v, "min"); mx <- get(v, "max")
    hi <- max(5, ceiling(mx) + 1)
    sprintf(paste0(
      '<div class="g-row"><div class="g-top"><span class="g-name">%s<span class="hint">%s</span></span>',
      '<span class="g-val">%s <span class="g-unit">a day</span></span></div>',
      '<div class="g-track"><div class="g-range" style="left:%.1f%%;width:%.1f%%;background:%s"></div>',
      '<div class="g-avg" style="left:%.1f%%"></div></div>',
      '<div class="g-foot"><span>Range across days: %d to %d</span><span>scale 0&ndash;%d</span></div></div>'),
      name, def, fmt_num(av), pct(mn, hi), max(1.2, pct(mx, hi) - pct(mn, hi)), col,
      pct(av, hi), round(mn), round(mx), hi)
  }
  paste0(
    '<p class="g-legend"><span class="lg lg-avg"></span>your average <span class="lg lg-range lg-neutral"></span>your range across days</p>',
    row("stress_n", "Unpleasant events", "like arguments or lots of schoolwork", rth_pal$stress),
    row("pos_n", "Positive events", "like quality time or feeling cared for by family or friends", rth_pal$sleep),
    row("help_n", "Times you helped others", "like helping with chores, giving advice, or standing up for someone", rth_pal$help))
}

#' Where unpleasant events came from: one bar per source, counts across all diaries
#' @keywords internal
sources_html <- function(dom) {
  tot <- sum(dom)
  if (tot == 0) return("<p>You didn't report any unpleasant events in your diaries.</p>")
  nm <- c(family = "Family", friends = "Friends or peers", school = "School", unfair = "Being treated unfairly")
  dom <- sort(dom, decreasing = TRUE)
  rows <- vapply(names(dom), function(k) sprintf(
    '<div class="s-row"><span class="s-name">%s</span><span class="s-bar"><span style="width:%.1f%%"></span></span><span class="s-n">%d</span></div>',
    nm[[k]], dom[[k]] / max(dom) * 100, dom[[k]]), "")
  sprintf('<h4 class="sub">Where your unpleasant events came from</h4><p>Across all your diaries you reported <strong>%d unpleasant %s</strong>.</p><div class="sources">%s</div>',
          tot, if (tot == 1) "event" else "events", paste(rows, collapse = ""))
}

#' Bedtime and wake-up time rows for the "at a glance" block, on a clock track
#' @keywords internal
timing_rows <- function(r) {
  a <- r$settings$clock_anchor_h
  one <- function(x, name, def, lo_h, hi_h) {
    m <- .clock_min(x, a); m <- m[!is.na(m)]
    if (!length(m)) return("")
    lo <- min(lo_h * 60, floor(min(m) / 60) * 60); hi <- max(hi_h * 60, ceiling(max(m) / 60) * 60)
    pct <- function(v) (v - lo) / (hi - lo) * 100
    sprintf(paste0(
      '<div class="g-row"><div class="g-top"><span class="g-name">%s<span class="hint">%s</span></span><span class="g-val">%s</span></div>',
      '<div class="g-track"><div class="g-range" style="left:%.1f%%;width:%.1f%%"></div><div class="g-avg" style="left:%.1f%%"></div></div>',
      '<div class="g-foot"><span>Range across nights: %s to %s</span><span>%s&ndash;%s</span></div></div>'),
      name, def, fmt_clock(mean(m), a), pct(min(m)), max(.8, pct(max(m)) - pct(min(m))), pct(mean(m)),
      fmt_clock(min(m), a), fmt_clock(max(m), a), sub(":00", "", fmt_clock(lo, a)), sub(":00", "", fmt_clock(hi, a)))
  }
  # Anchor is 6 PM, so 2 h = 8 PM, 8 h = 2 AM, 11 h = 5 AM, 17 h = 11 AM.
  paste0(one(r$days$bed, "Bedtime", "when you got into bed", 2, 8),
         one(r$days$wake, "Wake-up time", "when you got out of bed", 11, 17))
}
