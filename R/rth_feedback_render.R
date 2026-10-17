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
#' @return Path to the HTML report (invisibly). Also writes a staff-only
#'   pairing check (\code{staff/rth_pairing_<id>.csv}) and appends to the staff log.
#' @export
render_rth_feedback <- function(id, age, night_sav, morning_sav, sleep_csv,
                                out_dir = ".",
                                log_file = file.path(out_dir, "staff", "rth_feedback_staff_log.csv"),
                                contact_email = "risetohealth@cmu.edu",
                                settings = rth_settings(),
                                drop_previews = TRUE) {
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
  out_file <- sprintf("rth_feedback_%s.html", id)
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
                    resource_box = rep$trigger$triggered)
  utils::write.table(row, log_file, sep = ",", row.names = FALSE,
                     col.names = !file.exists(log_file), append = file.exists(log_file))

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

#' Overview-card highlight: the link that stood out most (relative to its
#' "about the same" band), phrased to fit a card.
#' @keywords internal
link_highlight <- function(links, s, kind = c("sleep", "mood"), n_days = NA) {
  kind <- match.arg(kind)
  u <- s$pomp_unit
  pred <- c(stress_n = "1 more unpleasant event", pos_n = "1 more positive event",
            help_n = "more helping", neg_pomp = sprintf("%d-pt higher negative mood", u),
            pos_pomp = sprintf("%d-pt higher positive mood", u))
  outc <- c(tst = "sleep", se = "sleep efficiency", sol = "time to fall asleep",
            awakenings = "wake-ups", neg_pomp = "negative mood", pos_pomp = "positive mood")
  if (is.null(links) || !nrow(links) || !any(links$status %in% c("shown", "about_same"))) {
    return(list(value = "&mdash;", sub = sprintf("needs at least %d days with %s", s$min_days,
                                                if (kind == "sleep") "a diary and sleep data" else "a diary")))
  }
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
  e <- x$est; a <- abs(e); sg <- if (e > 0) "+" else "&minus;"
  value <- switch(x$outcome,
    tst = , sol = paste0(sg, round(a), " min"),
    awakenings = paste0(sg, format(round(a, 1), nsmall = 1)),
    paste0(sg, round(a), if (round(a) == 1) " pt" else " pts"))
  joiner <- if (kind == "sleep") "after days with" else "on days with"
  list(value = value, sub = sprintf("%s %s %s", outc[[x$outcome]], joiner, pred[[x$predictor]]))
}

# Template helpers -------------------------------------------------------------

# Embed a ggplot as a base64 PNG so the HTML stays a single file.
#' @keywords internal
img_tag <- function(p, height = 3, width = 4.4, alt = "") {
  if (is.null(p)) return("")
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, p, width = width, height = height, dpi = 200, bg = "white")
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
    '<span class="lg lg-ref"></span>reference range</p>',
    row("Sleep time", "time the watch recorded you as asleep", fmt_hm(get("tst", "mean")),
        rng("tst", fmt_hm), "tst", h_lo, h_hi, div = 60,
        ref = if (!is.null(g)) c(g$lo, g$hi), ref_txt = if (!is.null(g)) sprintf("Recommended: %d&ndash;%d h", g$lo, g$hi) else ""),
    row("Time in bed", "from getting into bed to getting up", fmt_hm(get("tib", "mean")),
        rng("tib", fmt_hm), "tib", h_lo, h_hi, div = 60),
    timing_rows(r),
    row("Sleep efficiency", "the % of your time in bed that you spent asleep",
        paste0(round(get("se", "mean")), "%"), rng("se", function(x) paste0(round(x), "%")),
        "se", se_lo, 100, ref = c(s$ref_se_min, 100),
        ref_txt = sprintf("Typical for good sleep: %d%% or more", s$ref_se_min)),
    row("Sleep latency", "how long it took you to fall asleep after getting into bed", fmt_hm(get("sol", "mean")),
        rng("sol", fmt_hm), "sol", 0, sol_hi, ref = c(0, s$ref_sol_max),
        ref_txt = sprintf("Typical for good sleep: %d min or less", s$ref_sol_max)),
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
             neg_pomp = "On days your negative mood was 25 points higher",
             pos_pomp = "On days your positive mood was 25 points higher")
  short <- c(stress_n = "unpleasant events", pos_n = "positive events", help_n = "helping others",
             neg_pomp = "negative mood", pos_pomp = "positive mood")
  col <- c(stress_n = rth_pal$stress, pos_n = rth_pal$positive, help_n = rth_pal$help,
           neg_pomp = rth_pal$stress, pos_pomp = rth_pal$positive)
  outs <- c(tst = "Sleep time", se = "Sleep efficiency", sol = "Sleep latency", awakenings = "Times you woke up")
  unit_txt <- c(tst = "min", se = "points", sol = "min", awakenings = "times")
  val <- function(o, e) {
    a <- abs(e); sg <- if (e > 0) "+" else "&minus;"
    switch(o, tst = paste0(sg, round(a), " min"), sol = paste0(sg, round(a), " min"),
           se = paste0(sg, round(a), if (round(a) == 1) " pt" else " pts"),
           awakenings = paste0(sg, format(round(a, 1), nsmall = 1), if (round(a, 1) == 1) " time" else " times"))
  }
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
        o <- xp$outcome[i]; e <- xp$est[i]; band <- s$same_band[[o]]
        lim <- max(3 * band, abs(e) * 1.25)
        pos <- function(v) 50 + 50 * v / lim
        sprintf(paste0('<div class="c-row"><div class="g-top"><span>%s</span><span class="g-val">%s</span></div>',
                       '<div class="c-track"><div class="c-same" style="left:%.1f%%;width:%.1f%%"></div>',
                       '<div class="c-bar" style="left:%.1f%%;width:%.1f%%;background:%s"></div>',
                       '<div class="g-avg" style="left:50%%"></div></div>',
                       '<div class="c-foot"><span>less than usual</span><span>more than usual</span></div></div>'),
                outs[[o]], val(o, e), pos(-band), pos(band) - pos(-band),
                min(pos(0), pos(e)), max(.6, abs(pos(e) - pos(0))), col[[p]])
      }, "")
      out <- paste0(out, sprintf('<div class="c-card" style="border-top-color:%s"><div class="c-title">%s&hellip;</div>%s</div>',
                                 col[[p]], preds[[p]], paste(rows, collapse = "")))
    }
    out
  }
  how_to_read <- 'The dark dot in the middle is your usual night, just like &ldquo;your average&rdquo; in the charts above. The colored bar shows how that night tended to differ: a bar to the right means more of that measure, to the left means less. A bar that stays inside the grey zone counts as &ldquo;about the same.&rdquo;'

  out <- ""
  if (!nrow(shown)) {
    out <- paste0(out, '<div class="callout"><p><strong>Your sleep looked about the same no matter what kind of day you had.</strong> On days with more unpleasant or positive events, more helping, or higher or lower mood, your sleep that night stayed about the same. That&rsquo;s a pattern too: for you, these two weeks of daytime ups and downs didn&rsquo;t show up much in your sleep.</p></div>')
    k <- s$same_show_top
    if (!is.null(k) && k > 0 && nrow(same)) {
      same$rel <- abs(same$est) / unlist(s$same_band[same$outcome])
      top <- same[order(-same$rel), ][seq_len(min(k, nrow(same))), ]
      top <- top[order(match(top$predictor, names(preds))), ]
      out <- paste0(out, sprintf('<p>Here are your %d largest differences, so you can see what &ldquo;about the same&rdquo; looks like. %s Notice that all of these bars stay inside the grey zone.</p>',
                                 nrow(top), how_to_read), cards(top))
    }
  } else {
    out <- paste0('<p>Each card compares a day with the sleep you got <em>that night</em>. We only show the ones that stood out. ', how_to_read, '</p>', cards(shown))
    if (nrow(same)) {
      chips <- paste(sprintf('<span class="chip">%s &rarr; %s</span>', short[same$predictor],
                             tolower(outs[same$outcome])), collapse = "")
      out <- paste0(out, sprintf('<p><strong>Everything else stayed about the same</strong> (%d of the %d comparisons).</p><details><summary>See which ones</summary><p class="chips">%s</p></details>',
                                 nrow(same), nrow(sk), chips))
    }
  }
  out <- paste0(out, '<div class="callout"><p><strong>What this can and can&rsquo;t tell you.</strong> Here there is an order: the day comes first, then the night. So this section looks at how a day tended to be followed by that night&rsquo;s sleep. That still isn&rsquo;t the same as one causing the other. Take days with more unpleasant events: if you tended to sleep less after them, it could be that what happened kept you up, or that something else, like a late practice or a big assignment due, both made the day harder and pushed your bedtime later. A short night before could also make the next day feel harder.</p></div>')
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
    '<p class="g-legend"><span class="lg lg-avg"></span>your average <span class="lg lg-range"></span>your range across days</p>',
    row("stress_n", "Unpleasant events", "like arguments, lots of schoolwork, or being left out", rth_pal$stress),
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
  sprintf('<h4 class="sub">Where your unpleasant events came from</h4><p>Across all your diaries you reported <strong>%d unpleasant events</strong>. Most of them involved <strong>%s</strong>.</p><div class="sources">%s</div>',
          tot, tolower(nm[[names(dom)[1]]]), paste(rows, collapse = ""))
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
