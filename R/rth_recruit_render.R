# Weekly recruitment report ------------------------------------------------------

#' Render the weekly Rise to Health recruitment report (staff use)
#'
#' Reads the screener export and the participant tracking sheet (Google Sheet
#' or downloaded .xlsx), plus optionally the caregiver SES interview for
#' enrolled-family demographics, and writes one self-contained HTML file.
#' Names, emails and phone numbers are never shown.
#'
#' @param screener_sav  Prescreen export (.sav).
#' @param tracker Participant tracking sheet: the Google Sheets link (needs the
#'   googlesheets4 package; sign in once in the browser) or a downloaded .xlsx.
#' @param interview_sav Optional caregiver SES interview export (.sav), for
#'   household income and caregiver education of enrolled families.
#' @param out_dir Folder for the report.
#' @param week Study week to report on. Default: the last full week (Saturday
#'   to Friday) before \code{report_date}.
#' @param report_date Date of the run (default today).
#' @param study_start First day of week 1 (a Saturday).
#' @param seed_sheet Optional: the old weekly counts sheet (.tsv/.csv/.xlsx),
#'   for lab visits scheduled in weeks before the tracker logged booking dates.
#' @param tracker_tab Tab of the tracking sheet to read.
#' @param costs TRUE to add the Costs section (needs \code{costs_sheet});
#'   FALSE (default) leaves it out of the report entirely.
#' @param costs_sheet The RTH recruitment costs & hours sheet (Google Sheets
#'   link or downloaded .xlsx). Only read when \code{costs = TRUE}.
#' @param costs_tab Tab of the costs sheet to read.
#' @param hourly_rates Hourly cost of staff time, named as in the sheet's
#'   person column. Lab manager: $21.50/hour plus CMU's FY2027 fringe rate for
#'   full-time domestic staff (25.6\%) = $27.00. Grad student: $30,000 a year
#'   for about 10 hours a week (30000 / 520 = $57.69). Time logged for anyone
#'   else is costed with the amount column instead. These rates aren't printed
#'   in the report.
#' @param lm_hours_tab Tab of the costs sheet where the lab manager logs his
#'   other hours, for the "Lab manager's week" bar.
#' @param lm_week_hours Hours in the lab manager's full work week.
#' @param bots_tab Tab of the tracking sheet listing screener IDs that are
#'   bots ("Screener ID" column). Those screeners count as possible bots
#'   whatever their answers. Skipped if the tab doesn't exist.
#' @param source_groups Screener sources to report together, as
#'   c("screener choice" = "group"). Default: paper flyers, mail, bus ads and
#'   the candy stickers are grouped as "Local recruitment" (in the costs
#'   section too). Use \code{character()} to report every choice separately.
#' @return Path to the HTML report (invisibly).
#' @export
render_rth_recruitment <- function(screener_sav, tracker, interview_sav = NULL, out_dir = ".",
                                   week = NULL, report_date = Sys.Date(),
                                   study_start = as.Date("2026-04-25"), seed_sheet = NULL,
                                   tracker_tab = "tracking", costs = FALSE, costs_sheet = NULL, costs_tab = "costs",
                                   hourly_rates = c("Lab manager" = 21.50 * 1.256, "Grad student" = 30000 / 520),
                                   lm_hours_tab = "lab manager hours", lm_week_hours = 37.5,
                                   source_groups = .rth_source_groups, bots_tab = "sus bots") {
  study_start <- as.Date(study_start); report_date <- as.Date(report_date)
  for (f in c(screener_sav, interview_sav, seed_sheet))
    if (!file.exists(f)) stop("File not found: ", f, call. = FALSE)
  if (!is.logical(costs) || length(costs) != 1 || is.na(costs))
    stop("costs should be TRUE or FALSE. Put the costs sheet link in costs_sheet.", call. = FALSE)
  if (costs && is.null(costs_sheet))
    stop("costs = TRUE needs costs_sheet: the link to the costs & hours Google Sheet (or a downloaded .xlsx).", call. = FALSE)
  if (is.null(week)) week <- rth_study_week(report_date, study_start) - 1L
  if (week < 1) stop("The report week is before week 1; check study_start and report_date.", call. = FALSE)

  tr <- read_tracker(tracker, tracker_tab)
  fam <- tracker_families(tr, report_date)
  sus <- read_sus_bots(tracker, bots_tab)
  if (is.null(sus) && !is.data.frame(tracker)) message("No '", bots_tab, "' tab in the tracker; only reCAPTCHA scores and the survey's flag mark bots.")
  scr <- .group_sources(read_screener(screener_sav, approved = fam$screener_id, bots = sus), source_groups)
  ses <- if (!is.null(interview_sav)) read_interview_ses(interview_sav) else NULL
  seed <- if (!is.null(seed_sheet)) read_seed_sheet(seed_sheet) else NULL

  steps <- link_steps(scr, fam)
  wk_tab <- weekly_counts(scr, fam, study_start, week, seed)
  cs <- NULL
  if (isTRUE(costs)) {
    cst <- read_costs(costs_sheet, costs_tab, hourly_rates)
    cst$source <- .map_source(cst$source, source_groups)
    cs <- cost_summary(cst, scr, steps, week, study_start)
    cs$lmh <- read_lm_hours(costs_sheet, lm_hours_tab); cs$week_hours <- lm_week_hours
  }
  html <- recruitment_html(scr = scr, tr = tr, fam = fam, ses = ses, steps = steps, wk_tab = wk_tab,
                           week = week, study_start = study_start, report_date = report_date, cs = cs,
                           groups = source_groups)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(normalizePath(out_dir), sprintf("RTH_recruitment_week%02d_%s.html", week,
                                                    format(.week_start(week, study_start) + 6)))
  writeLines(html, path, useBytes = TRUE)
  this <- wk_tab[wk_tab$week == week, ]
  message("Recruitment report written: ", path,
          sprintf("\n  Week %d: %d completed screeners, %d eligible, %d caregiver consents, %d lab visits done.", week,
                  this$screeners, this$eligible, this$cg_consent, this$lab_done))
  invisible(path)
}

# HTML pieces ------------------------------------------------------------------------

.esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.bars <- function(x, empty = "None.") {
  if (!length(x) || sum(x) == 0) return(sprintf('<p class="note">%s</p>', empty))
  mx <- max(x)
  paste(sprintf('<div class="bar"><span>%s</span><div class="track"><div style="width:%.1f%%"></div></div><span class="n">%d</span></div>',
                names(x), 100 * x / mx, as.integer(x)), collapse = "")
}

.td <- function(v, show = v) sprintf('<td data-v="%s">%s</td>', ifelse(is.na(v), -1, v), show)
.pct_td <- function(p, n, missing = "&ndash;") {
  if (is.na(n)) return(.td(NA, sprintf('<span class="cnt">%s</span>', missing)))
  .td(if (is.na(p)) -1 else round(p, 1),
      sprintf('%s<span class="cnt">%d</span>', if (is.na(p)) "&ndash;" else sprintf("%.0f%%", p), as.integer(n)))
}
.table <- function(id, head, body, foot = NULL, cls = "") {
  # Totals sit right under the column headers (they stay put when sorting)
  sprintf('<div class="scroll"><table class="sortable %s" id="%s"><thead><tr>%s</tr>%s</thead><tbody>%s</tbody></table></div>',
          cls, id, paste0("<th>", head, "</th>", collapse = ""),
          if (is.null(foot)) "" else sprintf('<tr class="total">%s</tr>', foot), paste(body, collapse = ""))
}
.conv_cells <- function(r) paste0(
  .td(r$screeners_all), .td(r$screeners), .pct_td(r$pct_eligible, r$eligible), .pct_td(r$pct_consent, r$consent),
  .pct_td(r$pct_interview, r$interview), .pct_td(r$pct_survey, r$survey), .pct_td(r$pct_sched, r$sched),
  .pct_td(r$pct_lab, r$lab))
.conv_head <- c("All screeners", "Completed screeners", "Eligible", "Caregiver consent", "Caregiver interview", "Caregiver survey",
                "Lab visit scheduled", "Lab visit done")

# Okabe-Ito colors: easy to tell apart, colorblind-safe
.src_cols <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#9CA3AF")

plot_source_weeks <- function(scr, sw, weeks, top) {
  keep <- scr$completed
  src <- attr(scr, "sources")[keep, , drop = FALSE]; w <- sw[keep]
  grp <- c(top, "Remaining sources")
  d <- do.call(rbind, lapply(weeks, function(k) {
    s <- src[!is.na(w) & w == k, , drop = FALSE]
    v <- c(colSums(s[, intersect(top, colnames(s)), drop = FALSE]),
           `Remaining sources` = sum(s[, setdiff(colnames(s), top), drop = FALSE]))
    data.frame(week = k, source = factor(names(v), levels = rev(grp)), n = as.numeric(v))
  }))
  ggplot2::ggplot(d, ggplot2::aes(factor(week), n, fill = source)) +
    ggplot2::geom_col(width = .75) +
    ggplot2::scale_fill_manual(values = stats::setNames(rev(.src_cols[seq_along(grp)]), rev(grp)),
                               breaks = grp, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(x = "Study week", y = "Completed screeners (by source)") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(legend.position = "top", panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
}

# Daily assessment completion: how many teens completed each number of days
# (out of 15), one panel per assessment; 12+ (full pay) in the darker shade.
plot_completion <- function(ct) {
  labs <- c(night = "Teen night diaries", morning = "Teen morning diaries", food = "Food photos",
            watch = "Watch nights worn", cg_night = "Caregiver night diaries")
  d <- do.call(rbind, lapply(names(labs), function(v) {
    x <- ct[[v]][!is.na(ct[[v]])]
    if (!length(x)) return(NULL)
    t <- table(x)
    data.frame(measure = labs[[v]], days = as.numeric(names(t)), n = as.integer(t))
  }))
  if (is.null(d) || !nrow(d)) return(NULL)
  # one row per measure (first at the top), one circle per number of days
  d$row <- match(d$measure, rev(labs))
  d$hit <- ifelse(d$days >= 12, "12 or more days", "Fewer than 12 days")
  nmax <- max(d$n)
  ggplot2::ggplot(d, ggplot2::aes(days, row)) +
    ggplot2::annotate("rect", xmin = 11.5, xmax = 15.5, ymin = 0.5, ymax = length(labs) + 0.5, fill = "#ECFDF5") +
    ggplot2::geom_hline(yintercept = seq(1.5, length(labs) - 0.5), colour = "#D1D5DB", linewidth = .3) +
    ggplot2::geom_point(ggplot2::aes(size = n, fill = hit), shape = 21, colour = "white", stroke = .4) +
    ggplot2::geom_text(ggplot2::aes(label = n, colour = hit), size = 2.6, fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`12 or more days` = "#334155", `Fewer than 12 days` = "#94A3B8"), name = NULL) +
    ggplot2::scale_colour_manual(values = c(`12 or more days` = "white", `Fewer than 12 days` = "white")) +
    ggplot2::scale_size_continuous(range = c(4.6, if (nmax > 1) 9 else 4.6), limits = c(1, max(2, nmax)), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-0.5, 15.5), breaks = 0:15, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.5, length(labs) + 0.5), breaks = seq_along(labs), labels = rev(labs), expand = c(0, 0)) +
    ggplot2::labs(x = "Days completed (out of 15); shaded = 12 or more", y = NULL,
                  caption = "Number in each circle = participants who completed that many days") +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 4))) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "#9CA3AF", fill = NA, linewidth = .4),
                   axis.ticks.x = ggplot2::element_line(colour = "#9CA3AF", linewidth = .3),
                   axis.text.y = ggplot2::element_text(size = 9, colour = "#1F2937"),
                   plot.caption = ggplot2::element_text(colour = "#6B7280", hjust = 0))
}

.demo_html <- function(title, t) {
  rows <- vapply(seq_len(nrow(t)), function(i) {
    e <- t$eligible[i]; n <- t$not_eligible[i]
    sprintf('<div class="drow"><span>%s</span><div><div class="db el" style="width:%.1f%%"></div><div class="db ne" style="width:%.1f%%"></div></div><span class="dn">%s<br>%s</span></div>',
            .esc(t$group[i]), if (is.na(e)) 0 else e, if (is.na(n)) 0 else n,
            if (is.na(e)) "&ndash;" else sprintf("%.0f%%", e), if (is.na(n)) "&ndash;" else sprintf("%.0f%%", n))
  }, "")
  sprintf('<div class="demo"><h3>%s</h3>%s</div>', title, paste(rows, collapse = ""))
}

.wk_cell <- function(k, wl) .td(k, sprintf('%d <span class="cnt">%s</span>', k, wl(k)))

.checks_html <- function(chk) {
  items <- vapply(chk, function(c) {
    if (c$n == 0) return(sprintf('<li class="ok"><span class="mark">&#10003;</span>%s: none</li>', c$label))
    ids <- if (length(c$ids) > 0) sprintf('<details><summary>Show %d</summary><p class="ids">%s</p></details>', c$n,
                                          paste(.esc(c$ids), collapse = ", ")) else ""
    sprintf('<li class="flag"><span class="mark">!</span><strong>%s: %d</strong>%s%s</li>', c$label, c$n,
            if (nzchar(c$help)) sprintf(' <span class="note">%s</span>', c$help) else "", ids)
  }, "")
  sprintf('<ul class="checks">%s</ul>', paste(items, collapse = ""))
}

# Low reCAPTCHA scores across all outcomes. The screener only gives the
# "possible bot" status to otherwise-eligible caregivers, so most low scores
# end up under Ineligible, Didn't finish, etc.
.recaptcha_note <- function(scr, in_week, cut = .bot_cut) {
  ok <- !scr$screened_out; b <- ok & scr$bot
  why <- function(w) sum(b & scr$bot_why %in% w)
  parts <- c(if (why("score")) sprintf("%d with a reCAPTCHA score below %.1f", why("score"), cut),
             if (why("sus bots tab")) sprintf("%d on the tracker&rsquo;s &ldquo;sus bots&rdquo; tab", why("sus bots tab")),
             if (why("survey flag")) sprintf("%d flagged by the survey (no score saved)", why("survey flag")))
  no_score <- sum(ok & is.na(scr$recaptcha))
  sprintf(paste0('<p class="note"><b>Possible bots</b> = a reCAPTCHA score below %.1f, a screener ID on the tracker&rsquo;s ',
                 '&ldquo;sus bots&rdquo; tab, or the survey&rsquo;s own bot flag, whatever the answers and whether or not the ',
                 'screener was finished: %d this week, %d all time%s. They&rsquo;re left out of completed screeners and every %%.%s</p>'),
          cut, sum(b & in_week), sum(b), if (length(parts)) paste0(" (", paste(parts, collapse = "; "), ")") else "",
          if (all(is.na(scr$recaptcha[ok]))) " reCAPTCHA scores aren&rsquo;t in this export."
          else if (no_score) sprintf(" %d screeners have no score (from before scores were saved).", no_score) else "")
}

# "Local recruitment = Paper flyer, Mail, ..." for each group in the report
.groups_note <- function(groups, shown) {
  g <- intersect(unique(unname(groups)), shown)
  txt <- vapply(g, function(k) sprintf("<b>%s</b> = %s", .esc(k), .esc(paste(names(groups)[groups == k], collapse = ", "))), "")
  paste0('<p class="note">', if (length(txt)) paste0(paste(txt, collapse = "; "), " (a family who picked more than one of these counts once). ") else "",
         'All counts in this section are of <b>completed screeners only</b> (finished, not a possible bot).</p>')
}

.count_bars <- function(x, total) {
  x <- x[x > 0]
  if (!length(x)) return('<p class="note">None yet.</p>')
  paste(sprintf('<div class="bar wide"><span>%s</span><div class="track"><div style="width:%.1f%%"></div></div><span class="n">%d <span class="cnt">%.0f%%</span></span></div>',
                .esc(names(x)), 100 * x / max(x), as.integer(x), 100 * x / total), collapse = "")
}

recruitment_html <- function(scr, tr, fam, ses, steps, wk_tab, week, study_start, report_date, cs = NULL,
                             groups = character()) {
  css <- paste(readLines(file.path(.rth_recruit_home(), "recruit.css"), warn = FALSE), collapse = "\n")
  wl <- function(k) .week_label(k, study_start)
  sw <- rth_study_week(scr$date, study_start)
  in_week <- !is.na(sw) & sw == week
  ns <- !scr$screened_out
  this <- wk_tab[wk_tab$week == week, ]
  has <- attr(tr, "has")

  # Tracker checks ----
  checks <- .checks_html(tracker_checks(scr, tr, fam, steps, report_date))

  # This week ----
  prev <- wk_tab$screeners[wk_tab$week %in% (week - 4):(week - 1)]
  src_w <- source_counts(scr, in_week)
  top_w <- src_w[names(src_w) != "No source given"]
  wend <- .week_start(week, study_start) + 6
  upcoming <- sum(!is.na(fam$lab_date) & fam$lab_date > report_date)
  facts <- c(
    sprintf("Completed screeners: %d%s; all screeners, including unfinished ones and possible bots: %d; possible bots: %d",
            this$screeners,
            if (length(prev)) sprintf(" (average of previous %d weeks: %.0f)", length(prev), mean(prev)) else "",
            this$screeners_all, this$bots),
    sprintf("Eligible: %d; needs review (diagnosis reported): %d; needs review (possible repeat screener): %d", this$eligible,
            sum(in_week & scr$outcome == "Needs review: diagnosis reported"),
            sum(in_week & scr$outcome == "Needs review: possible repeat screener")),
    sprintf("Caregiver consents: %d", this$cg_consent),
    sprintf("Caregiver interviews: %d; caregiver surveys: %d", this$cg_interview, this$cg_survey),
    sprintf("Lab visits scheduled (booked this week): %s; lab visits done: %d; upcoming lab visits: %d",
            if (is.na(this$lab_sched)) "not tracked yet" else this$lab_sched, this$lab_done, upcoming),
    if (length(top_w)) sprintf("Top source: %s (%d of %d completed screeners)", names(top_w)[1], top_w[[1]], this$screeners)
    else "Top source: none reported")

  # All weeks (newest first; totals at the top) ----
  wt <- wk_tab[order(-wk_tab$week), ]
  na0 <- function(v) ifelse(is.na(v), "&ndash;", v)
  body <- vapply(seq_len(nrow(wt)), function(i) {
    r <- wt[i, ]
    paste0("<tr>", .wk_cell(r$week, wl), .td(r$screeners_all), .td(r$screeners), .td(r$eligible), .td(r$cg_consent),
           .td(r$cg_interview), .td(r$cg_survey), .td(r$lab_sched, na0(r$lab_sched)), .td(r$lab_done), "</tr>")
  }, "")
  tot <- c(sum(wt$screeners_all), sum(wt$screeners), sum(wt$eligible), sum(wt$cg_consent), sum(wt$cg_interview), sum(wt$cg_survey),
           sum(wt$lab_sched, na.rm = TRUE), sum(wt$lab_done))
  weekly <- .table("t-weekly", c("Week", "All screeners", "Completed screeners", "Eligible", "Caregiver consent", "Caregiver interview",
                                 "Caregiver survey", "Lab visits scheduled", "Lab visits done"), body,
                   paste0("<td>Total</td>", paste0("<td>", tot, "</td>", collapse = "")))

  # Screening outcomes and reasons ----
  out_w <- outcome_counts(scr, in_week); out_a <- outcome_counts(scr)
  rs_w <- reason_counts(scr, in_week); rs_a <- reason_counts(scr)
  bot <- .recaptcha_note(scr, in_week)

  # Conversion ----
  wc <- weekly_conversion(scr, steps, study_start, rev(wk_tab$week))
  convw <- .table("t-convw", c("Week screened", .conv_head),
                  vapply(seq_len(nrow(wc)), function(i) paste0("<tr>", .wk_cell(wc$week[i], wl), .conv_cells(wc[i, ]), "</tr>"), ""),
                  cls = "conv")
  sc <- source_conversion(scr, steps)
  totc <- attr(sc, "total")
  conv <- .table("t-conv", c("Source", .conv_head),
                 vapply(seq_len(nrow(sc)), function(i) paste0("<tr><td>", .esc(sc$source[i]), "</td>", .conv_cells(sc[i, ]), "</tr>"), ""),
                 paste0("<td>All screeners</td>", gsub("<td data-v=\"[^\"]*\">", "<td>", .conv_cells(totc))), cls = "conv")

  # Sources ----
  src_a <- source_counts(scr)
  top <- utils::head(setdiff(names(src_a), "No source given"), 5)
  chart <- if (length(top)) img_tag(plot_source_weeks(scr, sw, max(1, week - 11):week, top), height = 3, width = 9,
                                    alt = "Screeners by week and source") else '<p class="note">No sources reported yet.</p>'
  srcm <- attr(scr, "sources")[scr$completed, , drop = FALSE]; swn <- sw[scr$completed]
  vrow <- function(s) c(colSums(s[, top, drop = FALSE]), sum(s[, setdiff(colnames(s), top), drop = FALSE]), sum(rowSums(s) == 0))
  wks <- rev(sort(unique(stats::na.omit(swn[swn <= week]))))
  body <- vapply(wks, function(k) paste0("<tr>", .wk_cell(k, wl), paste(.td(vrow(srcm[!is.na(swn) & swn == k, , drop = FALSE])), collapse = ""), "</tr>"), "")
  src_tab <- .table("t-src", c("Week", top, "Remaining sources", "No source given"), body,
                    paste0("<td>Total</td>", paste0("<td>", vrow(srcm), "</td>", collapse = "")))
  wi <- attr(scr, "writein")
  wi <- wi[!scr$screened_out[wi$row], , drop = FALSE]
  writein <- if (nrow(wi)) {
    tt <- sort(table(paste0(wi$source, ": ", tolower(trimws(wi$text)))), decreasing = TRUE)
    sprintf('<details><summary>Write-ins for &ldquo;Paper flyer&rdquo; and &ldquo;Other&rdquo; (%d)</summary><ul class="wi">%s</ul></details>',
            nrow(wi), paste(sprintf("<li>%s (%d)</li>", .esc(names(tt)), as.integer(tt)), collapse = ""))
  } else ""

  # Daily assessments ----
  ct <- completion_table(tr, report_date)
  has_counts <- any(unlist(has[c("n_night", "n_morning", "n_food", "n_watch")]))
  daily <- if (!has_counts) {
    '<p class="note">The tracking sheet doesn&rsquo;t have the daily assessment count columns yet.</p>'
  } else if (!nrow(ct)) '<p class="note">No lab visits yet.</p>' else {
    summ <- function(v, lab) {
      v <- v[!is.na(v)]
      if (!length(v)) return(sprintf("<li>%s: not filled in yet</li>", lab))
      sprintf("<li>%s: median %s of 15 (range %s&ndash;%s); %d of %d (%.0f%%) did 12 or more, %d did all 15</li>",
              lab, format(stats::median(v)), min(v), max(v), sum(v >= 12), length(v), 100 * mean(v >= 12), sum(v >= 15))
    }
    li <- paste0(summ(ct$night, "Teen night diaries"), summ(ct$morning, "Teen morning diaries"),
                 summ(ct$food, "Food photos"), summ(ct$watch, "Watch nights worn"), summ(ct$cg_night, "Caregiver night diaries"))
    p <- plot_completion(ct)
    paste0(sprintf('<p class="note">Teens who did the lab visit: %d. Out of 15 days.</p><ul class="facts">%s</ul>', nrow(ct), li),
           if (is.null(p)) "" else img_tag(p, height = 3.4, width = 9, alt = "Daily assessment completion"))
  }

  # Demographics ----
  dm <- screener_demographics(scr)
  en <- enrolled_demographics(scr, tr, fam, ses, report_date)
  enrolled <- if (!en$n) '<p class="note">No families have done the lab visit yet.</p>' else paste0(
    sprintf('<p class="note">Families whose teen did the lab visit: %d. Race/ethnicity is from the teen&rsquo;s screener; income and education from the caregiver interview%s.</p>',
            en$n, if (en$has_ses) "" else " (no interview export given)"),
    '<div class="two">',
    sprintf('<div><h4>Teen age at lab visit</h4>%s<h4>Teen race/ethnicity</h4>%s</div>', .count_bars(en$age, en$n), .count_bars(en$race, en$n)),
    sprintf('<div><h4>Household income</h4>%s<h4>Highest education of either caregiver</h4>%s</div>',
            .count_bars(en$income, en$n), .count_bars(en$education, en$n)), '</div>')

  nav <- '<nav><b>Recruitment report</b>
<a href="#checks">Tracker checks</a>
<a href="#week">This week</a><a class="sub" href="#weekly">All weeks</a>
<a href="#status">Screening outcomes</a><a class="sub" href="#inel">Ineligible reasons</a>
<a href="#conv">Conversion</a><a class="sub" href="#conv-week">By week</a><a class="sub" href="#conv-src">By source</a>
<a href="#sources">Sources</a><a class="sub" href="#src-chart">By week (chart)</a><a class="sub" href="#src-week">By week (table)</a>
<a href="#demo">Demographics</a><a class="sub" href="#demo-scr">Screener</a><a class="sub" href="#demo-enr">Enrolled</a>
<a href="#daily">Daily assessments</a>'
  if (!is.null(cs)) nav <- paste0(nav, '
<a href="#costs">Costs</a><a class="sub" href="#cost-util">By source</a><a class="sub" href="#cost-weeks">By week</a><a class="sub" href="#cost-hours">Recruitment hours</a><a class="sub" href="#lm-week">Lab manager&rsquo;s week</a>')
  nav <- paste0(nav, '</nav>')

  paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    sprintf('<title>RTH recruitment, week %d</title><style>%s</style></head><body>', week, css), nav, '<main>',
    '<h1>Rise to Health recruitment report</h1>',
    sprintf('<p class="meta">Report run %s &middot; latest screener %s &middot; tracking sheet: %d people in %d families &middot; weeks run Saturday&ndash;Friday</p>',
            .long_date(report_date), if (any(!is.na(scr$date))) .long_date(max(scr$date, na.rm = TRUE)) else "none",
            sum(!is.na(tr$rth_id)), nrow(fam)),
    '<h2 id="checks">Tracker checks</h2>', checks,
    sprintf('<h2 id="week">This week (Week %d: %s)</h2><ul class="facts">%s</ul>', week, wl(week),
            paste0("<li>", facts, "</li>", collapse = "")),
    '<h3 id="weekly">All weeks</h3>', weekly,
    '<p class="note">Screeners and eligible are counted in the week of the screener; everything else in the week it happened (lab visits scheduled = the day it was booked). Screeners from blocked IPs/locations are not counted.</p>',
    '<h2 id="status">Screening outcomes</h2><div class="two">',
    sprintf('<div><h3>This week</h3>%s</div><div><h3>All time</h3>%s</div></div>', .bars(out_w), .bars(out_a)), bot,
    '<h3 id="inel">Ineligible reasons</h3><p class="note">A family can have more than one reason.</p><div class="two">',
    sprintf('<div><h4>This week</h4>%s</div><div><h4>All time</h4>%s</div></div>',
            .bars(rs_w[rs_w > 0], "No ineligible screeners this week."), .bars(rs_a[rs_a > 0], "None yet.")),
    '<h2 id="conv">Conversion</h2>',
    '<p class="note">Each cell: a %, then the number of families in small grey. All screeners includes unfinished ones and possible bots; completed screeners = finished and not a possible bot, and every % starts from them. Eligible is out of completed screeners and consent out of eligible; interview, survey and lab visit scheduled are out of caregiver consents; lab visit done is out of lab visits scheduled. A family with a later step counts as consented (and a lab visit as scheduled) even if that date is missing in the tracker. Families are linked through the screener ID in the tracking sheet.</p>',
    '<h3 id="conv-week">By week</h3>', convw,
    '<h3 id="conv-src">By source (all time)</h3>', conv,
    '<h2 id="sources">Sources</h2>', .groups_note(groups, colnames(attr(scr, "sources"))),
    sprintf('<div class="two"><div><h3>This week</h3>%s</div><div><h3>All time</h3>%s</div></div>',
            .bars(src_w, "No screeners this week."), .bars(src_a, "No screeners yet.")),
    '<h3 id="src-chart">Completed screeners by week and source (last 12 weeks)</h3>', chart,
    '<h3 id="src-week">Completed screeners by source, each week</h3>', src_tab, writein,
    '<h2 id="demo">Demographics</h2>',
    sprintf('<h3 id="demo-scr">Screener (all time)</h3><p class="note">Completed screeners only (finished, not a possible bot): %d eligible, %d not eligible. Bars show the %% of each group.</p>',
            dm$n_eligible, dm$n_not_eligible),
    '<div class="dl"><span><i class="el"></i>Eligible</span><span><i class="ne"></i>Not eligible</span></div><div class="three">',
    .demo_html("Child&rsquo;s age", dm$age), .demo_html("Race/ethnicity", dm$race), .demo_html("Household income", dm$income),
    '</div><h3 id="demo-enr">Enrolled (did the lab visit)</h3>', enrolled,
    '<h2 id="daily">Daily assessments</h2>', daily,
    if (!is.null(cs)) paste0('<h2 id="costs">Costs</h2>', cost_html(cs, week, wl, cs$lmh, study_start, cs$week_hours)),
    '</main><script>', .sort_js, '</script></body></html>')
}

.sort_js <- 'document.querySelectorAll("table.sortable").forEach(function(t){t.querySelectorAll("thead th").forEach(function(th,i){th.addEventListener("click",function(){
var asc=!th.classList.contains("asc");t.querySelectorAll("th").forEach(function(h){h.classList.remove("asc","desc")});th.classList.add(asc?"asc":"desc");
var b=t.tBodies[0],rs=Array.from(b.rows);var v=function(r){var c=r.cells[i],d=c.getAttribute("data-v");if(d!==null)return parseFloat(d);return c.innerText.trim().toLowerCase()};
rs.sort(function(a,c){var x=v(a),y=v(c);return(x>y?1:x<y?-1:0)*(asc?1:-1)});rs.forEach(function(r){b.appendChild(r)});});});});'

.rth_recruit_home <- function() {
  pkg <- tryCatch(utils::packageName(environment(.rth_recruit_home)), error = function(e) NULL)
  p <- if (is.null(pkg)) "" else system.file("rth_recruit", package = pkg)
  if (nzchar(p) && file.exists(file.path(p, "recruit.css"))) return(p)
  p <- getOption("rth_recruit.home")
  if (is.null(p)) stop("Can't find the recruitment report files: install the lsh package or run dev/load_dev.R")
  p
}
