# Summaries for the weekly recruitment report ----------------------------------

#' Read the weekly counts sheet used before this report (one row per week)
#'
#' Only "lab visits scheduled" is used, for weeks before the tracking sheet has
#' a "lab visit scheduled date" column filled in. Accepts .csv, .txt/.tsv
#' (tab) or .xlsx; the first column holds labels like "week 12 (7/12-7/18)".
#' @keywords internal
read_seed_sheet <- function(path) {
  ext <- tolower(tools::file_ext(path))
  d <- if (ext == "xlsx") openxlsx::read.xlsx(path, check.names = FALSE) else
    utils::read.delim(path, sep = if (ext == "csv") "," else "\t", check.names = FALSE,
                      colClasses = "character", quote = "\"")
  wk <- suppressWarnings(as.integer(sub("^\\s*week\\s*([0-9]+).*$", "\\1", as.character(d[[1]]), ignore.case = TRUE)))
  lc <- grep("lab visit", names(d), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(lc)) stop("Can't find a 'lab visits scheduled' column in ", basename(path), call. = FALSE)
  v <- suppressWarnings(as.integer(d[[lc]]))
  keep <- !is.na(wk) & grepl("^\\s*week", as.character(d[[1]]), ignore.case = TRUE) & !is.na(v)
  tibble::tibble(week = wk[keep], lab_visits_scheduled = v[keep])
}

# Weekly table --------------------------------------------------------------------

#' Counts per study week
#'
#' Screeners and eligible count in the week of the screener (blocked IPs left
#' out). Caregiver consent, interview, caregiver survey, lab visits scheduled
#' (date booked) and lab visits done count in the week they happened.
#' @keywords internal
weekly_counts <- function(scr, fam, study_start, last_week, seed = NULL) {
  wk <- function(d) rth_study_week(d, study_start)
  s <- scr[!scr$screened_out, , drop = FALSE]
  all_w <- c(wk(s$date), wk(fam$consent), wk(fam$interview), wk(fam$cg_survey), wk(fam$lab_sched),
             wk(fam$lab_date[fam$lab_done]), if (!is.null(seed)) seed$week)
  first <- max(1L, min(c(all_w[!is.na(all_w)], last_week)))
  w <- first:last_week
  cnt <- function(x) as.integer(table(factor(x, levels = w)))
  out <- tibble::tibble(
    week = w,
    screeners_all = cnt(wk(s$date)),
    bots = cnt(wk(s$date[s$bot])),
    screeners = cnt(wk(s$date[s$completed])),
    eligible = cnt(wk(s$date[s$eligible])),
    cg_consent = cnt(wk(fam$consent)),
    cg_interview = cnt(wk(fam$interview)),
    cg_survey = cnt(wk(fam$cg_survey)),
    lab_sched = cnt(wk(fam$lab_sched)),
    lab_done = cnt(wk(fam$lab_date[fam$lab_done])))
  # Before booking dates were tracked, use the old weekly sheet's numbers
  booked <- wk(fam$lab_sched)
  start <- if (any(!is.na(booked))) min(booked, na.rm = TRUE) else Inf
  if (!is.null(seed)) {
    old <- out$week < start & out$week %in% seed$week
    out$lab_sched[old] <- seed$lab_visits_scheduled[match(out$week[old], seed$week)]
  }
  out$lab_sched[out$week < start & !(out$week %in% (seed$week %||% integer()))] <- NA
  out
}

# Linking screeners to the tracker ----------------------------------------------------

#' For each screener: which later steps the family reached (via screener ID)
#' @keywords internal
link_steps <- function(scr, fam) {
  linked <- fam[!is.na(fam$screener_id), , drop = FALSE]
  # the typed ID can be the 6-digit screener ID or the Qualtrics Response ID
  m <- match(scr$screener_id, linked$screener_id)
  m2 <- match(scr$response_id, linked$screener_id)
  m[is.na(m)] <- m2[is.na(m)]
  has <- function(v) !is.na(m) & !is.na(linked[[v]][m])
  # Any later step means the family consented, and a lab visit means it was
  # scheduled, even if that date is missing in the tracker (missing consent
  # dates are listed in the checks). So no % can go over 100.
  lab <- !is.na(m) & linked$lab_done[m] %in% TRUE
  sched <- has("lab_sched") | has("lab_date") | lab
  survey <- has("cg_survey")
  interview <- has("interview")
  consent <- has("consent") | interview | survey | sched
  tibble::tibble(family = linked$family[m], consent = consent, interview = interview,
                 survey = survey, sched = sched, lab = lab)
}

# One conversion row: counts and %. Eligible is out of completed screeners
# (finished, not a possible bot), consent out of
# eligible; interview, survey and lab visit scheduled out of consents; lab visit
# done out of lab visits scheduled.
.conv_row <- function(sel, s, st) {
  sc <- sum(sel & s$completed); el <- sum(sel & s$eligible); cc <- sum(sel & st$consent)
  pc <- function(n, d) if (d) 100 * n / d else NA
  list(screeners_all = sum(sel), bots = sum(sel & s$bot), screeners = sc, eligible = el, pct_eligible = pc(el, sc),
       consent = cc, pct_consent = pc(cc, el),
       interview = sum(sel & st$interview), pct_interview = pc(sum(sel & st$interview), cc),
       survey = sum(sel & st$survey), pct_survey = pc(sum(sel & st$survey), cc),
       sched = sum(sel & st$sched), pct_sched = pc(sum(sel & st$sched), cc),
       lab = sum(sel & st$lab), pct_lab = pc(sum(sel & st$lab), sum(sel & st$sched)))
}

#' Conversion by source, all time
#' @keywords internal
source_conversion <- function(scr, steps) {
  keep <- !scr$screened_out
  src <- attr(scr, "sources")[keep, , drop = FALSE]
  s <- scr[keep, , drop = FALSE]; st <- steps[keep, , drop = FALSE]
  rows <- lapply(colnames(src), function(k) c(list(source = k), .conv_row(src[, k], s, st)))
  none <- rowSums(src) == 0
  if (any(none)) rows <- c(rows, list(c(list(source = "No source given"), .conv_row(none, s, st))))
  out <- dplyr::bind_rows(rows)
  if (nrow(out)) out <- out[out$screeners_all > 0, , drop = FALSE]      # choices nobody picked
  if (nrow(out)) out <- out[order(-out$screeners), , drop = FALSE]
  attr(out, "total") <- .conv_row(rep(TRUE, nrow(s)), s, st)
  out
}

#' Conversion by week: families grouped by the week they did the screener
#' @keywords internal
weekly_conversion <- function(scr, steps, study_start, weeks) {
  keep <- !scr$screened_out
  s <- scr[keep, , drop = FALSE]; st <- steps[keep, , drop = FALSE]
  w <- rth_study_week(s$date, study_start)
  dplyr::bind_rows(lapply(weeks, function(k) c(list(week = k), .conv_row(!is.na(w) & w == k, s, st))))
}

# Tracker checks ------------------------------------------------------------------------

#' Things to follow up on in the tracking sheet
#'
#' Each check returns a short label, a count, and the IDs involved (RTH IDs or
#' screener IDs; no names).
#' @keywords internal
tracker_checks <- function(scr, tr, fam, steps, report_date, wait_days = 14, diary_days = 21) {
  chk <- list()
  add <- function(label, ids, help = "") chk[[length(chk) + 1]] <<- list(label = label, n = length(ids), ids = ids, help = help)
  has <- attr(tr, "has")
  fid <- function(f) unique(tr$rth_id[tr$family %in% f])

  # IDs
  add("RTH ID cells that aren't a single 4-digit ID ending in 1 or 2",
      sprintf("row %d: %s", tr$sheet_row[is.na(tr$rth_id) | is.na(tr$role)], tr$rth_id_raw[is.na(tr$rth_id) | is.na(tr$role)]),
      "These rows are left out until fixed.")
  ok <- !is.na(tr$rth_id)
  add("RTH IDs listed more than once", unique(tr$rth_id[ok][duplicated(tr$rth_id[ok])]))
  sheet_role <- ifelse(grepl("adol|teen", tr$role_sheet), "teen", ifelse(grepl("care|parent", tr$role_sheet), "caregiver", NA))
  add("Role column doesn't match the RTH ID's last digit (1 = teen, 2 = caregiver)",
      tr$rth_id[ok & !is.na(sheet_role) & !is.na(tr$role) & sheet_role != tr$role])

  # Screener IDs
  if (isTRUE(has[["screener_id"]])) {
    add("Families with no screener ID in the tracker", fid(fam$family[is.na(fam$screener_id)]),
        "Without it, the family can't be linked to a source.")
    known <- c(scr$screener_id, scr$response_id)
    add("Screener IDs in the tracker that aren't in the screener export",
        sprintf("%s (RTH %s)", fam$screener_id, vapply(fam$family, function(f) paste(fid(f), collapse = "/"), ""))[
          !is.na(fam$screener_id) & !fam$screener_id %in% known],
        "Usually a typo.")
  } else {
    add("The tracker has no 'screener ID' column yet", "add the column", "Sources can't be linked to consents and lab visits until it's added.")
  }

  # Follow-ups
  # recent enough to follow up: screened 2-8 weeks ago (newest first)
  old_el <- scr$eligible & !scr$screened_out & !is.na(scr$date) & is.na(steps$family) &
    scr$date <= report_date - wait_days & scr$date > report_date - 56
  o <- which(old_el)[order(scr$date[old_el], decreasing = TRUE)]
  add("Eligible screeners from 2 to 8 weeks ago with no family in the tracker",
      sprintf("%s (screened %s)", scr$screener_id[o], format(scr$date[o], "%m/%d")),
      "Not necessarily a problem (they may not have signed up yet), but worth checking they were contacted.")
  add(sprintf("Caregiver consented %d+ days ago, no interview date", wait_days),
      fid(fam$family[!is.na(fam$consent) & fam$consent <= report_date - wait_days & is.na(fam$interview)]))
  fut <- (!is.na(fam$consent) & fam$consent > report_date) | (!is.na(fam$interview) & fam$interview > report_date) |
    (!is.na(fam$cg_survey) & fam$cg_survey > report_date)
  add("Consent, interview or caregiver survey date is in the future", fid(fam$family[fut]),
      "Usually a typo (future lab visit dates are fine).")
  later <- !is.na(fam$interview) | !is.na(fam$cg_survey) | !is.na(fam$lab_sched) | !is.na(fam$lab_date)
  add("Interview, survey or lab visit date but no consent date", fid(fam$family[later & is.na(fam$consent)]),
      "The report counts them as consented anyway; please fill in the consent date.")
  add("Interview date is before the consent date",
      fid(fam$family[!is.na(fam$consent) & !is.na(fam$interview) & fam$interview < fam$consent]))
  if (isTRUE(has[["lab_sched_date"]]) && any(!is.na(fam$lab_sched))) {
    # only visits since booking dates started being logged
    since <- min(fam$lab_sched, na.rm = TRUE)
    add("Lab visit date but no 'lab visit scheduled date'",
        fid(fam$family[!is.na(fam$lab_date) & fam$lab_date >= since & is.na(fam$lab_sched)]))
  }
  teen <- tr[tr$role %in% "teen" & !is.na(tr$lab_date) & tr$lab_date <= report_date - diary_days, , drop = FALSE]
  cols <- intersect(c("n_night", "n_morning", "n_food", "n_watch"), names(tr))
  if (any(unlist(has[c("n_night", "n_morning", "n_food", "n_watch")]))) {
    miss <- teen$rth_id[rowSums(is.na(as.matrix(teen[, cols, drop = FALSE]))) > 0]
    add(sprintf("Lab visit %d+ days ago, daily assessment counts not filled in", diary_days), miss)
  }
  chk
}

# Daily assessment completion -------------------------------------------------------------

#' Completion counts for teens who did the lab visit (and their caregiver's night diaries)
#' @keywords internal
completion_table <- function(tr, report_date) {
  teen <- tr[tr$role %in% "teen" & !is.na(tr$lab_date) & tr$lab_date <= report_date, , drop = FALSE]
  cg <- tr[tr$role %in% "caregiver", , drop = FALSE]
  tibble::tibble(rth_id = teen$rth_id, lab_date = teen$lab_date,
                 night = teen$n_night, morning = teen$n_morning, food = teen$n_food, watch = teen$n_watch,
                 cg_night = cg$n_night[match(teen$family, cg$family)])
}

# Outcomes, reasons, sources, demographics ---------------------------------------------

.outcome_levels <- c("Eligible (child 14-17)", "Eligible (child 18)", "Not marked eligible, but enrolled",
                     "Needs review: diagnosis reported", "Needs review: possible repeat screener",
                     "Possible bot (low reCAPTCHA score)",
                     "Incomplete answers", "Ineligible", "Didn't finish screener",
                     "No status recorded", "Screened out (blocked IP/location)")

outcome_counts <- function(scr, sel = rep(TRUE, nrow(scr))) {
  t <- table(factor(scr$outcome[sel], levels = .outcome_levels))
  always <- c("Eligible (child 14-17)", "Eligible (child 18)", "Needs review: diagnosis reported",
              "Possible bot (low reCAPTCHA score)", "Incomplete answers", "Ineligible")
  t <- t[t > 0 | names(t) %in% always]
  stats::setNames(as.integer(t), names(t))
}

reason_counts <- function(scr, sel = rep(TRUE, nrow(scr))) {
  r <- attr(scr, "reasons")[sel, , drop = FALSE]
  x <- colSums(r)
  x <- x[x > 0 | colnames(r) != "Other / not recorded"]
  sort(x, decreasing = TRUE)
}

source_counts <- function(scr, sel = rep(TRUE, nrow(scr))) {
  sel <- sel & !scr$screened_out
  src <- attr(scr, "sources")[sel, , drop = FALSE]
  x <- colSums(src)
  none <- sum(rowSums(src) == 0)
  if (none) x <- c(x, `No source given` = none)
  sort(x[x > 0], decreasing = TRUE)
}

.race_levels <- c("Monoracial White", "Monoracial Black", "Monoracial Asian", "Monoracial Hispanic/Latino",
                  "Monoracial American Indian/Alaska Native", "Monoracial Native Hawaiian/Pacific Islander",
                  "Monoracial Other", "Multiracial Black", "Multiracial Hispanic/Latino", "Multiracial Asian",
                  "Multiracial, another race", "Not answered")
.inc_screener <- c("Less than $5,000", "$5,000 to $19,999", "$20,000 to $29,999", "$30,000 to $39,999",
                   "$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $89,999",
                   "$90,000 or higher")

.age_group <- function(age) {
  a <- suppressWarnings(as.integer(age))
  out <- ifelse(is.na(age), "Not answered", ifelse(is.na(a) | a > 18, "Over 18", ifelse(a < 14, "Under 14", as.character(a))))
  factor(out, levels = c("Under 14", as.character(14:18), "Over 18", "Not answered"))
}

#' Screener demographics: % of eligible and % of not-eligible screeners per group
#'
#' Among completed screeners (finished, not blocked, not a possible bot).
#' @keywords internal
screener_demographics <- function(scr) {
  base <- scr$completed
  el <- base & scr$eligible; ne <- base & !scr$eligible
  tab <- function(x, lv) {
    x <- factor(x, levels = lv)
    tibble::tibble(group = lv,
                   eligible = if (sum(el)) 100 * as.numeric(table(x[el])) / sum(el) else NA_real_,
                   not_eligible = if (sum(ne)) 100 * as.numeric(table(x[ne])) / sum(ne) else NA_real_)
  }
  inc <- ifelse(is.na(scr$income), "Not answered", scr$income)
  list(n_eligible = sum(el), n_not_eligible = sum(ne),
       age = tab(as.character(.age_group(scr$age)), levels(.age_group(NA))),
       race = tab(scr$race_group, .race_levels),
       income = tab(inc, c(union(.inc_screener, unique(stats::na.omit(scr$income))), "Not answered")))
}

#' Enrolled demographics: families whose teen did the lab visit
#'
#' Teen age at the lab visit (tracker), teen race/ethnicity (screener, via the
#' screener ID), and from the caregiver interview: household income and the
#' highest education of either caregiver.
#' @keywords internal
enrolled_demographics <- function(scr, tr, fam, ses, report_date) {
  done <- fam[fam$lab_done, , drop = FALSE]
  teen <- tr[tr$role %in% "teen", , drop = FALSE]
  age <- teen$age_lab[match(done$family, teen$family)]
  m <- match(done$screener_id, scr$screener_id); m2 <- match(done$screener_id, scr$response_id)
  m[is.na(m)] <- m2[is.na(m)]
  race <- ifelse(is.na(m), "No screener linked", scr$race_group[m])
  inc_lv <- c("Less than $20,000", "$20,000 to $39,999", "$40,000 to $59,999", "$60,000 to $79,999",
              "$80,000 to $99,999", "$100,000 or higher", "Not answered")
  s <- if (is.null(ses)) NULL else ses[match(done$family, ses$family), , drop = FALSE]
  inc <- if (is.null(s)) rep(NA_real_, nrow(done)) else s$income
  inc_g <- ifelse(is.na(inc), "Not answered",
                  inc_lv[findInterval(inc, c(0, 20000, 40000, 60000, 80000, 1e5))])
  edu <- if (is.null(s)) rep(NA_character_, nrow(done)) else s$education
  cnt <- function(x, lv) { x <- factor(x, levels = lv); stats::setNames(as.integer(table(x)), lv) }
  agel <- c(as.character(13:19), "Not recorded")
  list(n = nrow(done), has_ses = !is.null(ses),
       age = cnt(ifelse(is.na(age), "Not recorded", as.character(round(age))), agel),
       race = cnt(race, c(.race_levels, "No screener linked")),
       income = cnt(inc_g, inc_lv),
       education = cnt(ifelse(is.na(edu), "Not answered", edu), c(rev(.edu_order), "Not answered")))
}
