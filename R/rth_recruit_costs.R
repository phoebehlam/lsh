# Recruitment costs ------------------------------------------------------------------
#
# The "costs" tab of the RTH recruitment costs & hours sheet: one row per
# purchase (type = Expense) or block of recruitment time (type = Time).
# Staff time is costed with hourly rates set in render_rth_recruitment(), so
# pay rates never live in the sheet.

.cost_cols <- c(date = "date", type = "type", person = "person", source = "source",
                mode = "in person or remote", hours = "hours", amount = "amount",
                what = "what / item", notes = "notes")

#' Read the costs tab
#'
#' @param costs Google Sheets link, downloaded .xlsx, or data frame.
#' @param sheet Tab name.
#' @param hourly_rates Named hourly cost per person (names as in the person column).
#' @return One row per expense or time entry, with \code{cost} in dollars
#'   (Expense: the amount; Time: hours x the person's rate, or the amount if the
#'   person has no rate; NA if neither). Undated rows (totals from before weekly
#'   tracking) count toward all-time totals only. Stops with a list of cells to
#'   fix if a date or number can't be read.
#' @keywords internal
read_costs <- function(costs, sheet = "costs", hourly_rates) {
  raw <- .flatten_sheet(.tracker_raw(costs, sheet, must_have_tab = TRUE))
  k <- .key(names(raw))
  miss <- .cost_cols[c("source", "hours", "amount")][is.na(match(.key(.cost_cols[c("source", "hours", "amount")]), k))]
  if (length(miss)) stop("The costs sheet is missing these columns: ", paste(miss, collapse = ", "), call. = FALSE)
  pick <- function(nm) { j <- match(.key(.cost_cols[[nm]]), k); if (is.na(j)) rep(NA_character_, nrow(raw)) else trimws(raw[[j]]) }
  d <- as.data.frame(stats::setNames(lapply(names(.cost_cols), pick), names(.cost_cols)), stringsAsFactors = FALSE)
  d$sheet_row <- seq_len(nrow(raw)) + 1
  d <- d[!(.is_blank(d$type) & .is_blank(d$source) & .is_blank(d$hours) & .is_blank(d$amount)), , drop = FALSE]

  bad <- character()
  cell <- function(col, i, v) sprintf('  "%s", row %d: "%s"', .cost_cols[[col]], d$sheet_row[i], v)
  date <- .parse_slash_date(d$date)
  w <- which(is.na(date) & !.is_blank(d$date)); if (length(w)) bad <- c(bad, cell("date", w, d$date[w]))
  num <- function(col) {                     # commas only as thousands separators in amounts
    v <- d[[col]]
    x <- suppressWarnings(as.numeric(gsub(if (col == "amount") "[$,[:space:]]" else "[[:space:]]", "", v)))
    if (col == "amount") x[grepl(",[0-9]{1,2}$|,[0-9]{4}", v)] <- NA
    w <- which(is.na(x) & !.is_blank(v)); if (length(w)) bad <<- c(bad, cell(col, w, v[w]))
    x
  }
  hours <- num("hours"); amount <- num("amount")
  type <- tolower(d$type)
  type <- ifelse(.is_blank(d$type), ifelse(!is.na(hours) & is.na(amount), "time", "expense"), type)
  w <- which(!type %in% c("time", "expense")); if (length(w)) bad <- c(bad, cell("type", w, d$type[w]))
  if (length(bad)) stop("Please fix these cells in the costs sheet (dates like 11/2/2026, numbers like 350 or 1.5, type Expense or Time):\n",
                        paste(utils::head(bad, 30), collapse = "\n"),
                        if (length(bad) > 30) sprintf("\n  ...and %d more", length(bad) - 30) else "", call. = FALSE)

  is_time <- type == "time"
  rate <- unname(hourly_rates[match(.key(d$person), .key(names(hourly_rates)))])
  person <- ifelse(is.na(rate), d$person, names(hourly_rates)[match(.key(d$person), .key(names(hourly_rates)))])
  cost <- ifelse(!is_time, ifelse(is.na(amount), 0, amount),
                 ifelse(!is.na(rate) & !is.na(hours), hours * rate, amount))
  mode <- ifelse(grepl("person", tolower(d$mode)), "In person", ifelse(grepl("remote", tolower(d$mode)), "Remote", NA))
  tibble::tibble(sheet_row = d$sheet_row, date = date, type = ifelse(is_time, "Time", "Expense"),
                 person = ifelse(.is_blank(person), NA, person), source = ifelse(.is_blank(d$source), NA, d$source),
                 mode = mode, hours = ifelse(is_time, hours, NA), amount = amount, cost = cost,
                 what = ifelse(.is_blank(d$what), NA, d$what))
}

#' Cost summaries: by source (with fractional credit), by week, hours this week
#'
#' A screener who picked several sources gives each an equal share (2 sources:
#' half each). General costs, and costs whose source isn't a screener choice,
#' count only in the all-strategies total, which is divided by every screener
#' (including those who gave no source).
#' @keywords internal
cost_summary <- function(cst, scr, steps, week, study_start) {
  keep <- !scr$screened_out
  src <- attr(scr, "sources")[keep, , drop = FALSE]
  s <- scr[keep, , drop = FALSE]; st <- steps[keep, , drop = FALSE]
  share <- if (ncol(src)) src / pmax(rowSums(src), 1) else src
  credit <- function(sel) colSums(share * sel)
  cr <- cbind(screeners = credit(s$completed), eligible = credit(s$eligible), consent = credit(st$consent), lab = credit(st$lab))

  cst$week <- rth_study_week(cst$date, study_start)
  cst <- cst[is.na(cst$week) | cst$week <= week, , drop = FALSE]        # nothing after the report week
  j <- match(.key(cst$source), .key(colnames(src)))
  cst$matched <- ifelse(is.na(j), NA, colnames(src)[j])
  general <- .key(cst$source) %in% "general"

  by_src <- do.call(rbind, lapply(sort(unique(stats::na.omit(cst$matched))), function(k) {
    x <- cst[cst$matched %in% k, , drop = FALSE]
    data.frame(source = k, money = sum(x$cost[x$type == "Expense"], na.rm = TRUE),
               hours = sum(x$hours, na.rm = TRUE), labor = sum(x$cost[x$type == "Time"], na.rm = TRUE),
               cost = sum(x$cost, na.rm = TRUE), t(cr[k, ]), stringsAsFactors = FALSE)
  }))
  if (is.null(by_src)) by_src <- data.frame(source = character(), money = numeric(), hours = numeric(), labor = numeric(),
                                            cost = numeric(), screeners = numeric(), eligible = numeric(),
                                            consent = numeric(), lab = numeric())
  if (nrow(by_src)) by_src <- by_src[order(-by_src$cost), , drop = FALSE]
  tot <- data.frame(source = "All strategies", money = sum(cst$cost[cst$type == "Expense"], na.rm = TRUE),
                    hours = sum(cst$hours, na.rm = TRUE), labor = sum(cst$cost[cst$type == "Time"], na.rm = TRUE),
                    cost = sum(cst$cost, na.rm = TRUE), screeners = sum(s$completed), eligible = sum(s$eligible),
                    consent = sum(st$consent), lab = sum(st$lab))

  # by week (dated rows), newest first; undated rows = before weekly tracking
  sw <- rth_study_week(s$date, study_start)
  wks <- sort(unique(stats::na.omit(cst$week)), decreasing = TRUE)
  wrow <- function(x, n) data.frame(money = sum(x$cost[x$type == "Expense"], na.rm = TRUE), hours = sum(x$hours, na.rm = TRUE),
                                    labor = sum(x$cost[x$type == "Time"], na.rm = TRUE), cost = sum(x$cost, na.rm = TRUE),
                                    screeners = n)
  by_week <- do.call(rbind, c(list(data.frame(week = integer(), money = numeric(), hours = numeric(), labor = numeric(),
                                              cost = numeric(), screeners = integer())),
    lapply(wks, function(k) cbind(week = k, wrow(cst[cst$week %in% k, ], sum(!is.na(sw) & sw == k & s$completed))))))
  before <- cst[is.na(cst$week), , drop = FALSE]

  this <- cst[cst$week %in% week & cst$type == "Time", , drop = FALSE]
  hrs <- if (nrow(this)) {
    g <- interaction(ifelse(is.na(this$person), "(no person)", this$person), ifelse(is.na(this$source), "(no source)", this$source),
                     drop = TRUE, lex.order = TRUE, sep = "\r")
    do.call(rbind, lapply(split(this, g), function(x) data.frame(
      person = ifelse(is.na(x$person[1]), "(no person)", x$person[1]), source = ifelse(is.na(x$source[1]), "(no source)", x$source[1]),
      in_person = sum(x$hours[x$mode %in% "In person"], na.rm = TRUE), remote = sum(x$hours[x$mode %in% "Remote"], na.rm = TRUE),
      unmarked = sum(x$hours[is.na(x$mode)], na.rm = TRUE), hours = sum(x$hours, na.rm = TRUE),
      what = paste(unique(stats::na.omit(x$what)), collapse = "; "), stringsAsFactors = FALSE)))
  } else NULL

  flags <- list(
    unmatched = unique(cst$source[!is.na(cst$source) & is.na(cst$matched) & !general]),
    no_source = cst$sheet_row[is.na(cst$source)],
    not_costed = cst$sheet_row[is.na(cst$cost)],
    no_mode = cst$sheet_row[cst$type == "Time" & is.na(cst$mode) & !is.na(cst$week)],
    no_person = cst$sheet_row[cst$type == "Time" & is.na(cst$person)],
    free = setdiff(colnames(src)[colSums(src) > 0], by_src$source))
  list(by_src = by_src, total = tot, by_week = by_week, before = before, week_rows = cst[cst$week %in% week, , drop = FALSE],
       hours_week = hrs, flags = flags, all = cst)
}

# HTML ---------------------------------------------------------------------------------

.money <- function(x, digits = 2) ifelse(is.na(x), "&ndash;", paste0("$", formatC(x, format = "f", digits = digits, big.mark = ",")))
.hrs <- function(h) ifelse(is.na(h), "&ndash;", sub("\\.$", "", sub("0+$", "", formatC(round(h, 2), format = "f", digits = 2))))
.cred <- function(x) ifelse(abs(x - round(x)) < 1e-9, sprintf("%d", as.integer(round(x))), sprintf("%.1f", x))

.util_cols <- c(screeners = "completed screener", eligible = "eligible family", consent = "caregiver consent", lab = "lab visit")
.thin_n <- 3

# Cost per step for one row; class "best" / "thin" set by the caller
.per_td <- function(cost, n, cls = "") {
  if (n <= 0) return(sprintf('<td data-v="999999999" class="%s">&ndash;<span class="cnt">0</span></td>', cls))
  sprintf('<td data-v="%.4f" class="%s">%s<span class="cnt">%s</span></td>', cost / n, cls, .money(cost / n), .cred(n))
}

cost_html <- function(cs, week, wl, lmh = NULL, study_start, week_hours = 37.5) {
  bs <- cs$by_src; tt <- cs$total
  # cheapest per column, among sources with at least 3 families' worth of credit
  best <- vapply(names(.util_cols), function(v) {
    ok <- bs[[v]] >= .thin_n & bs$cost > 0
    if (!any(ok)) return(NA_character_)
    r <- bs[ok, , drop = FALSE]; r$source[which.min(r$cost / r[[v]])]
  }, "")
  body <- vapply(seq_len(nrow(bs)), function(i) {
    r <- bs[i, ]
    cells <- vapply(names(.util_cols), function(v) {
      cls <- if (r[[v]] < .thin_n) "thin" else if (identical(best[[v]], r$source)) "best" else ""
      .per_td(r$cost, r[[v]], cls)
    }, "")
    paste0("<tr><td>", .esc(r$source), "</td>",
           sprintf('<td data-v="%.2f" title="ads/materials %s + staff time %s (%s hr)">%s</td>', r$cost,
                   .money(r$money), .money(r$labor), .hrs(r$hours), .money(r$cost)),
           paste(cells, collapse = ""), "</tr>")
  }, "")
  foot <- paste0("<td>All strategies</td>",
                 sprintf('<td title="ads/materials %s + staff time %s (%s hr)">%s</td>', .money(tt$money), .money(tt$labor),
                         .hrs(tt$hours), .money(tt$cost)),
                 paste(vapply(names(.util_cols), function(v) gsub(' data-v="[^"]*"', "", .per_td(tt$cost, tt[[v]])), ""), collapse = ""))
  util <- if (nrow(bs)) .table("t-util", c("Source", "Total cost", paste("$ per", .util_cols)), body, foot, cls = "conv util")
          else '<p class="note">No costs tied to a screener source yet.</p>'

  # this week / all time
  wr <- cs$week_rows
  w_money <- sum(wr$cost[wr$type == "Expense"], na.rm = TRUE); w_labor <- sum(wr$cost[wr$type == "Time"], na.rm = TRUE)
  w_hours <- sum(wr$hours, na.rm = TRUE)
  b <- cs$before
  hw <- cs$hours_week
  hp <- if (!is.null(hw)) tapply(hw$hours, hw$person, sum) else numeric()
  per <- function(v) sprintf("%s per %s", .money(if (tt[[v]] > 0) tt$cost / tt[[v]] else NA), .util_cols[[v]])
  cheap <- vapply(names(.util_cols), function(v) if (is.na(best[[v]])) sprintf("per %s: not enough families yet", .util_cols[[v]]) else {
    r <- bs[bs$source == best[[v]], ]; sprintf("per %s: %s (%s)", .util_cols[[v]], .esc(best[[v]]), .money(r$cost / r[[v]])) }, "")
  facts <- c(
    if (nrow(wr)) sprintf("This week: %s (%s on ads, materials and treats; %s of staff time, %s recruitment hours)",
                          .money(w_money + w_labor), .money(w_money), .money(w_labor), .hrs(w_hours))
    else "This week: nothing logged in the costs sheet yet",
    if (length(hp)) sprintf("Recruitment hours this week: %s", paste(sprintf("%s %s", names(hp), .hrs(hp)), collapse = "; ")),
    sprintf("All time: %s (%s on ads, materials and treats; %s of staff time, %s hours)%s", .money(tt$cost), .money(tt$money),
            .money(tt$labor), .hrs(tt$hours),
            if (nrow(b)) sprintf(", including %s logged before weekly tracking", .money(sum(b$cost, na.rm = TRUE))) else ""),
    sprintf("All strategies, all time: %s", paste(vapply(names(.util_cols), per, ""), collapse = "; ")),
    sprintf("Cheapest so far: %s", paste(cheap, collapse = "; ")))

  # by week
  bw <- cs$by_week
  wbody <- c(vapply(seq_len(nrow(bw)), function(i) {
    r <- bw[i, ]
    paste0("<tr>", .wk_cell(r$week, wl), .td(round(r$money, 2), .money(r$money)), .td(r$hours, .hrs(r$hours)),
           .td(round(r$labor, 2), .money(r$labor)), .td(round(r$cost, 2), .money(r$cost)), .td(r$screeners),
           .td(if (r$screeners) round(r$cost / r$screeners, 2) else NA, .money(if (r$screeners) r$cost / r$screeners else NA)), "</tr>")
  }, ""), if (nrow(b)) paste0('<tr><td data-v="0">Before weekly tracking</td>',
    .td(round(sum(b$cost[b$type == "Expense"], na.rm = TRUE), 2), .money(sum(b$cost[b$type == "Expense"], na.rm = TRUE))),
    .td(sum(b$hours, na.rm = TRUE), .hrs(sum(b$hours, na.rm = TRUE))),
    .td(round(sum(b$cost[b$type == "Time"], na.rm = TRUE), 2), .money(sum(b$cost[b$type == "Time"], na.rm = TRUE))),
    .td(round(sum(b$cost, na.rm = TRUE), 2), .money(sum(b$cost, na.rm = TRUE))), "<td>&ndash;</td><td>&ndash;</td></tr>"))
  weeks_tab <- .table("t-costweek", c("Week", "Ads, materials &amp; treats", "Staff hours", "Staff time", "Total",
                                      "Completed screeners that week", "$ per completed screener that week"), wbody,
                      paste0("<td>Total</td><td>", .money(tt$money), "</td><td>", .hrs(tt$hours), "</td><td>", .money(tt$labor),
                             "</td><td>", .money(tt$cost), "</td><td>&ndash;</td><td>&ndash;</td>"))

  f <- cs$flags
  chk <- list(
    list(label = "Sources in the costs sheet that aren't a screener choice", n = length(f$unmatched), ids = f$unmatched,
         help = "Counted only in All strategies. Spell them exactly like the screener's \"Where did you hear about us?\" choices (or use General)."),
    list(label = "Rows with no source", n = length(f$no_source), ids = paste("row", f$no_source),
         help = "Counted only in All strategies."),
    list(label = "Time rows that couldn't be costed (no person with an hourly rate, and no amount)", n = length(f$not_costed),
         ids = paste("row", f$not_costed), help = "Pick Lab manager or Grad student, or put what the time cost in amount."),
    list(label = "Time rows without in person / remote", n = length(f$no_mode), ids = paste("row", f$no_mode),
         help = "Needed for the lab manager hours tab."))
  free <- if (length(f$free)) sprintf('<p class="note">Sources with screeners but no costs logged (not in the table): %s.</p>',
                                      paste(.esc(f$free), collapse = ", ")) else ""

  paste0('<ul class="facts">', paste0("<li>", facts, "</li>", collapse = ""), '</ul>',
         '<h3 id="cost-checks">Costs sheet checks</h3>', .checks_html(chk),
         '<h3 id="cost-util">Cost per step, by source (all time)</h3>',
         '<p class="note">Total cost = ads, materials and treats + staff time spent on recruitment (hover for the split). ',
         'Each cell: cost per family reaching that step, then the number of families in small grey. A family that picked 2 sources counts as half for each, ',
         'so counts can have decimals. General costs are only in All strategies, which is divided by every completed screener. ',
         sprintf('<span class="best-key">Green</span> = cheapest; <span class="thin-key">grey</span> = fewer than %d families, too few to compare. ', .thin_n),
         'Later steps lag: recent screeners haven&rsquo;t had time to reach a lab visit yet.</p>',
         util, free,
         '<h3 id="cost-weeks">By week</h3>', weeks_tab,
         '<p class="note">Costs are counted in the week of the date in the sheet. &ldquo;$ per completed screener that week&rdquo; is only a rough guide, since ads and flyers keep bringing in screeners after the week they were paid for.</p>',
         sprintf('<h3 id="cost-hours">Lab manager&rsquo;s recruitment hours this week (Week %d: %s)</h3>', week, wl(week)),
         lm_rec_html(cs, week),
         sprintf('<h3 id="lm-week">Lab manager&rsquo;s week (Week %d: %s)</h3>', week, wl(week)),
         lm_week_html(cs, lmh, week, study_start, week_hours))
}

# Lab manager hours ---------------------------------------------------------------------

#' Read the "lab manager hours" tab: one row per day, one column per kind of work
#'
#' The recruitment columns are skipped (they're formulas copying the costs
#' tab; recruitment hours are taken from the costs tab directly), as are day,
#' total and notes.
#' @return Long table: date, category, hours (categories in sheet order, as a
#'   factor); notes as attribute "notes". NULL if the tab doesn't exist.
#' @keywords internal
read_lm_hours <- function(costs, sheet = "lab manager hours") {
  raw <- tryCatch(.flatten_sheet(.tracker_raw(costs, sheet, must_have_tab = TRUE)), error = function(e) {
    if (grepl("No '.*' tab|not found|Can't find", conditionMessage(e))) NULL else stop(e) })
  if (is.null(raw) || !nrow(raw)) return(NULL)
  k <- .key(names(raw))
  jd <- match("date", k)
  if (is.na(jd)) stop("The '", sheet, "' tab needs a 'date' column.", call. = FALSE)
  skip <- k %in% c("date", "day", "total", "notes") | grepl("recruit", k) | k == ""
  cats <- names(raw)[!skip]
  date <- .parse_slash_date(raw[[jd]])
  row <- seq_len(nrow(raw)) + 1
  bad <- character()
  w <- which(is.na(date) & !.is_blank(raw[[jd]])); if (length(w)) bad <- sprintf('  "date", row %d: "%s"', row[w], raw[[jd]][w])
  long <- do.call(rbind, c(list(data.frame(date = as.Date(character()), category = character(), hours = numeric())),
    lapply(cats, function(cn) {
      v <- trimws(raw[[cn]]); x <- suppressWarnings(as.numeric(v))
      w <- which(is.na(x) & !.is_blank(v)); if (length(w)) bad <<- c(bad, sprintf('  "%s", row %d: "%s"', cn, row[w], v[w]))
      keep <- !is.na(x) & x != 0 & !is.na(date)
      data.frame(date = date[keep], category = rep(cn, sum(keep)), hours = x[keep])
    })))
  if (length(bad)) stop(sprintf("Please fix these cells in the '%s' tab (hours as numbers like 1.5, dates like 11/2/2026):\n", sheet),
                        paste(utils::head(bad, 30), collapse = "\n"),
                        if (length(bad) > 30) sprintf("\n  ...and %d more", length(bad) - 30) else "", call. = FALSE)
  long$category <- factor(long$category, levels = cats)
  jn <- match("notes", k)
  attr(long, "notes") <- if (is.na(jn)) NULL else {
    n <- trimws(raw[[jn]]); keep <- !.is_blank(n) & !is.na(date)
    data.frame(date = date[keep], note = n[keep], stringsAsFactors = FALSE)
  }
  long
}

# Okabe-Ito blues for recruitment; softer colors for everything else
.rec_cols <- c("Recruitment, in person" = "#0072B2", "Recruitment, remote" = "#56B4E9", "Recruitment, not marked" = "#9CC3E0")
.work_cols <- c("#E69F00", "#009E73", "#CC79A7", "#F0E442", "#D55E00", "#B8A1D9", "#A3A3A3", "#8FBC8F", "#E7B9A6", "#C9B458")

# The lab manager's week as one bar out of week_hours: recruitment (from the
# costs tab) + each kind of work (from the hours tab) + white for unlogged time
lm_week_html <- function(cs, lmh, week, study_start, week_hours = 37.5) {
  rec <- cs$all[cs$all$type == "Time" & cs$all$person %in% "Lab manager" & cs$all$week %in% week, , drop = FALSE]
  rh <- c(sum(rec$hours[rec$mode %in% "In person"], na.rm = TRUE), sum(rec$hours[rec$mode %in% "Remote"], na.rm = TRUE),
          sum(rec$hours[is.na(rec$mode)], na.rm = TRUE))
  segs <- data.frame(label = names(.rec_cols), hours = rh, col = unname(.rec_cols), rec = TRUE, stringsAsFactors = FALSE)
  if (!is.null(lmh)) {
    wk <- lmh[rth_study_week(lmh$date, study_start) %in% week, , drop = FALSE]
    oh <- tapply(wk$hours, wk$category, sum)
    oh[is.na(oh)] <- 0
    up1 <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
    segs <- rbind(segs, data.frame(label = up1(names(oh)), hours = as.numeric(oh), col = rep_len(.work_cols, length(oh)),
                                   rec = FALSE, stringsAsFactors = FALSE))
  }
  segs <- segs[segs$hours > 0 | segs$label %in% names(.rec_cols)[1:2], , drop = FALSE]
  logged <- sum(segs$hours)
  gap <- week_hours - logged
  scale <- max(week_hours, logged)
  lab <- function(h) sprintf("%s h &middot; %.0f%%", .hrs(h), 100 * h / week_hours)
  bar_segs <- segs[segs$hours > 0, , drop = FALSE]
  bar <- paste0(
    paste(sprintf('<div class="seg" style="width:%.3f%%;background:%s;color:%s" title="%s: %s">%s</div>', 100 * bar_segs$hours / scale,
                  bar_segs$col, ifelse(bar_segs$col %in% c("#0072B2", "#009E73", "#D55E00"), "#fff", "#111827"), .esc(bar_segs$label), gsub("&middot;", "-", lab(bar_segs$hours)),
                  ifelse(bar_segs$hours / scale >= .085, lab(bar_segs$hours), "")), collapse = ""),
    if (gap > 0) sprintf('<div class="seg empty" style="width:%.3f%%" title="Not logged: %s">%s</div>', 100 * gap / scale,
                         gsub("&middot;", "-", lab(gap)), if (gap / scale >= .085) lab(gap) else "") else "",
    if (logged > week_hours) sprintf('<div class="mark375" style="left:%.3f%%"></div>', 100 * week_hours / scale) else "")
  chips <- paste0(
    paste(sprintf('<span class="chip%s"><i style="background:%s"></i>%s: <b>%s</b></span>', ifelse(segs$rec, " rec", ""),
                  segs$col, .esc(segs$label), lab(segs$hours)), collapse = ""),
    if (gap > 0) sprintf('<span class="chip"><i class="empty"></i>Not logged: <b>%s</b></span>', lab(gap)) else "")
  rec_tot <- sum(segs$hours[segs$rec])
  head <- sprintf('<p class="note">Out of a %s-hour week. Recruitment: <b>%s</b>. Logged in all: %s%s.</p>',
                  .hrs(week_hours), lab(rec_tot), lab(logged),
                  if (logged > week_hours) sprintf(" (%s h over; the line marks %s h)", .hrs(logged - week_hours), .hrs(week_hours)) else "")
  nts <- attr(lmh, "notes")
  nts <- if (!is.null(nts)) nts[rth_study_week(nts$date, study_start) %in% week, , drop = FALSE] else NULL
  notes <- if (!is.null(nts) && nrow(nts)) sprintf('<details><summary>His notes this week (%d)</summary><ul class="wi">%s</ul></details>',
    nrow(nts), paste(sprintf("<li>%s: %s</li>", format(nts$date, "%a %m/%d"), .esc(nts$note)), collapse = "")) else ""
  missing_tab <- if (is.null(lmh)) '<p class="note">No &ldquo;lab manager hours&rdquo; tab found in the costs sheet, so only recruitment is shown.</p>' else ""
  bracket <- if (rec_tot > 0) sprintf('<div class="recspan" style="width:%.3f%%">Recruitment %s</div>', 100 * rec_tot / scale, lab(rec_tot)) else ""
  paste0(missing_tab, head, '<div class="wkwrap">', bracket, '<div class="wkbar">', bar, '</div></div>', '<div class="chips">', chips, '</div>', notes)
}

# Lab manager's recruitment hours this week, by source (in person / remote)
lm_rec_html <- function(cs, week) {
  rec <- cs$all[cs$all$type == "Time" & cs$all$person %in% "Lab manager" & cs$all$week %in% week, , drop = FALSE]
  if (!nrow(rec)) return('<p class="note">No recruitment time logged for the lab manager this week.</p>')
  src <- ifelse(is.na(rec$source), "(no source)", rec$source)
  srcs <- names(sort(tapply(rec$hours, src, sum, na.rm = TRUE), decreasing = TRUE))
  mx <- max(tapply(rec$hours, src, sum, na.rm = TRUE))
  rows <- vapply(srcs, function(s) {
    x <- rec[src == s, , drop = FALSE]
    h <- c(sum(x$hours[x$mode %in% "In person"], na.rm = TRUE), sum(x$hours[x$mode %in% "Remote"], na.rm = TRUE),
           sum(x$hours[is.na(x$mode)], na.rm = TRUE))
    segs <- paste(sprintf('<div style="width:%.3f%%;background:%s" title="%s: %s h"></div>', 100 * h / mx, .rec_cols, names(.rec_cols), .hrs(h))[h > 0], collapse = "")
    sprintf('<div class="recrow"><span>%s</span><div class="stack">%s</div><span class="n">%s h</span><span class="what">%s</span></div>',
            .esc(s), segs, .hrs(sum(h)), .esc(paste(unique(stats::na.omit(x$what)), collapse = "; ")))
  }, "")
  used <- c(any(rec$mode %in% "In person"), any(rec$mode %in% "Remote"), any(is.na(rec$mode)))
  key <- paste(sprintf('<span class="chip"><i style="background:%s"></i>%s</span>', .rec_cols, sub("Recruitment, ", "", names(.rec_cols)))[used], collapse = "")
  paste0(sprintf('<p class="note">Total: %s h (%s h in person, %s h remote).</p>', .hrs(sum(rec$hours, na.rm = TRUE)),
                 .hrs(sum(rec$hours[rec$mode %in% "In person"], na.rm = TRUE)), .hrs(sum(rec$hours[rec$mode %in% "Remote"], na.rm = TRUE))),
         '<div class="chips">', key, '</div>', paste(rows, collapse = ""))
}
