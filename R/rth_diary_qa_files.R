# Diary QA: the proposals workbook, the decisions log, and the clean data -------

.qa_prop_cols <- c("row", "responseid", "id", "startdate", "enddate", "progress", "finished", "diary_date", "rule",
                   "proposed_action", "proposed_should_date", "final_action", "final_should_date", "source",
                   "reviewer", "review_date", "note")
.qa_log_cols <- c(setdiff(.qa_prop_cols, "row"), "logged_on")
.qa_edit_cols <- c("final_action", "final_should_date", "source", "reviewer", "review_date", "note")
.qa_date_cols <- c("diary_date", "proposed_should_date", "final_should_date", "review_date", "logged_on")
.qa_actions <- c("keep", "reassign", "drop")

.qa_empty <- function(cols) {
  df <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols),
                      stringsAsFactors = FALSE)
  for (v in intersect(.qa_date_cols, cols)) df[[v]] <- as.Date(character())
  df
}

# One row per tagged submission that has no decision yet, in report order.
.qa_proposal_rows <- function(r) {
  x <- r$x[!is.na(r$x$row), , drop = FALSE]
  if (!nrow(x)) return(.qa_empty(.qa_prop_cols))
  x <- x[order(x$row), , drop = FALSE]
  pa <- c(move = "reassign", keep = "keep", drop = "drop")[x$action]
  pa <- unname(ifelse(x$rule %in% "R99", NA, pa))
  psd <- x$p_sd
  psd[is.na(pa) | pa %in% "drop"] <- NA
  fmt <- function(t) ifelse(is.na(t), NA, format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  data.frame(row = x$row, responseid = x$responseid, id = x$person, startdate = fmt(x$start), enddate = fmt(x$end),
             progress = x$progress, finished = as.integer(x$finished), diary_date = x$diary_date, rule = x$rule,
             proposed_action = pa, proposed_should_date = psd, final_action = pa, final_should_date = psd,
             source = ifelse(is.na(pa), NA, "auto"), reviewer = NA_character_, review_date = as.Date(NA),
             note = NA_character_, stringsAsFactors = FALSE)
}

# Workbook with one tab per diary. editable = TRUE for the proposals file: the
# columns to fill in are shaded, final_action and source have dropdowns.
.qa_write_book <- function(path, tabs, cols, editable = FALSE) {
  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(textDecoration = "bold", border = "bottom")
  ds <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")
  ed <- openxlsx::createStyle(fgFill = "#FFF4CC")
  widths <- c(row = 5, responseid = 19, id = 11, startdate = 19, enddate = 19, progress = 9, finished = 8,
              diary_date = 11, rule = 6, proposed_action = 15, proposed_should_date = 19, final_action = 13,
              final_should_date = 17, source = 9, reviewer = 10, review_date = 12, note = 40, logged_on = 11)
  for (tp in names(.qa_types)) {
    df <- tabs[[tp]]
    if (is.null(df) || !nrow(df)) df <- .qa_empty(cols)
    df <- df[, cols, drop = FALSE]
    openxlsx::addWorksheet(wb, tp)
    openxlsx::writeData(wb, tp, df, headerStyle = hs, keepNA = FALSE)
    last <- max(nrow(df) + 1, if (editable) 300 else 1)
    for (v in intersect(.qa_date_cols, cols))
      openxlsx::addStyle(wb, tp, ds, rows = 2:last, cols = match(v, cols), gridExpand = TRUE, stack = TRUE)
    if (editable) {
      openxlsx::addStyle(wb, tp, ed, rows = 1:last, cols = match(.qa_edit_cols, cols), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::setColWidths(wb, tp, cols = seq_along(cols), widths = unname(widths[cols]))
    openxlsx::freezePane(wb, tp, firstActiveRow = 2, firstActiveCol = 3)
  }
  if (editable) {
    openxlsx::addWorksheet(wb, "lists", visible = FALSE)
    openxlsx::writeData(wb, "lists", .qa_actions, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, "lists", c("auto", "override"), startCol = 2, colNames = FALSE)
    for (tp in names(.qa_types)) {
      # (some openxlsx versions warn about an unused sprintf argument here; the dropdown is fine)
      suppressWarnings({
        openxlsx::dataValidation(wb, tp, cols = match("final_action", cols), rows = 2:300, type = "list",
                                 value = "'lists'!$A$1:$A$3")
        openxlsx::dataValidation(wb, tp, cols = match("source", cols), rows = 2:300, type = "list",
                                 value = "'lists'!$B$1:$B$2")
      })
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# Excel dates arrive as day numbers, Date, or text (2026-07-06, 7/6/2026, 7/6/26).
.qa_xl_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA
  out <- as.Date(rep(NA, length(x)))
  num <- suppressWarnings(as.numeric(x))
  ok <- !is.na(num)
  out[ok] <- as.Date(num[ok], origin = "1899-12-30")
  for (f in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d")) {
    need <- is.na(out) & !is.na(x)
    if (!any(need)) break
    try <- as.Date(x[need], format = f)
    yr <- as.numeric(format(try, "%Y"))
    try[!is.na(yr) & (yr < 2000 | yr > 2100)] <- NA
    out[need] <- try
  }
  attr(out, "bad") <- which(is.na(out) & !is.na(x))
  out
}

.qa_read_book <- function(path, cols) {
  out <- stats::setNames(lapply(names(.qa_types), function(tp) .qa_empty(cols)), names(.qa_types))
  if (is.null(path) || !file.exists(path)) return(out)
  sheets <- openxlsx::getSheetNames(path)
  for (tp in intersect(names(.qa_types), sheets)) {
    df <- tryCatch(openxlsx::read.xlsx(path, sheet = tp, detectDates = FALSE, skipEmptyRows = TRUE,
                                       na.strings = c("NA", "")),
                   error = function(e) NULL)
    if (is.null(df) || !nrow(df)) next
    for (v in setdiff(cols, names(df))) df[[v]] <- NA
    df <- df[!is.na(df$responseid) & trimws(as.character(df$responseid)) != "", , drop = FALSE]
    if (!nrow(df)) next
    for (v in setdiff(cols, .qa_date_cols)) df[[v]] <- if (v %in% c("row", "progress", "finished"))
      suppressWarnings(as.numeric(df[[v]])) else trimws(as.character(df[[v]]))
    for (v in intersect(.qa_date_cols, cols)) df[[v]] <- .qa_xl_date(df[[v]])
    df$final_action <- tolower(df$final_action)
    df$final_action[df$final_action %in% c("move", "reassigned")] <- "reassign"
    df$final_action[df$final_action %in% "kept"] <- "keep"
    df$final_action[df$final_action %in% "dropped"] <- "drop"
    out[[tp]] <- df[, cols, drop = FALSE]
  }
  out
}

# Check the reviewed proposals for one diary; returns them ready for the log.
.qa_check_props <- function(p, r, path) {
  if (!nrow(p)) return(p)
  tab <- r$type
  where <- function(i) paste(ifelse(is.na(p$row[i]), paste0("line ", i + 1), paste0("row ", p$row[i])), collapse = ", ")
  probs <- character()
  blank <- is.na(p$final_action) | p$final_action == ""
  if (any(blank)) probs <- c(probs, sprintf("final_action is empty for %s (these need a decision).", where(which(blank))))
  bad <- !blank & !(p$final_action %in% .qa_actions)
  if (any(bad)) probs <- c(probs, sprintf("final_action must be keep, reassign or drop (%s).", where(which(bad))))
  gone <- !(p$responseid %in% r$x$responseid)
  if (any(gone)) probs <- c(probs, sprintf("response ID not in this export: %s (%s).",
                                           paste(p$responseid[gone], collapse = ", "), where(which(gone))))
  baddate <- unique(c(attr(p$final_should_date, "bad"), attr(p$review_date, "bad")))
  if (length(baddate)) probs <- c(probs, sprintf("can't read the date in %s. use a date like 2026-07-06.", where(baddate)))
  nodate <- p$final_action %in% "reassign" & is.na(p$final_should_date)
  if (any(nodate)) probs <- c(probs, sprintf("reassign needs final_should_date (%s).", where(which(nodate))))
  if (length(probs))
    stop(sprintf("Please fix the %s tab of %s:\n- %s", tab, basename(path), paste(probs, collapse = "\n- ")), call. = FALSE)

  dd <- r$x$diary_date[match(p$responseid, r$x$responseid)]
  k <- p$final_action %in% "keep" & is.na(p$final_should_date)
  p$final_should_date[k] <- dd[k]
  p$final_should_date[p$final_action %in% "drop"] <- NA
  same_date <- (is.na(p$final_should_date) & is.na(p$proposed_should_date)) |
    (!is.na(p$final_should_date) & !is.na(p$proposed_should_date) & p$final_should_date == p$proposed_should_date)
  same <- !is.na(p$proposed_action) & p$final_action == p$proposed_action & same_date
  p$source <- ifelse(same, "auto", "override")
  attr(p$final_should_date, "bad") <- NULL; attr(p$review_date, "bad") <- NULL
  p
}

.qa_merge_log <- function(old, new, run_date) {
  if (!nrow(new)) return(old)
  new$logged_on <- as.Date(run_date)
  new <- new[, .qa_log_cols, drop = FALSE]
  old <- old[!(old$responseid %in% new$responseid), .qa_log_cols, drop = FALSE]
  out <- rbind(old, new)
  out[order(suppressWarnings(as.numeric(sub(",.*", "", out$id))), out$startdate), , drop = FALSE]
}

# Step 2: dates where a reviewer's change put a second entry (keep / reassign
# onto a date that already has one). Returns the messages and the rows involved.
.qa_review_clashes <- function(r, p) {
  x <- r$x
  st <- x$diary_date
  st[x$decided] <- x$d_sd[x$decided]
  st[x$decided & x$d_action %in% "drop"] <- NA
  ov <- x$responseid %in% p$responseid[p$source %in% "override"]
  key <- ifelse(is.na(st), NA, paste(x$person, st))
  dup <- unique(key[!is.na(key) & duplicated(key)])
  dup <- dup[vapply(dup, function(k) any(ov & key %in% k), TRUE)]
  msg <- vapply(dup, function(k) {
    i <- which(key %in% k)
    sprintf("%s: %d entries for %s (%s)", x$person[i[1]], length(i), .qa_md(st[i[1]]),
            paste(.qa_ts(x$start[i]), collapse = ", "))
  }, "")
  list(msg = unname(msg), rows = x$responseid[key %in% dup])
}

# After step 2: every kept entry has its own night/day.
.qa_conflicts <- function(r) {
  x <- r$x[!is.na(r$x$cur), , drop = FALSE]
  key <- paste(x$person, x$cur)
  dup <- key %in% key[duplicated(key)]
  if (!any(dup)) return(character())
  x <- x[dup, , drop = FALSE]
  vapply(split(x, paste(x$person, x$cur)), function(g)
    sprintf("%s: %d entries kept for %s (%s)", g$person[1], nrow(g), .qa_md(g$cur[1]),
            paste(.qa_ts(g$start), collapse = ", ")), "")
}

# Clean data: every response from the export (previews left out), one row per
# ID, with the QA columns added. Dropped entries stay in, with exclude = 1.
.qa_clean <- function(r, logged, s) {
  d <- r$d
  raw <- d$raw
  n <- nrow(raw)
  k <- r$x$.row
  should <- as.Date(rep(NA, n)); excl <- rep(1, n)
  qa_rule <- rep(NA_character_, n); qa_action <- rep(NA_character_, n); qa_source <- rep(NA_character_, n)
  should[k] <- r$x$cur
  excl[k] <- as.numeric(is.na(r$x$cur))
  m <- match(r$x$responseid, logged$responseid)
  qa_rule[k] <- logged$rule[m]
  qa_action[k] <- logged$final_action[m]
  qa_source[k] <- logged$source[m]
  qa_rule[setdiff(seq_len(n), k)] <- "no id or start time"
  out <- raw
  out$diary_date <- d$x$diary_date
  out$should_date <- should
  out$compliance <- .qa_compliance(raw$startdate, should, d$kind, s)
  b <- .qa_binge(r$x$person, r$x$diary_date, r$x$start, !is.na(r$x$cur), s$binge_hours)
  out$compliance[k[b]] <- 4L
  out$exclude <- excl
  out$qa_rule <- qa_rule
  out$qa_action <- qa_action
  out$qa_source <- qa_source
  # one row per ID (a shared caregiver's entries count for each teen)
  ids <- lapply(d$x$person, function(p) if (is.na(p)) NA_character_ else .qa_split(p))
  out <- out[rep(seq_len(n), lengths(ids)), , drop = FALSE]
  out$id <- unlist(ids)
  first <- tapply(out$should_date[out$exclude == 0], out$id[out$exclude == 0], min)
  f <- as.Date(as.numeric(first[out$id]), origin = "1970-01-01")
  out$should_day <- as.numeric(out$should_date - f) + 1
  out$should_day[out$exclude == 1] <- NA
  front <- c("id", "diary_date", "should_date", "should_day", "compliance", "exclude", "qa_rule", "qa_action", "qa_source")
  out <- out[order(out$id, out$should_date, out$startdate), c(setdiff(names(out), front), front)]
  attr(out$exclude, "label") <- "1 = dropped in diary QA (or no ID); leave out of analyses"
  attr(out$compliance, "label") <- if (d$kind == "night")
    sprintf("1 = on time (by %02d:00 next day), 2 = before %02d:00 next day, 3 = %02d:00 next day or later, 4 = binged (done in one sitting with other entries, all kept)",
            s$night_ontime_until, s$night_cutoff, s$night_cutoff)
  else sprintf("1 = before %02d:00, 2 = %02d:00 or later that day, 3 = next day or later, 4 = binged (done in one sitting with other entries, all kept)",
               s$morning_ontime_until, s$morning_ontime_until)
  out
}
