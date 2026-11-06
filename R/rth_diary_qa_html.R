# Diary QA: the HTML report -------------------------------------------------------

.qa_esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.qa_table <- function(head, rows, num = integer()) {
  th <- vapply(seq_along(head), function(i)
    sprintf('<th%s>%s</th>', if (i %in% num) ' class="num"' else "", head[i]), "")
  sprintf('<div class="tw"><table class="qa"><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>',
          paste(th, collapse = ""), paste(rows, collapse = "\n"))
}
.qa_tr <- function(cells, cls = "", wrap = integer(), num = integer()) {
  td <- vapply(seq_along(cells), function(i) {
    c <- c(if (i %in% wrap) "wrap", if (i %in% num) "num")
    sprintf('<td%s>%s</td>', if (length(c)) sprintf(' class="%s"', paste(c, collapse = " ")) else "", cells[i])
  }, "")
  sprintf('<tr%s>%s</tr>', if (nzchar(cls)) sprintf(' class="%s"', cls) else "", paste(td, collapse = ""))
}

.qa_anchor <- function(type, p) paste0(type, "-", gsub("[^0-9A-Za-z]+", "-", p))

# As recorded: the export as the cleaning script sees it.
.qa_raw_table <- function(y) {
  day <- as.numeric(y$diary_date - min(y$diary_date)) + 1
  rows <- vapply(seq_len(nrow(y)), function(i) {
    rw <- if (!is.na(y$row[i])) y$row[i] else if (y$decided[i]) "logged" else ""
    .qa_tr(c(day[i], .qa_md(y$diary_date[i]), .qa_ts(y$start[i]),
             if (y$finished[i] && !is.na(y$end[i])) .qa_hms(y$end[i]) else "&ndash;", rw),
           cls = if (y$tagged[i]) "flag" else "")
  }, "")
  .qa_table(c("day", "diary_date", "started", "finished", "row"), rows)
}

# After proposals: every night/day, as it would be after the proposals.
.qa_after_table <- function(y, unit, s) {
  r99 <- y$rule %in% "R99"
  sd <- y$cur; sd[r99] <- NA
  pos <- sd; pos[is.na(pos)] <- y$diary_date[is.na(pos)]
  first <- if (any(!is.na(sd))) min(sd, na.rm = TRUE) else min(y$diary_date)
  last <- max(first + s$n_days - 1, pos)
  occupied <- unique(c(sd[!is.na(sd)], y$diary_date[r99]))
  empty <- seq(first, last, by = "day")
  empty <- empty[!(empty %in% occupied)]
  key <- rbind(data.frame(date = pos, k = 0, t = as.numeric(y$start), i = seq_len(nrow(y))),
               if (length(empty)) data.frame(date = empty, k = 1, t = 0, i = NA))
  key <- key[order(key$date, key$k, key$t), , drop = FALSE]
  rows <- vapply(seq_len(nrow(key)), function(j) {
    i <- key$i[j]
    if (is.na(i)) {
      d <- key$date[j]
      return(.qa_tr(c(as.numeric(d - first) + 1, .qa_md(d), '<i>no entry</i>', "", "", "", "", ""), cls = "none"))
    }
    rule <- prop <- ""
    if (y$tagged[i]) {
      rule <- y$rule[i]
      prop <- if (r99[i]) "needs a decision" else switch(y$action[i], move = paste("move to", .qa_md(sd[i])),
                                                          keep = "keep", drop = "drop")
    } else if (y$decided[i]) {
      rule <- if (is.na(y$d_rule[i])) "" else y$d_rule[i]
      prop <- paste("logged:", y$d_action[i])
    }
    n <- if (is.na(sd[i])) "&ndash;" else as.numeric(sd[i] - first) + 1
    .qa_tr(c(n, if (is.na(sd[i])) "&ndash;" else .qa_md(sd[i]), .qa_ts(y$start[i]), .qa_md(y$diary_date[i]),
             rule, prop, if (is.na(y$compliance[i])) "&ndash;" else y$compliance[i],
             if (is.na(y$row[i])) "" else y$row[i]),
           cls = if (y$tagged[i]) "flag" else "none")
  }, "")
  .qa_table(c(unit, "should_date", "started", "diary_date", "rule", "proposal", "compliance", "row"), rows)
}

.qa_person_html <- function(r, p, s) {
  unit <- .qa_unit(r$kind)
  y <- r$x[r$x$person == p, , drop = FALSE]
  y <- y[order(y$start), , drop = FALSE]
  ev <- Filter(function(e) e$person == p, r$events)
  ids <- .qa_split(p)
  notes <- character()
  if (length(ids) > 1)
    notes <- c(notes, sprintf("same caregiver for %s; decisions apply to all of these ids",
                              paste(ids, collapse = " and ")))
  if (any(y$decided)) notes <- c(notes, "rows marked logged were decided in an earlier week")
  li <- vapply(ev, function(e) sprintf("<li><b>%s:</b> %s</li>",
                                       if (e$rule == "R99") "needs a decision (R99)" else e$rule, .qa_esc(e$text)), "")
  kept <- y$cur[!is.na(y$cur) & !(y$rule %in% "R99")]
  if (length(kept) && min(kept) < min(y$diary_date)) {
    k <- as.numeric(min(y$diary_date) - min(kept))
    li <- c(li, sprintf("<li><b>first %s changes</b> from %s to %s, so every day number for %s goes up by %s.</li>",
                        unit, .qa_md(min(y$diary_date)), .qa_md(min(kept)), .qa_esc(p),
                        if (k == 1) "one" else k))
  }
  paste0(sprintf('<h2 id="%s">%s</h2>', .qa_anchor(r$type, p), .qa_esc(p)),
         if (length(notes)) sprintf('<p class="note">%s</p>', paste(notes, collapse = " &middot; ")) else "",
         sprintf("<ul>%s</ul>", paste(li, collapse = "")),
         "<h3>as recorded</h3>", .qa_raw_table(y),
         "<h3>after proposals</h3>", .qa_after_table(y, unit, s))
}

.qa_counts <- function(r) {
  t <- r$x[r$x$tagged & !r$x$decided, , drop = FALSE]
  c(tagged = nrow(t), people = length(unique(t$person)),
    needs = sum(vapply(r$events, function(e) isTRUE(e$needs), TRUE)))
}

.qa_diary_html <- function(r, type, s) {
  info <- .qa_types[[type]]
  head <- sprintf('<h1 id="%s">%s</h1>', type, info$title)
  if (is.null(r)) return(paste0(head, "<p>no export given this week.</p>"))
  cn <- .qa_counts(r)
  out <- paste0(head, sprintf("<p>%s. export: %s. tab <b>%s</b> in the proposals file.</p>",
                              if (cn[["tagged"]]) sprintf("%d submissions tagged across %d %s", cn[["tagged"]],
                                                          cn[["people"]], if (cn[["people"]] == 1) info$who else info$whos)
                              else "nothing tagged", .qa_esc(r$d$file), type))
  # Rows to fix (only when there are any)
  fix <- character()
  if (nrow(r$d$previews))
    fix <- c(fix, sprintf("<li>%d preview or test response%s left out (%s). please delete %s in qualtrics.</li>",
                          nrow(r$d$previews), if (nrow(r$d$previews) == 1) " is" else "s are",
                          .qa_esc(paste(r$d$previews$started, collapse = ", ")),
                          if (nrow(r$d$previews) == 1) "it" else "them"))
  if (nrow(r$d$noid))
    fix <- c(fix, sprintf("<li>%d response%s without an id (externalreference) or start time, left out: %s.</li>",
                          nrow(r$d$noid), if (nrow(r$d$noid) == 1) "" else "s",
                          .qa_esc(paste(r$d$noid$responseid, r$d$noid$started, collapse = ", "))))
  if (length(r$d$bad_ids))
    fix <- c(fix, sprintf("<li>id%s that don't look like %s ids (4 digits ending in %s): %s.</li>",
                          if (length(r$d$bad_ids) == 1) "" else "s", info$who, info$role,
                          .qa_esc(paste(r$d$bad_ids, collapse = ", "))))
  if (length(fix)) out <- paste0(out, "<h3>rows to fix</h3><ul>", paste(fix, collapse = ""), "</ul>")
  # Needs a decision
  nd <- Filter(function(e) isTRUE(e$needs), r$events)
  out <- paste0(out, "<h3>needs a decision</h3>")
  if (!length(nd)) {
    out <- paste0(out, "<p>none found.</p>")
  } else {
    rows <- vapply(nd, function(e) {
      rr <- r$x$row[r$x$person == e$person & r$x$rule %in% "R99" & r$x$cur %in% e$date]
      .qa_tr(c(.qa_esc(e$person), .qa_md(e$date), paste(range(rr), collapse = "&ndash;"),
               .qa_esc(sub("^[^:]*: ", "", e$text))), wrap = 4)
    }, "")
    out <- paste0(out, .qa_table(c(info$who, "date", "rows", "why"), rows))
  }
  paste0(out, paste(vapply(r$persons, function(p) .qa_person_html(r, p, s), ""), collapse = "\n"))
}

.qa_rules_html <- function(s) {
  dates <- .qa_table(c("diary", "diary_date", "compliance 1", "compliance 2", "compliance 3"), c(
    .qa_tr(c("caregiver and adolescent night", sprintf("started before %02d:00 counts for the previous night", s$night_cutoff),
             sprintf("by %02d:00 next day", s$night_ontime_until),
             sprintf("%02d:00&ndash;%02d:59 next day", s$night_ontime_until, s$night_cutoff - 1),
             sprintf("%02d:00 next day or later", s$night_cutoff)), wrap = 2),
    .qa_tr(c("adolescent morning", sprintf("started before %02d:00 counts for the previous day", s$morning_cutoff),
             sprintf("before %02d:00", s$morning_ontime_until),
             sprintf("%02d:00&ndash;23:59", s$morning_ontime_until), "next day or later"), wrap = 2)))
  tags <- .qa_table(c("rule", "diaries", "when", "proposal"), c(
    .qa_tr(c("R0", "all", sprintf("an entry with no answers, or an unfinished entry restarted within %d minutes (other unfinished entries count like any entry)", s$restart_minutes),
             "drop it"), wrap = 3:4),
    .qa_tr(c("R1", "all", sprintf("2+ entries on a night or day, and 1&ndash;%d empty ones right before it. on a participant's first date: one night-diary entry started %02d:00&ndash;%02d:00",
                                  s$r1_max_gap, s$r3_from, s$r3_until),
             "the earliest entries fill the empty ones, one each, nearest first"), wrap = 3:4),
    .qa_tr(c("R2", "all", "2+ entries left on a night or day",
             sprintf("keep one, drop the rest. night entries all started after %02d:00 on the date: the one closest to the participant's usual start time (median of their other entries); otherwise the earliest. finished entries first",
                     s$r2_evening_from)), wrap = 3:4),
    .qa_tr(c("R3", "night only", sprintf("1 entry started between %02d:00 and %02d:00, and the night before is empty (not for a participant's first entry)",
                                         s$r3_from, s$r3_until), "move it back one night"), wrap = 3:4),
    .qa_tr(c("R99", "all", "none of the above fits, or something looks off (e.g. an entry that took 0 seconds, or a clash with an earlier week's decision)",
             "needs a decision"), wrap = 3:4)))
  paste0('<h1 id="rules">rules</h1><h3>dates and compliance</h3>', dates, "<h3>tagging</h3>", tags,
         '<p class="note">decisions in the decisions log are applied first and never proposed again. ',
         'compliance is worked out from the start time and should_date.</p>')
}

.qa_html <- function(res, s, run_time) {
  css <- paste(readLines(file.path(.qa_home(), "diary_qa.css"), warn = FALSE), collapse = "\n")
  toc <- c('<a href="#summary">summary</a>')
  sum_rows <- character()
  for (tp in names(.qa_types)) {
    r <- res[[tp]]
    toc <- c(toc, sprintf('<a href="#%s">%s</a>', tp, .qa_types[[tp]]$title))
    if (!is.null(r)) toc <- c(toc, vapply(r$persons, function(p)
      sprintf('<a class="sub" href="#%s">%s</a>', .qa_anchor(tp, p), .qa_esc(p)), ""))
    cn <- if (is.null(r)) c(tagged = NA, people = NA, needs = NA) else .qa_counts(r)
    na <- function(v) if (is.na(v)) "&ndash;" else v
    sum_rows <- c(sum_rows, .qa_tr(c(.qa_types[[tp]]$title, na(cn[["tagged"]]), na(cn[["people"]]), na(cn[["needs"]])),
                                   num = 2:4))
  }
  toc <- c(toc, '<a href="#rules">rules</a>')
  body <- paste(vapply(names(.qa_types), function(tp) .qa_diary_html(res[[tp]], tp, s), ""), collapse = "\n")
  paste0('<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
         '<meta name="viewport" content="width=device-width, initial-scale=1"><title>rth diaries qa</title><style>',
         css, '</style></head><body><div class="main"><nav id="toc">', paste(toc, collapse = "\n"),
         '</nav><div class="content"><h1 class="title">rth diaries qa</h1>',
         sprintf('<h4 class="date">%s &middot; step 1 of 2</h4>', tolower(format(run_time, "%d %B %Y, %H:%M"))),
         '<p>Please open the proposals excel sheet and make any revisions when we disagree with the proposed plan.</p>',
         '<h1 id="summary">summary</h1>',
         .qa_table(c("diary", "submissions tagged", "participants", "need a decision"), sum_rows, num = 2:4),
         '<p class="note">in each section, <b>as recorded</b> is the data as it comes out of the cleaning script ',
         '(diary_date and day from the date rules below), with tagged submissions in bold. <b>after proposals</b> ',
         'shows every night or day as it would be after the proposals; rows in grey need nothing.</p>',
         body, .qa_rules_html(s), '</div></div></body></html>')
}

.qa_home <- function() {
  pkg <- tryCatch(utils::packageName(environment(.qa_home)), error = function(e) NULL)
  p <- if (is.null(pkg)) "" else system.file("rth_diary_qa", package = pkg)
  if (nzchar(p) && file.exists(file.path(p, "diary_qa.css"))) return(p)
  p <- getOption("rth_diary_qa.home")
  if (is.null(p)) stop("Can't find the diary QA report files: install the lsh package or run dev/load_dev.R", call. = FALSE)
  p
}
