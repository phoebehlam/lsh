# Weekly diary QA: duplicate, binged and late diary entries ----------------------

#' Settings for the weekly diary QA
#'
#' Change a setting with \code{s <- rth_diary_qa_settings(); s$r3_until <- 20},
#' then pass \code{settings = s} to \code{\link{rth_diary_qa}}.
#'
#' @return A list:
#' \describe{
#'   \item{n_days}{Diary days per participant (15). Extra days are allowed; this only sets how many
#'     nights/days the report lists.}
#'   \item{night_cutoff}{Night diaries started before this hour count for the previous night (12).
#'     Also the start of compliance 3 the next day.}
#'   \item{night_ontime_until}{Night diaries started the next day before this hour are on time (4).}
#'   \item{r3_from, r3_until}{R3: a single night entry started between these hours (12 to 19, i.e. before
#'     the night diary link usually goes out) moves back one night when that night is empty.}
#'   \item{morning_cutoff}{Morning diaries started before this hour count for the previous day (4).}
#'   \item{morning_ontime_until}{Morning diaries started before this hour are on time (14).}
#'   \item{keep_status}{Qualtrics Status codes kept: 0 = real response, 4 = imported, 16 = offline.
#'     Previews and tests are left out and listed in the report.}
#'   \item{compare_items}{Answers compared between two entries for the same night/day, to tell a
#'     resubmission (identical answers) from a separate attempt.}
#' }
#' @export
rth_diary_qa_settings <- function() {
  list(
    n_days = 15,
    night_cutoff = 12,
    night_ontime_until = 4,
    r3_from = 12,
    r3_until = 19,
    morning_cutoff = 4,
    morning_ontime_until = 14,
    keep_status = c(0, 4, 16),
    compare_items = list(
      night = paste0("feeling_", c("lonely", "angry", "calm", "close", "embarrass", "excited", "happy", "joyful",
                                   "judged", "nervous", "rejected", "sad", "understood", "worried")),
      morning = c("bedtime_hr", "bedtime_min", "bedtime_am.pm", "latency", "waketime_hr", "waketime_min",
                  "waketime_am.pm", "rested", "quality", "watchwear", "remove_watch")))
}

#' Weekly QA of the Rise to Health diaries (duplicate, binged and late entries)
#'
#' Run it twice each week with the three Qualtrics exports, as downloaded
#' (never edited).
#'
#' \strong{Step 1} (no \code{proposals}): tags entries that need a look, proposes
#' what to do with each, and writes an HTML report plus a proposals workbook
#' with one tab per diary. Entries already in the decisions log are not
#' proposed again.
#'
#' \strong{Step 2} (\code{proposals} = the reviewed workbook): checks the
#' edits, adds them to the decisions log, and writes the clean data: one .sav
#' per diary with every response (previews left out), one row per ID, and these
#' columns added at the end: \code{id}, \code{diary_date}, \code{should_date},
#' \code{should_day}, \code{compliance}, \code{exclude} (1 = dropped; leave out
#' of analyses), \code{qa_rule}, \code{qa_action}, \code{qa_source}.
#'
#' The caregiver diary's externalreference can hold two IDs for a caregiver
#' with two teens ("1212, 1172"). Their entries are reviewed once and the
#' decision applies to both; the clean data has one row per ID.
#'
#' Dates and compliance:
#' \itemize{
#'   \item Night diaries (caregiver and adolescent): started before 12:00 counts
#'     for the previous night. Compliance 1 = on time (by 04:00 the next day),
#'     2 = 04:00-11:59 the next day, 3 = 12:00 the next day or later.
#'   \item Morning diary: started before 04:00 counts for the previous day.
#'     Compliance 1 = before 14:00, 2 = 14:00-23:59, 3 = the next day or later.
#' }
#' Tagging rules, per participant and night/day:
#' \itemize{
#'   \item R0: unfinished entry and a finished one exists for the same date: drop the unfinished one.
#'   \item R1: 2+ entries and the night/day before is empty: the earliest moves back one.
#'   \item R2: 2+ entries and the night/day before is filled: keep the earliest finished one, drop the rest.
#'   \item R3 (night diaries): one entry started 12:00-19:00 and the night before is empty (not the
#'     participant's first entry): it moves back one night.
#'   \item R99: anything else that looks off (e.g. an entry that took 0 seconds): needs a decision.
#' }
#'
#' @param caregiver_night,adolescent_night,adolescent_morning Paths to this
#'   week's Qualtrics exports (.sav). Leave one out (NULL) to skip that diary.
#' @param proposals Path to the reviewed proposals workbook (.xlsx) from step 1.
#'   NULL (default) runs step 1.
#' @param log The decisions log (.xlsx). Default: \code{rth_diary_qa_decisions_log.xlsx}
#'   in \code{out_dir}. Created in the first step 2 run; keep using the same file every week.
#' @param out_dir Folder for the report, proposals and clean data.
#' @param run_date Date used in file names (default today).
#' @param settings From \code{\link{rth_diary_qa_settings}}.
#' @return Step 1: path to the report (invisibly). Step 2: a named list of the clean data (invisibly).
#' @examples
#' \dontrun{
#' # step 1
#' rth_diary_qa(caregiver_night    = "rth_caregiver night diary_November 5, 2026_13.14.sav",
#'              adolescent_night   = "rth_adolescent night diary_November 5, 2026_13.15.sav",
#'              adolescent_morning = "rth_adolescent morning diary_November 5, 2026_13.15.sav",
#'              out_dir = "diary qa")
#' # review diary qa/rth_diary_qa_proposals_2026-11-06.xlsx, then step 2
#' rth_diary_qa(caregiver_night    = "rth_caregiver night diary_November 5, 2026_13.14.sav",
#'              adolescent_night   = "rth_adolescent night diary_November 5, 2026_13.15.sav",
#'              adolescent_morning = "rth_adolescent morning diary_November 5, 2026_13.15.sav",
#'              proposals = "diary qa/rth_diary_qa_proposals_2026-11-06.xlsx",
#'              out_dir = "diary qa")
#' }
#' @export
rth_diary_qa <- function(caregiver_night = NULL, adolescent_night = NULL, adolescent_morning = NULL,
                         proposals = NULL, log = NULL, out_dir = ".", run_date = Sys.Date(),
                         settings = rth_diary_qa_settings()) {
  paths <- list(caregiver_night = caregiver_night, adolescent_night = adolescent_night,
                adolescent_morning = adolescent_morning)
  paths <- paths[!vapply(paths, is.null, TRUE)]
  if (!length(paths)) stop("Give at least one diary export (.sav).", call. = FALSE)
  for (p in c(unlist(paths), proposals)) if (!file.exists(p)) stop("File not found: ", p, call. = FALSE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_dir <- normalizePath(out_dir)
  if (is.null(log)) log <- file.path(out_dir, "rth_diary_qa_decisions_log.xlsx")
  run_date <- as.Date(run_date)
  s <- settings

  logged <- .qa_read_book(log, .qa_log_cols)
  dat <- lapply(names(paths), function(tp) .qa_read(paths[[tp]], tp, s))
  names(dat) <- names(paths)

  # Step 1: propose ---------------------------------------------------------------
  if (is.null(proposals)) {
    res <- lapply(names(dat), function(tp) .qa_tag(dat[[tp]], logged[[tp]], s))
    names(res) <- names(dat)
    prop_path <- file.path(out_dir, sprintf("rth_diary_qa_proposals_%s.xlsx", format(run_date)))
    rep_path <- file.path(out_dir, sprintf("rth_diary_qa_%s.html", format(run_date)))
    .qa_write_book(prop_path, lapply(res, .qa_proposal_rows), .qa_prop_cols, editable = TRUE)
    writeLines(.qa_html(res, s, Sys.time()), rep_path, useBytes = TRUE)
    lines <- vapply(names(res), function(tp) {
      cn <- .qa_counts(res[[tp]])
      sprintf("  %s: %d tagged, %d need a decision", .qa_types[[tp]]$title, cn[["tagged"]], cn[["needs"]])
    }, "")
    message("Diary QA report: ", rep_path, "\nProposals: ", prop_path, "\n", paste(lines, collapse = "\n"),
            "\nNext: review the proposals, then run again with proposals = \"", prop_path, "\".")
    return(invisible(rep_path))
  }

  # Step 2: apply -------------------------------------------------------------------
  props <- .qa_read_book(proposals, .qa_prop_cols)
  res <- list(); new_log <- logged; probs <- character()
  for (tp in names(dat)) {
    r0 <- .qa_tag(dat[[tp]], logged[[tp]], s)
    p <- .qa_check_props(props[[tp]], r0, proposals)
    new_log[[tp]] <- .qa_merge_log(logged[[tp]], p, run_date)
    r <- .qa_tag(dat[[tp]], new_log[[tp]], s)
    cl <- .qa_review_clashes(r, p)
    if (length(cl$msg))
      probs <- c(probs, sprintf("%s: a change in the proposals file puts two entries on the same date. Change final_action or final_should_date:\n    %s",
                                .qa_types[[tp]]$title, paste(cl$msg, collapse = "\n    ")))
    left <- r$x[r$x$tagged & !r$x$decided & !(r$x$responseid %in% cl$rows), , drop = FALSE]
    if (nrow(left))
      probs <- c(probs, sprintf("%s: %d tagged submission%s not in the proposals file (%s). The export may have new entries since step 1, or a reassign put two entries on the same date. Run step 1 again with these exports.",
                                .qa_types[[tp]]$title, nrow(left), if (nrow(left) == 1) " is" else "s are",
                                paste(sprintf("%s %s", left$person, .qa_ts(left$start)), collapse = "; ")))
    cf <- .qa_conflicts(r)
    if (length(cf))
      probs <- c(probs, sprintf("%s: more than one entry kept for the same date. Change final_action or final_should_date:\n    %s",
                                .qa_types[[tp]]$title, paste(cf, collapse = "\n    ")))
    res[[tp]] <- r
  }
  if (length(probs)) stop("Nothing was written.\n- ", paste(probs, collapse = "\n- "), call. = FALSE)

  .qa_write_book(log, new_log, .qa_log_cols, editable = FALSE)
  clean <- list(); lines <- character()
  for (tp in names(res)) {
    cl <- .qa_clean(res[[tp]], new_log[[tp]], s)
    path <- file.path(out_dir, sprintf("rth_%s_qa_%s.sav", tp, format(run_date)))
    haven::write_sav(cl, path)
    clean[[tp]] <- cl
    n_new <- nrow(props[[tp]])
    lines <- c(lines, sprintf("  %s: %s (%d rows, %d excluded; %d decisions added to the log, %d overrides)",
                              .qa_types[[tp]]$title, path, nrow(cl), sum(cl$exclude == 1), n_new,
                              sum(new_log[[tp]]$source[new_log[[tp]]$responseid %in% props[[tp]]$responseid] %in% "override")))
  }
  message("Decisions log updated: ", log, "\nClean data:\n", paste(lines, collapse = "\n"))
  invisible(clean)
}
