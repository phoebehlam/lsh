# Participant tracking sheet ------------------------------------------------------
#
# One row per person. RTH IDs are 4 digits: the first 3 are the family, the
# last is the role (1 = teen, 2 = caregiver), e.g. 1231 / 1232. Rows without an
# RTH ID are left out. Only the columns below are read; contact details,
# compensation, etc. are ignored.

# Column names are matched loosely (case, spaces, dots and punctuation ignored).
.tracker_cols <- c(
  rth_id         = "RTH ID",
  role           = "adolescent or caregiver?",
  screener_id    = "screener ID",
  consent_date   = "consent signed date",
  interview_date = "phone interview date",
  lab_sched_date = "lab visit scheduled date",
  lab_date       = "lab visit date",
  survey_date    = "date of questionnaire completion",
  age_lab        = "Age at Lab Visit",
  n_night        = "night diaries completed",
  n_morning      = "morning diaries completed",
  n_food         = "food photos completed",
  n_watch        = "watch nights worn")
.tracker_dates <- c("consent_date", "interview_date", "lab_sched_date", "lab_date", "survey_date")
.key <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

#' Read the participant tracking sheet
#'
#' @param tracker Google Sheets link (read with googlesheets4), a downloaded
#'   .xlsx, or a data frame.
#' @param sheet Tab name.
#' @return One row per person with an RTH ID. Stops with a list of cells to fix
#'   if a date isn't written like 10/24/2026.
#' @keywords internal
read_tracker <- function(tracker, sheet = "tracking") {
  raw <- .flatten_sheet(.tracker_raw(tracker, sheet))
  k <- .key(names(raw))
  pick <- function(nm) { j <- match(.key(.tracker_cols[[nm]]), k); if (is.na(j)) NULL else raw[[j]] }
  if (is.null(pick("rth_id"))) stop("Can't find an 'RTH ID' column in the tracking sheet.", call. = FALSE)

  sheet_row <- seq_len(nrow(raw)) + 1                       # row 1 holds the headers
  id_raw <- trimws(pick("rth_id"))
  keep <- !is.na(id_raw) & id_raw != "" & !id_raw %in% c("-", "NA", "N/A")
  get <- function(nm) { v <- pick(nm); if (is.null(v)) rep(NA_character_, sum(keep)) else trimws(v[keep]) }

  out <- tibble::tibble(sheet_row = sheet_row[keep], rth_id_raw = id_raw[keep])
  out$rth_id <- ifelse(grepl("^[0-9]{4}$", sub("\\.0+$", "", out$rth_id_raw)), sub("\\.0+$", "", out$rth_id_raw), NA_character_)
  out$family <- substr(out$rth_id, 1, 3)
  out$role <- ifelse(substr(out$rth_id, 4, 4) == "1", "teen", ifelse(substr(out$rth_id, 4, 4) == "2", "caregiver", NA))
  out$role_sheet <- tolower(get("role"))
  out$screener_id <- .clean_id(get("screener_id"))
  out$screener_id[out$screener_id %in% c("-", "na", "n/a")] <- NA

  bad <- list()
  for (nm in .tracker_dates) {
    v <- get(nm)
    d <- .parse_slash_date(v)
    wrong <- which(is.na(d) & !.is_blank(v))
    if (length(wrong)) bad[[nm]] <- sprintf('  "%s", row %d (RTH ID %s): "%s"', .tracker_cols[[nm]],
                                            out$sheet_row[wrong], out$rth_id_raw[wrong], v[wrong])
    out[[nm]] <- d
  }
  bad <- unlist(bad, use.names = FALSE)
  if (length(bad)) stop("Please fix these dates in the tracking sheet (write them like 10/24/2026):\n",
                        paste(utils::head(bad, 30), collapse = "\n"),
                        if (length(bad) > 30) sprintf("\n  ...and %d more", length(bad) - 30) else "", call. = FALSE)

  num <- function(nm) suppressWarnings(as.numeric(get(nm)))
  out$age_lab <- num("age_lab")
  for (nm in c("n_night", "n_morning", "n_food", "n_watch")) out[[nm]] <- num(nm)
  attr(out, "has") <- vapply(names(.tracker_cols), function(nm) !is.null(pick(nm)), logical(1))
  out
}

.is_blank <- function(v) is.na(v) | trimws(v) %in% c("", "-", "NA", "N/A", "n/a", "na")

# Dates written with slashes (10/24/2026 or 10/24/26). A bare number is an
# Excel date (from a downloaded .xlsx), which is fine too.
.parse_slash_date <- function(v) {
  v <- trimws(v)
  out <- as.Date(rep(NA_character_, length(v)))
  s <- !is.na(v) & grepl("^[0-9]{1,2}/[0-9]{1,2}/([0-9]{2}|[0-9]{4})$", v)
  yy <- s & grepl("/[0-9]{2}$", v)
  out[s & !yy] <- as.Date(v[s & !yy], "%m/%d/%Y")
  out[yy] <- as.Date(v[yy], "%m/%d/%y")
  serial <- !is.na(v) & grepl("^[0-9]{5}(\\.[0-9]+)?$", v)
  out[serial] <- as.Date(floor(as.numeric(v[serial])), origin = "1899-12-30")
  out
}

# Every column as text; dates (from Google Sheets) written m/d/yyyy
.flatten_sheet <- function(raw) {
  as.data.frame(lapply(raw, function(v) {
    if (is.list(v)) v <- vapply(v, function(e) {
      if (length(e) == 0 || all(is.na(e))) return(NA_character_)
      e <- e[[1]]
      if (inherits(e, "Date") || inherits(e, "POSIXt")) format(as.Date(e), "%m/%d/%Y") else as.character(e)
    }, "")
    if (inherits(v, "Date") || inherits(v, "POSIXt")) v <- format(as.Date(v), "%m/%d/%Y")
    as.character(v)
  }), check.names = FALSE, stringsAsFactors = FALSE)
}

# Google Sheets link, .xlsx or data frame. With must_have_tab, an .xlsx without
# that tab is an error instead of falling back to the first tab.
.tracker_raw <- function(tracker, sheet, must_have_tab = FALSE) {
  if (is.data.frame(tracker)) return(tracker)
  if (grepl("^https?://", tracker) || (!file.exists(tracker) && !grepl("\\.xlsx$", tracker))) {
    if (!requireNamespace("googlesheets4", quietly = TRUE))
      stop("To read the Google Sheet directly, install googlesheets4 first: install.packages(\"googlesheets4\")", call. = FALSE)
    # The first time, a browser window asks you to sign in with your CMU Google account.
    # Real date cells come back as dates (any display format is fine); dates
    # typed as text must use slashes.
    return(googlesheets4::read_sheet(tracker, sheet = sheet, .name_repair = "minimal"))
  }
  if (!file.exists(tracker)) stop("File not found: ", tracker, call. = FALSE)
  shs <- openxlsx::getSheetNames(tracker)
  if (must_have_tab && !sheet %in% shs)
    stop(sprintf("No '%s' tab in %s (tabs: %s).", sheet, basename(tracker), paste(shs, collapse = ", ")), call. = FALSE)
  openxlsx::read.xlsx(tracker, sheet = if (sheet %in% shs) sheet else 1, detectDates = FALSE,
                      check.names = FALSE, sep.names = " ", skipEmptyRows = FALSE)
}

#' One row per family: screener ID and the dates of each step
#'
#' Caregiver steps (consent, interview, caregiver survey) come from the
#' caregiver row (RTH ID ending in 2); lab visit dates from the teen row
#' (ending in 1). If a family has only one row, that row is used.
#' @keywords internal
tracker_families <- function(tr, report_date) {
  tr <- tr[!is.na(tr$family), , drop = FALSE]
  fams <- unique(tr$family)
  if (!length(fams)) {
    e <- as.Date(character())
    return(tibble::tibble(family = character(), screener_id = character(), consent = e, interview = e,
                          cg_survey = e, lab_sched = e, lab_date = e, lab_done = logical()))
  }
  first <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else x[NA_integer_][1] }
  pickrole <- function(f, r, v) {
    rows <- tr[tr$family == f, , drop = FALSE]
    pref <- rows[[v]][rows$role %in% r]
    if (all(is.na(pref))) first(rows[[v]]) else first(pref)
  }
  d <- function(f, r, v) as.Date(pickrole(f, r, v))
  out <- tibble::tibble(
    family = fams,
    screener_id = vapply(fams, function(f) first(tr$screener_id[tr$family == f]), ""),
    consent = do.call(c, lapply(fams, d, "caregiver", "consent_date")),
    interview = do.call(c, lapply(fams, d, "caregiver", "interview_date")),
    cg_survey = do.call(c, lapply(fams, function(f) as.Date(first(tr$survey_date[tr$family == f & tr$role %in% "caregiver"])))),
    lab_sched = do.call(c, lapply(fams, d, "teen", "lab_sched_date")),
    lab_date = do.call(c, lapply(fams, d, "teen", "lab_date")))
  out$lab_done <- !is.na(out$lab_date) & out$lab_date <= report_date
  out
}

# Interview (caregiver SES) ---------------------------------------------------------------

.edu_order <- c("Elementary school or less", "Some high school or less",
                "High school diploma or equivalency (GED)", "Some college", "Associate's Degree",
                "Bachelor's Degree", "Master's Degree", "Doctorate or Professional Degree")

#' Read the caregiver SES interview: highest education of both caregivers and
#' household income, one row per family
#' @keywords internal
read_interview_ses <- function(path) {
  raw <- .drop_previews(haven::read_sav(path))
  sid <- .find_col(raw, "SID")
  if (is.na(sid)) stop("Can't find the 'SID' column in ", basename(path), call. = FALSE)
  rth <- .clean_id(raw[[sid]], pad = 4)
  rank_of <- function(v) {
    l <- .lab(if (v %in% names(raw)) raw[[v]] else rep(NA, nrow(raw)))
    l <- gsub("\u2019", "'", l)
    vapply(l, function(x) if (is.na(x)) NA_integer_ else {
      m <- which(vapply(.edu_order, function(o) startsWith(tolower(x), tolower(o)), logical(1)))
      if (length(m)) m[1] else NA_integer_ }, 0L)
  }
  hi <- suppressWarnings(pmax(rank_of("ParentSESv1_7"), rank_of("ParentSESv1_19"), na.rm = TRUE))
  # Income: the exact amount if it can be read, otherwise the bracket's lower end
  exact <- if ("ParentSESv1_53" %in% names(raw)) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(raw$ParentSESv1_53)))) else rep(NA_real_, nrow(raw))
  catl <- .lab(if ("ParentSESv1_53cat" %in% names(raw)) raw$ParentSESv1_53cat else rep(NA, nrow(raw)))
  low <- suppressWarnings(as.numeric(gsub("[^0-9]", "", sub("^Less than.*", "0", sub("[\u2013-].*$", "", catl)))))
  inc <- ifelse(!is.na(exact) & exact > 0, exact, low)
  out <- tibble::tibble(family = substr(rth, 1, 3), education = .edu_order[hi], income = inc)
  out <- out[!is.na(out$family), , drop = FALSE]
  out[!duplicated(out$family), , drop = FALSE]
}

# Race/ethnicity scoring ---------------------------------------------------------------------

#' Score the select-all race/ethnicity item
#'
#' One choice: "Monoracial <race>". More than one: "Multiracial Black" if Black
#' is among them; otherwise "Multiracial Hispanic/Latino" if Hispanic/Latino is;
#' otherwise "Multiracial Asian" if Asian is; otherwise "Multiracial, another race".
#' @keywords internal
score_race <- function(race) {
  nm <- colnames(race)
  short <- c("American Indian or Alaska Native" = "American Indian/Alaska Native",
             "Native Hawaiian or Other Pacific Islander" = "Native Hawaiian/Pacific Islander",
             "Black or African American" = "Black")
  lab <- ifelse(nm %in% names(short), short[nm], nm)
  is <- function(p) { j <- grep(p, nm, ignore.case = TRUE); if (length(j)) race[, j[1]] else rep(FALSE, nrow(race)) }
  n <- rowSums(race)
  one <- vapply(seq_len(nrow(race)), function(i) if (n[i] == 1) lab[which(race[i, ])] else NA_character_, "")
  dplyr::case_when(
    n == 0 ~ "Not answered",
    n == 1 ~ paste("Monoracial", one),
    is("black") ~ "Multiracial Black",
    is("hispanic") ~ "Multiracial Hispanic/Latino",
    is("asian") ~ "Multiracial Asian",
    TRUE ~ "Multiracial, another race")
}
