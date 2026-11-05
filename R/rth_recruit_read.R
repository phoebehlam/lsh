# Readers for the weekly recruitment report ------------------------------------
#
# Every reader works on the Qualtrics SPSS (.sav) export as downloaded. Answers
# are matched by their labels ("Yes", "No", ...) rather than codes, because the
# screener codes Yes/No differently from question to question.

# Qualtrics response types kept: real responses (0), imported (4), offline (16).
.rth_keep_status <- c(0, 4, 16)

.lab <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x, levels = "labels")
  out <- trimws(as.character(x))
  out[out == ""] <- NA_character_
  out
}

# Find a column by name, ignoring case and dots/underscores ("screener_id",
# "ScreenerID", "screenerid" all match).
.find_col <- function(df, name) {
  key <- function(x) tolower(gsub("[._ ]", "", x))
  hit <- names(df)[key(names(df)) == key(name)]
  if (length(hit)) hit[1] else NA_character_
}

# IDs as clean text: no spaces, no trailing ".0"; 4-digit RTH IDs keep leading zeros.
.clean_id <- function(x, pad = 0) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x[x %in% c("", "NA")] <- NA_character_
  if (pad > 0) {
    num <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < pad
    x[num] <- formatC(as.integer(x[num]), width = pad, flag = "0")
  }
  x
}

.drop_previews <- function(df) {
  if (!"Status" %in% names(df)) return(df)
  df[as.numeric(df$Status) %in% .rth_keep_status, , drop = FALSE]
}

# Study weeks run Saturday-Friday; week 1 starts on study_start (Apr 25, 2026).
rth_study_week <- function(date, study_start) {
  as.integer(floor(as.numeric(as.Date(date) - as.Date(study_start)) / 7) + 1)
}
.week_start <- function(week, study_start) as.Date(study_start) + 7 * (week - 1)
.week_label <- function(week, study_start) {
  s <- .week_start(week, study_start)
  sprintf("%s&ndash;%s", .md(s), .md(s + 6))
}
# 10/4 and Oct 4, 2026 without leading zeros. ("%-d" works on Linux but not in
# R on macOS, so build them by hand.)
.md <- function(d) ifelse(is.na(d), NA_character_,
                          paste0(as.integer(format(d, "%m")), "/", as.integer(format(d, "%d"))))
.long_date <- function(d) ifelse(is.na(d), NA_character_,
                                 paste0(format(d, "%b"), " ", as.integer(format(d, "%d")), ", ", format(d, "%Y")))

#' Read the Rise to Health prescreen (screener) export
#'
#' One row per screener response, with the screening outcome, ineligible
#' reasons, where they heard about the study, and demographics. Names, emails
#' and phone numbers are not kept.
#' @param path Path to the screener .sav.
#' @param approved Screener IDs of families in the tracking sheet; they count as
#'   eligible even if the screener said otherwise (e.g. approved after review).
#' @keywords internal
.bot_cut <- 0.5

read_screener <- function(path, approved = character()) {
  raw <- haven::read_sav(path)
  raw <- .drop_previews(raw)
  n <- nrow(raw)
  col <- function(v) if (v %in% names(raw)) raw[[v]] else rep(NA, n)

  status <- tolower(trimws(as.character(col("eligi_status"))))
  status[is.na(status)] <- ""
  finished <- as.numeric(col("Finished")) %in% 1
  term <- trimws(as.character(col("Q_TerminateFlag")))
  screened_out <- !is.na(term) & term != ""

  outcome <- dplyr::case_when(
    screened_out ~ "Screened out (blocked IP/location)",
    grepl("_eligible_under18$", status) ~ "Eligible (child 14-17)",
    grepl("_eligible_18$", status) ~ "Eligible (child 18)",
    grepl("_unsure_disorder$", status) ~ "Needs review: diagnosis reported",
    grepl("_unsure_recaptcha$", status) ~ "Possible bot (low reCAPTCHA score)",
    grepl("_unsure_duplicate$", status) ~ "Needs review: possible repeat screener",
    grepl("_unsure$", status) ~ "Incomplete answers",
    grepl("_ineligible$", status) ~ "Ineligible",
    !finished ~ "Didn't finish screener",
    TRUE ~ "No status recorded")

  # Possible bot = reCAPTCHA score below 0.5, whatever the answers and whether
  # or not the screener was finished (the survey flow only flags otherwise-
  # eligible caregivers). Without the score in the export, only the survey
  # flow's flag is available. Families later enrolled count as real (below).
  rc <- suppressWarnings(as.numeric(col("Q_RecaptchaScore")))
  outcome[!is.na(rc) & rc < .bot_cut & !screened_out] <- "Possible bot (low reCAPTCHA score)"

  sid <- .clean_id(col("screenerid"))
  rid <- as.character(col("ResponseId"))
  ok_review <- ((!is.na(sid) & sid %in% .clean_id(approved)) | (!is.na(rid) & rid %in% .clean_id(approved))) & !screened_out
  outcome[ok_review & !grepl("^Eligible", outcome)] <- "Not marked eligible, but enrolled"

  # Ineligible reasons, using the same rules as the screener's survey flow
  age <- .lab(col("age"))
  age_num <- suppressWarnings(as.integer(age))
  reasons <- cbind(
    `Can't attend lab visit at CMU` = .lab(col("attend")) %in% "No",
    `Child's age outside 14-18` = !is.na(age) & (is.na(age_num) | age_num < 14 | age_num > 18),
    `Anti-inflammatory / hormonal medication` = .lab(col("medication")) %in% "Yes",
    `Child not fluent in English` = .lab(col("child_eng")) %in% "No",
    `Caregiver not fluent in English` = .lab(col("caregiver_eng")) %in% "No",
    `Pregnant / breastfeeding` = .lab(col("preg")) %in% "Yes",
    `In prison` = .lab(col("prison")) %in% "Yes")
  inel <- outcome == "Ineligible"
  reasons[!inel, ] <- FALSE
  other <- inel & rowSums(reasons) == 0
  reasons <- cbind(reasons, `Other / not recorded` = other)

  # Where they heard about us (caregiver and older adolescent versions merged)
  hv <- grep("^heard\\.where_(car|ado)_[0-9]+$", names(raw), value = TRUE)
  src_name <- vapply(hv, function(v) .source_name(attr(raw[[v]], "label"), v), "")
  srcs <- unique(src_name)
  sources <- vapply(srcs, function(s) {
    m <- as.matrix(as.data.frame(lapply(hv[src_name == s], function(v) !is.na(raw[[v]]) & as.numeric(raw[[v]]) == 1)))
    rowSums(m) > 0
  }, logical(n))
  if (!is.matrix(sources)) sources <- matrix(sources, nrow = n, dimnames = list(NULL, srcs))
  tv <- grep("^heard\\.where_(car|ado)_[0-9]+_TEXT$", names(raw), value = TRUE)
  writein <- do.call(rbind, c(list(data.frame(source = character(), text = character(), row = integer())),
    lapply(tv, function(v) {
      txt <- trimws(as.character(raw[[v]]))
      keep <- which(!is.na(txt) & txt != "")
      data.frame(source = rep(.source_name(attr(raw[[sub("_TEXT$", "", v)]], "label"), v), length(keep)),
                 text = .scrub_contact(txt[keep]), row = keep)
    })))

  # Demographics
  rv <- grep("^race_[0-9]+$", names(raw), value = TRUE)
  race <- vapply(rv, function(v) !is.na(raw[[v]]) & as.numeric(raw[[v]]) == 1, logical(n))
  if (!is.matrix(race)) race <- matrix(race, nrow = n)
  colnames(race) <- vapply(rv, function(v) sub(".*Selected Choice ", "", attr(raw[[v]], "label") %||% v), "")

  start <- if ("StartDate" %in% names(raw)) raw$StartDate else rep(as.POSIXct(NA), n)
  out <- tibble::tibble(
    response_id = as.character(col("ResponseId")),
    screener_id = sid,
    date = .wall_date(start),
    role = .lab(col("role")),
    finished = finished,
    screened_out = screened_out,
    outcome = outcome,
    eligible = grepl("^Eligible", outcome) | outcome == "Not marked eligible, but enrolled",
    bot = outcome == "Possible bot (low reCAPTCHA score)",
    # finished, and not a possible bot: the base for every %
    completed = !screened_out & !outcome %in% c("Didn't finish screener", "Possible bot (low reCAPTCHA score)"),
    age = age,
    income = .lab(col("income")),
    race_group = score_race(race),
    recaptcha = rc)
  attr(out, "reasons") <- reasons
  attr(out, "sources") <- sources
  attr(out, "writein") <- writein
  attr(out, "race") <- race
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# "Where did you hear about us? - Selected Choice Paper flyer (if so, ...)" -> "Paper flyer"
.source_name <- function(label, fallback = "") {
  if (is.null(label) || is.na(label)) return(fallback)
  x <- sub(".*Selected Choice ", "", label)
  x <- sub("^Where did you hear about us\\? - ", "", x)
  x <- sub(" - Text$", "", x)
  x <- sub("\\s*\\(.*$", "", x)
  x <- sub(",.*$", "", x)
  x <- trimws(x)
  if (tolower(x) == "bus ads") x <- "Bus ads"
  if (tolower(x) == "halloween candy sticker") x <- "Halloween candy sticker"
  x
}


# Source groups -----------------------------------------------------------------------

.rth_source_groups <- c("Paper flyer" = "Local recruitment", "Mail" = "Local recruitment",
                        "Bus ads" = "Local recruitment", "Halloween candy sticker" = "Local recruitment",
                        "Candy / Gatorade Sticker" = "Local recruitment")

# Rename sources to their group (names matched loosely); others stay as they are
.map_source <- function(x, groups) {
  if (!length(groups)) return(x)
  g <- unname(groups[match(.key(x), .key(names(groups)))])
  ifelse(is.na(g), x, g)
}

# Combine the screener's source columns into groups: a family that picked
# Mail and Paper flyer counts once for Local recruitment
.group_sources <- function(scr, groups) {
  src <- attr(scr, "sources")
  if (!length(groups) || !ncol(src)) return(scr)
  grp <- .map_source(colnames(src), groups)
  out <- vapply(unique(grp), function(g) rowSums(src[, grp == g, drop = FALSE]) > 0, logical(nrow(src)))
  if (!is.matrix(out)) out <- matrix(out, nrow = nrow(src), dimnames = list(NULL, unique(grp)))
  attr(scr, "sources") <- out
  scr
}

# Write-ins can contain contact details (e.g. "referred by <name> <email>"):
# never let emails, phone numbers or web/social handles reach the report
.scrub_contact <- function(x) {
  x <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "[email removed]", x, perl = TRUE)
  x <- gsub("(\\+?1[ .-]?)?\\(?[0-9]{3}\\)?[ .-]?[0-9]{3}[ .-]?[0-9]{4}", "[phone removed]", x, perl = TRUE)
  x <- gsub("(^|\\s)@[A-Za-z0-9._]{2,}", "\\1[handle removed]", x, perl = TRUE)
  # "referred (to) by / from Jane Doe" -> "referred (to) by [name removed]"
  gsub("(?i)(refer+ed(?:\\s+to)?\\s+(?:by|from)\\s+)(?!\\[)(?!a friend|my |friend|family)[A-Za-z'.-]+(?:\\s+(?!\\[)[A-Za-z'.-]+)?",
       "\\1[name removed]", x, perl = TRUE)
}
