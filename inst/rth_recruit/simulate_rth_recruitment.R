# Simulated exports for testing render_rth_recruitment(). Not real data.
#
# The screener is built from the real (empty) Qualtrics shell so it has the
# same columns and answer codes; eligibility follows the survey flow's rules.
# The tracking sheet uses the real sheet's column names plus the new ones
# (screener ID, lab visit scheduled date, daily assessment counts); dates are
# written m/d/yyyy. The interview has SID and the education/income items.
#
# Usage (from the package root): Rscript inst/rth_recruit/simulate_rth_recruitment.R <screener_shell.sav>
args <- commandArgs(trailingOnly = TRUE)
shell <- haven::read_sav(args[1])
out <- "inst/rth_recruit/example_data"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
set.seed(2026)

start <- as.Date("2026-04-25")
per_week <- c(5, 23, 12, 34, 28, 12, 33, 81, 42, 35, 48, 70, 38, 21, 19, 36, 24, 35, 27, 11, 13, 29, 10, 10, 20, 78, 69)
n <- sum(per_week)
wk <- rep(seq_along(per_week), per_week)
date <- start + 7 * (wk - 1) + sample(0:6, n, TRUE)
when <- as.POSIXct(paste(date, sprintf("%02d:%02d:00", sample(8:22, n, TRUE), sample(0:59, n, TRUE))), tz = "UTC")

code <- function(v, lab) unname(attr(shell[[v]], "labels")[lab])
pick <- function(v, labs, p) code(v, sample(labs, n, TRUE, p))
lab <- function(v, x) haven::labelled(x, labels = attr(shell[[v]], "labels"), label = attr(shell[[v]], "label"))

d <- list()
d$StartDate <- when; d$EndDate <- when + 300; d$RecordedDate <- when + 300
d$Status <- lab("Status", ifelse(runif(n) < .01, 1, 0))
d$Finished <- lab("Finished", ifelse(runif(n) < .12, 0, 1))
d$ResponseId <- sprintf("R_sim%05d", seq_len(n))
d$role <- lab("role", ifelse(wk < 10 & runif(n) < .15, 1, 2))
ages <- c(as.character(8:21))
d$age <- lab("age", pick("age", ages, c(.02, .03, .05, .06, .08, .1, .12, .12, .11, .1, .07, .05, .05, .04)))
d$child_eng <- lab("child_eng", pick("child_eng", c("Yes", "No"), c(.96, .04)))
d$caregiver_eng <- lab("caregiver_eng", pick("caregiver_eng", c("Yes", "No"), c(.96, .04)))
d$medication <- lab("medication", pick("medication", c("Yes", "No"), c(.08, .92)))
d$preg <- lab("preg", pick("preg", c("Yes", "No", "N/A - My child is male"), c(.01, .55, .44)))
d$prison <- lab("prison", pick("prison", c("Yes", "No"), c(.003, .997)))
d$attend <- lab("attend", pick("attend", c("Yes", "No"), c(.8, .2)))
for (v in c("chronic", "psych", "mental", "dev")) d[[v]] <- lab(v, pick(v, c("Yes", "No"), c(.1, .9)))
for (j in 1:7) { v <- paste0("race_", j); d[[v]] <- lab(v, ifelse(runif(n) < c(.02, .08, .1, .01, .3, .55, .04)[j], 1, NA)) }
d$income <- lab("income", sample(1:9, n, TRUE, c(.05, .1, .1, .1, .1, .1, .1, .15, .2)))
src_vars <- c(Mail = "heard.where_car_1", Bus = "heard.where_car_2", Facebook = "heard.where_car_3",
              Instagram = "heard.where_car_4", Pitt = "heard.where_car_7", Halloween = "heard.where_car_8",
              Flyer = "heard.where_car_6", Other = "heard.where_car_5")
pr <- function(w) if (w >= 26) c(.03, .02, .7, .12, .03, .01, .06, .05) else c(.05, .06, .2, .35, .08, .01, .2, .08)
srcpick <- t(vapply(wk, function(w) runif(8) < pr(w) * 1.15, logical(8)))
for (j in seq_along(src_vars)) d[[src_vars[j]]] <- lab(src_vars[j], ifelse(srcpick[, j], 1, NA))
d$heard.where_car_6_TEXT <- ifelse(srcpick[, 7], sample(c("Allderdice HS", "library", "coffee shop", "pediatrician office"), n, TRUE), "")
d$heard.where_car_5_TEXT <- ifelse(srcpick[, 8], sample(c("friend", "school email", "church"), n, TRUE), "")
d$screenerid <- as.character(sample(100000:999999, n))
d$Q_TerminateFlag <- ifelse(runif(n) < .06, "Screened", "")

# eligibility, following the screener's survey flow
L <- function(v) as.character(haven::as_factor(d[[v]], levels = "labels"))
a <- suppressWarnings(as.integer(L("age")))
ok_basic <- L("child_eng") == "Yes" & L("caregiver_eng") == "Yes" & L("medication") == "No" &
  L("preg") != "Yes" & L("prison") == "No" & L("attend") == "Yes"
dx <- L("chronic") == "Yes" | L("psych") == "Yes" | L("mental") == "Yes" | L("dev") == "Yes"
st <- ifelse(ok_basic & !dx & a %in% 14:17, "caregiv_eligible_under18",
      ifelse(ok_basic & !dx & a == 18, "caregiv_eligible_18",
      ifelse(ok_basic & dx & a %in% 14:18, "caregiv_unsure_disorder", "caregiv_ineligible")))
st[runif(n) < .03] <- "caregiv_unsure"
st[runif(n) < .02 & grepl("eligible", st) & !grepl("in", st)] <- "caregiv_unsure_recaptcha"
st[d$role == 1] <- sub("^caregiv_", "ado_", st[d$role == 1])
st[as.numeric(d$Finished) == 0 | d$Q_TerminateFlag != ""] <- ""
d$eligi_status <- st

scr <- shell[0, ]
scr <- scr[rep(NA_integer_, n), ]
for (v in names(d)) scr[[v]] <- d[[v]]
haven::write_sav(scr, file.path(out, "sim_prescreen.sav"))

# Tracking sheet: about half of eligible families enroll -------------------------
rep_day <- as.Date("2026-10-30")
elig <- which(grepl("_eligible", st) & as.numeric(d$Status) == 0)
fam_i <- sort(sample(elig, round(length(elig) * .5)))
nf <- length(fam_i)
consent <- date[fam_i] + sample(3:20, nf, TRUE)
interview <- consent + sample(1:10, nf, TRUE)
survey <- ifelse(runif(nf) < .85, interview + sample(0:7, nf, TRUE), NA)
booked <- interview + sample(0:5, nf, TRUE)
lab <- booked + sample(5:20, nf, TRUE)
lab[runif(nf) < .15] <- NA                         # some never come in
booked[is.na(lab)] <- NA
f2 <- function(x) ifelse(is.na(x) | x > rep_day, "", format(as.Date(x, origin = "1970-01-01"), "%m/%d/%Y"))
f2lab <- function(x) ifelse(is.na(x), "", format(as.Date(x, origin = "1970-01-01"), "%m/%d/%Y"))   # future visits listed too
ok <- consent <= rep_day
fam_i <- fam_i[ok]; consent <- consent[ok]; interview <- interview[ok]; survey <- survey[ok]; booked <- booked[ok]; lab <- lab[ok]
nf <- length(fam_i)
famid <- 100 + seq_len(nf)
done <- !is.na(lab) & lab <= rep_day & lab <= rep_day - 18
cnt <- function(p) ifelse(done, pmin(15, rbinom(nf, 15, p)), "")
teen <- data.frame(
  `Wet Lab ID` = "", `RTH ID` = paste0(famid, "1"), `adolescent or caregiver?` = "adolescent",
  `screener ID` = d$screenerid[fam_i], email = "", `phone number` = "",
  `signed up for phone interview?` = "-", `phone interview date` = "-",
  `signed consent?` = ifelse(!is.na(lab) & lab <= rep_day, "TRUE", "FALSE"),
  `consent signed date` = f2(ifelse(!is.na(lab) & lab <= rep_day, lab, NA)),
  `signed up for lab visit?` = ifelse(is.na(lab), "FALSE", "TRUE"),
  `lab visit scheduled date` = f2(booked), `lab visit date` = f2lab(ifelse(booked <= rep_day, lab, NA)),
  `Age at Lab Visit` = ifelse(!is.na(lab) & lab <= rep_day, as.character(haven::as_factor(d$age, levels = "labels"))[fam_i], ""),
  `completed online questionnaire?` = ifelse(!is.na(lab) & lab <= rep_day, "TRUE", "FALSE"),
  `date of questionnaire completion` = f2(ifelse(!is.na(lab) & lab <= rep_day, lab, NA)),
  `night diaries completed` = cnt(.9), `morning diaries completed` = cnt(.85),
  `food photos completed` = cnt(.75), `watch nights worn` = cnt(.95),
  check.names = FALSE, stringsAsFactors = FALSE)
cg <- teen
cg$`RTH ID` <- paste0(famid, "2"); cg$`adolescent or caregiver?` <- "caregiver"
cg$`signed up for phone interview?` <- "TRUE"; cg$`phone interview date` <- f2(interview)
cg$`signed consent?` <- "TRUE"; cg$`consent signed date` <- f2(consent)
cg$`signed up for lab visit?` <- "-"; cg$`lab visit scheduled date` <- ""; cg$`lab visit date` <- ""; cg$`Age at Lab Visit` <- ""
cg$`completed online questionnaire?` <- ifelse(is.na(survey) | survey > rep_day, "FALSE", "TRUE")
cg$`date of questionnaire completion` <- f2(survey)
for (v in c("morning diaries completed", "food photos completed", "watch nights worn")) cg[[v]] <- ""
cg$`night diaries completed` <- cnt(.8)
# older families: booking dates weren't logged before Oct 1
old <- !is.na(lab) & lab < as.Date("2026-10-01")
teen$`lab visit scheduled date`[old] <- ""
trk <- rbind(teen, cg)
trk <- trk[order(trk$`RTH ID`), ]
blank <- trk[1:5, ]; blank[,] <- ""                      # rows without an RTH ID are ignored
trk <- rbind(trk, blank)
openxlsx::write.xlsx(list(tracking = trk), file.path(out, "sim_tracking.xlsx"))

# Caregiver SES interview ----------------------------------------------------------------
edu <- c("Elementary school or less" = 10, "Some high school or less" = 14,
         "High school diploma or equivalency (GED)" = 5, "Some college" = 12, "Associate's Degree" = 6,
         "Bachelor's Degree (BA, BS)" = 7, "Master's Degree (MA, MS)" = 8,
         "Doctorate or Professional Degree (PhD, MD, JD, DDS)" = 9)
cats <- c("Less than $5,000", "$5,000 - 9,999", "$10,000 - 14,999", "$20,000 - 24,999", "$40,000 - 44,999",
          "$60,000 - 69,999", "$80,000 - 89,999", "$100,000 or higher")
iv <- interview <= rep_day
n_iv <- sum(iv)
inc_exact <- ifelse(runif(n_iv) < .6, as.character(round(rlnorm(n_iv, log(65000), .7), -3)), "")
ses <- data.frame(
  StartDate = as.POSIXct(paste(interview[iv], "15:00:00"), tz = "UTC"), Status = 0, Finished = 1,
  SID = paste0(famid[iv], "2"),
  ParentSESv1_7 = haven::labelled(sample(unname(edu), n_iv, TRUE), labels = edu),
  ParentSESv1_19 = haven::labelled(ifelse(runif(n_iv) < .6, sample(unname(edu), n_iv, TRUE), NA), labels = edu),
  ParentSESv1_53 = inc_exact,
  ParentSESv1_53cat = haven::labelled(ifelse(inc_exact == "", sample(seq_along(cats), n_iv, TRUE), NA),
                                      labels = stats::setNames(seq_along(cats), cats)))
haven::write_sav(ses, file.path(out, "sim_cg_interview.sav"))
cat("screeners:", n, " families in tracker:", nf, " lab visits done:", sum(!is.na(lab) & lab <= rep_day), "\n")

# Costs & hours sheet (costs tab; same columns as the real sheet) --------------------------
before <- "Before weekly tracking: total through 9/21/26."
D <- function(x) as.Date(x, "%m/%d/%Y")
costs <- data.frame(
  date = D(c(NA, NA, NA, NA, NA, NA, NA, NA, NA,
             "9/22/2026", "9/23/2026", "9/25/2026", "9/28/2026", "9/29/2026", "10/1/2026", "10/5/2026", "10/6/2026",
             "10/8/2026", "10/13/2026", "10/14/2026", "10/17/2026", "10/17/2026", "10/20/2026", "10/21/2026",
             "10/24/2026", "10/26/2026", "10/26/2026", "10/27/2026", "10/28/2026", "10/29/2026", "10/30/2026", "10/30/2026")),
  type = c("Expense", "Time", "Expense", "Expense", "Expense", "Time", "Expense", "Expense", "Expense",
           "Time", "Time", "Time", "Expense", "Time", "Time", "Expense", "Time", "Time", "Time", "Time", "Expense", "Time",
           "Time", "Expense", "Time", "Expense", "Time", "Time", "Time", "Time", "Time", "Expense"),
  person = c(NA, "Lab manager", NA, NA, NA, "Lab manager", NA, NA, NA,
             "Lab manager", "Lab manager", "Grad student", NA, "Lab manager", "Lab manager", NA, "Lab manager", "Lab manager",
             "Lab manager", "Lab manager", NA, "Lab manager", "Lab manager", NA, "Lab manager", NA, "Lab manager",
             "Lab manager", "Grad student", "Other", "Lab manager", NA),
  source = c("Instagram", "Instagram", "Mail", "Bus ads", "Paper flyer", "Paper flyer", "General", "General", "General",
             "Facebook", "Paper flyer", "Pitt + Me", "Instagram", "Instagram", "Facebook", "Paper flyer", "Paper flyer", "Facebook",
             "Facebook", "Paper flyer", "General", "General", "Facebook", "Halloween candy sticker", "Halloween candy sticker",
             "Instagram", "Facebook", "Facebook", "Facebook", "Halloween candy sticker", "Paper flyer", "General"),
  `in person or remote` = c(NA, "Remote", NA, NA, NA, "In person", NA, NA, NA,
             "Remote", "In person", "Remote", NA, "Remote", "Remote", NA, "In person", "Remote", "Remote", "In person", NA,
             "In person", "Remote", NA, "In person", NA, "Remote", "Remote", "Remote", "In person", "In person", NA),
  hours = c(NA, 1, NA, NA, NA, 24, NA, NA, NA, 1.5, 3, 2, NA, 0.5, 1, NA, 2.5, 1, 1.5, 3, NA, 2, 1, NA, 3, NA, 0.5, 1.5, 1, 4, 2, NA),
  amount = c(1400, NA, 2520.62, 264, 85, NA, 156, 12, 16, NA, NA, NA, 350, NA, NA, 40, NA, NA, NA, NA, 25, NA, NA, 60,
             NA, 350, NA, NA, NA, 64, NA, 18.5),
  `what / item` = c("Instagram ads (4 runs x $350)", "Instagram ad setup", "Postcards / direct mail", "Bus ads", "Flyer printing",
             "Flyering", "Stickers", "Skittles", "Gatorade (2 x $8)",
             "Posted in parent groups", "Flyering Squirrel Hill", "Set up Pitt + Me listing", "Instagram ad run", "Ad setup",
             "Posted in parent groups", "Flyer reprint", "Flyering Oakland", "Posted in 4 groups", "Emailed youth centers",
             "Flyering East Liberty", "Snacks for tabling", "Tabling at library", "Posted in groups", "Halloween stickers",
             "Stickering candy bags", "Instagram ad run", "Ad setup", "Posted in groups", "Posted in groups",
             "RA helped with candy bags", "Flyering Shadyside", "Gatorade"),
  notes = c(rep(before, 9), rep(NA, 23)),
  check.names = FALSE, stringsAsFactors = FALSE)
# lab manager hours tab: one row per day Mon-Sat; recruitment columns are formulas
# in the real sheet (left empty here: the report takes recruitment from the costs tab)
days <- seq(as.Date("2026-10-17"), as.Date("2026-10-30"), by = "day")
days <- days[format(days, "%u") != "7"]
cats <- c("participant tracking", "caregiver interviews", "lab visits (dry & wet)", "lab maintenance (dry & wet)",
          "actigraphy data processing", "data coding & management", "misc.", "other")
set.seed(31)
hrs <- t(vapply(days, function(d) {
  if (format(d, "%u") == "6") return(rep(NA_real_, length(cats)))
  h <- c(2, .5, 1, .5, 1, .5, .5, 0) * sample(c(.5, 1, 1.5), length(cats), TRUE)
  h[3] <- if (format(d, "%u") %in% c("2", "4")) 3 else 0
  ifelse(h == 0, NA, round(h * 2) / 2)
}, numeric(length(cats))))
colnames(hrs) <- cats
lmh <- data.frame(date = days, day = format(days, "%a"), `in-person recruitment (from costs tab)` = NA_real_,
                  `remote recruitment (from costs tab)` = NA_real_, hrs, total = NA_real_,
                  notes = ifelse(days == as.Date("2026-10-29"), "Other: IT ticket for lab laptop", NA), check.names = FALSE)
openxlsx::write.xlsx(list(`how to use` = data.frame(x = "Simulated costs sheet (not real)."), costs = costs,
                          `lab manager hours` = lmh), file.path(out, "sim_costs.xlsx"))

# "sus bots" tab on the tracker: 3 eligible-looking screeners judged to be bots ---------------
pre <- haven::read_sav(file.path(out, "sim_prescreen.sav"))
trk_ids <- as.character(openxlsx::read.xlsx(file.path(out, "sim_tracking.xlsx"), check.names = FALSE, sep.names = " ")$`screener ID`)
cand <- as.character(pre$screenerid[grepl("_eligible_under18$", pre$eligi_status) & !as.character(pre$screenerid) %in% trk_ids])
set.seed(77)
wb <- openxlsx::loadWorkbook(file.path(out, "sim_tracking.xlsx"))
openxlsx::addWorksheet(wb, "sus bots")
openxlsx::writeData(wb, "sus bots", data.frame(`Screener ID` = sample(cand, 3), why = "odd answers, same IP",
                                               check.names = FALSE))
openxlsx::saveWorkbook(wb, file.path(out, "sim_tracking.xlsx"), overwrite = TRUE)
