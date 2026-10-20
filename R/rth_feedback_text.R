# Sentence builders. descriptives. 
.pred_text <- list(
  stress_n = list(more = "reported one more unpleasant event",
                  general = "On days with more unpleasant events",
                  noun = "unpleasant events"),
  pos_n    = list(more = "reported one more positive event",
                  general = "On days with more positive events",
                  noun = "positive events"),
  help_n   = list(more = "helped someone one more time",
                  general = "On days when you helped others more",
                  noun = "helping others"),
  neg_pomp = list(more = "rated your negative mood 10 points higher (out of 100)",
                  general = "On days when your negative mood was higher",
                  noun = "negative mood"),
  pos_pomp = list(more = "rated your positive mood 10 points higher (out of 100)",
                  general = "On days when your positive mood was higher",
                  noun = "positive mood")
)

.out_clause <- function(outcome, est) {
  up <- est > 0; a <- abs(est)
  switch(outcome,
    tst = sprintf("you tended to sleep about %s %s that night",
                  fmt_hm(a), if (up) "more" else "less"),
    se = sprintf("your sleep efficiency tended to be about %s %s %s that night",
                 round(a), if (round(a) == 1) "point" else "points",
                 if (up) "higher" else "lower"),
    sol = sprintf("you tended to take about %s %s to fall asleep that night",
                  fmt_hm(a), if (up) "longer" else "less"),
    awakenings = sprintf("you tended to wake up about %s %s %s that night",
                         format(round(a, 1), nsmall = 1),
                         if (up) "more" else "fewer",
                         if (round(a, 1) == 1) "time" else "times"),
    neg_pomp = sprintf("your negative mood that day tended to be about %s points %s",
                       round(a), if (up) "higher" else "lower"),
    pos_pomp = sprintf("your positive mood that day tended to be about %s points %s",
                       round(a), if (up) "higher" else "lower"))
}

.out_noun <- c(tst = "how long you slept", se = "your sleep efficiency",
               sol = "your sleep latency (time to fall asleep)",
               awakenings = "how often you woke up",
               neg_pomp = "your negative mood", pos_pomp = "your positive mood")

#' @keywords internal
link_sentence <- function(predictor, outcome, est, status) {
  p <- .pred_text[[predictor]]
  switch(status,
    shown = sprintf("On days when you %s, %s.", p$more, .out_clause(outcome, est)),
    about_same = sprintf("%s, %s stayed about the same.", p$general, .out_noun[[outcome]]),
    too_few_days = sprintf("There weren't enough days with both pieces of information to look at %s and %s.",
                           p$noun, sub("^your ", "", .out_noun[[outcome]])),
    no_variation = sprintf("Your %s didn't change enough from day to day to look at this pattern.",
                           sub("^helping others$", "helping", p$noun)))
}

# Short text for a grid cell.
#' @keywords internal
link_cell <- function(outcome, est, status) {
  if (status == "about_same") return("&asymp;&nbsp;same")
  if (status %in% c("too_few_days", "no_variation")) return("&mdash;")
  sgn <- if (est > 0) "+" else "&minus;"; a <- abs(est)
  switch(outcome,
    tst = paste0(sgn, round(a), " min"),
    sol = paste0(sgn, round(a), " min"),
    se = paste0(sgn, round(a), " pt"),
    awakenings = paste0(sgn, format(round(a, 1), nsmall = 1)),
    neg_pomp = paste0(sgn, round(a), " pts"),
    pos_pomp = paste0(sgn, round(a), " pts"))
}

#' @keywords internal
coherence_sentence <- function(measure, mean_diff, n) {
  if (is.na(mean_diff) || n == 0) {
    return(sprintf("There weren't enough nights with both your report and the watch to compare %s.", measure))
  }
  a <- abs(mean_diff)
  main <- if (a < 5) {
    sprintf("Your reported %s <strong>closely matched</strong> the watch (within about %d minutes on average).",
            measure, max(1, round(a)))
  } else {
    word <- switch(measure,
      "bedtime" = if (mean_diff > 0) "later" else "earlier",
      "wake time" = if (mean_diff > 0) "later" else "earlier",
      "time in bed" = if (mean_diff > 0) "more" else "less",
      "time to fall asleep" = if (mean_diff > 0) "longer" else "shorter")
    lead <- switch(measure,
      "bedtime" = "you reported getting into bed <strong>about %s %s</strong> than the watch recorded",
      "wake time" = "you reported waking up <strong>about %s %s</strong> than the watch recorded",
      "time in bed" = "you reported <strong>about %s %s time in bed</strong> than the watch recorded",
      "time to fall asleep" = "you reported taking <strong>about %s %s</strong> to fall asleep than the watch estimated")
    paste0("On average, ", sprintf(lead, fmt_hm(a), word), ".")
  }
  main
}

# One plain-language sentence on whether self-reports rose and fell with the
# watch across nights (no correlation coefficients shown to teens).
#' @keywords internal
coherence_tracking <- function(coh) {
  x <- coh[!is.na(coh$r), ]
  if (!nrow(x)) return(NULL)
  how <- ifelse(x$r >= .7, "closely", ifelse(x$r >= .4, "somewhat", "only loosely"))
  if (length(unique(how)) == 1) {
    return(sprintf("From night to night, your diary and the watch went up and down together <strong>%s</strong>: on nights the watch showed a later bedtime or more time in bed, your diary %s.",
                   how[1], if (how[1] == "only loosely") "often didn't" else "usually did too"))
  }
  parts <- sprintf("%s <strong>%s</strong>", x$measure, how)
  lst <- paste(paste(parts[-length(parts)], collapse = ", "), "and", parts[length(parts)])
  paste0("From night to night, your diary and the watch went up and down together for ", lst, ".")
}
