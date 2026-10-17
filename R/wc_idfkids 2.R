#' IDF children and adolescent cutoff for waist circumference
#'
#' This function categorizes waist circumference (WC) as
#' below (0) or at/above (1) the 90th percentile cutoff for
#' children and adolescents aged 2–18 years, based on
#' International Diabetes Federation (IDF) guidelines.
#'
#' @param age Integer. Age in years (must be between 2 and 18).
#' @param sex Character. Biological sex, must be either `"male"` or `"female"`.
#' @param wc Numeric. Measured waist circumference (cm).
#' @param race Character (optional). Population-specific cutoffs to use. 
#'   Must be one of `"all"` (default, all-race combined), `"white"`, 
#'   `"black"`, or `"latino"`.
#'
#' @return Integer 0/1. Returns `1` if WC is at or above the
#'   90th percentile cutoff for given age, sex, and race; otherwise `0`.
#'
#' @examples
#' # Example: 10-year-old boy, WC = 78 cm, all-race cutoff
#' wc_idfkids(age = 10, sex = "male", wc = 78)
#'
#' # Example: 10-year-old Black boy, WC = 78 cm
#' wc_idfkids(age = 10, sex = "male", wc = 78, race = "black")
#'
#' # Example: 12-year-old Latina girl, WC = 80 cm
#' wc_idfkids(age = 12, sex = "female", wc = 80, race = "latino")
#'
#' # Example: 15-year-old girl, WC = 93 cm, unspecified race (defaults to all)
#' wc_idfkids(age = 15, sex = "female", wc = 93)
#'
#' @importFrom magrittr "%>%"
#' @export
wc_idfkids <- function(age, sex, wc, race = "all") {
  # if race missing, default to "all"
  if (is.null(race)) race <- "all"
  
  # check inputs
  if (!(sex %in% c("male", "female"))) stop("sex must be 'male' or 'female'")
  if (!(race %in% c("all", "white", "black", "latino"))) stop("race must be one of: 'all','white','black','latino'")
  if (!(age %in% 2:18)) stop("age must be between 2 and 18")
  
  # ages
  ages <- 2:18
  
  # cut-offs
  # all race combined: boys 90th percentile wc values
  boys_wc_90_all <- c(
    50.8, 54.2, 57.6, 61.0, 64.4, 67.8, 71.2, 74.6, 78.0,
    81.4, 84.8, 88.2, 91.6, 95.0, 98.4, 101.8, 105.2)
  
  # all race combined: girl 90th percentile wc values
  girls_wc_90_all <- c(
    52.2, 55.3, 58.3, 61.4, 64.4, 67.5, 70.5, 73.6, 76.6,
    79.7, 82.7, 85.8, 88.8, 91.9, 94.9, 98.0, 101.0)
  
  # white boys 90th percentile wc values
  boys_wc_90_white <- c(
    50.6, 54.0, 57.4, 60.8, 64.2, 67.6, 71.0, 74.3, 77.7,
    81.1, 84.5, 87.9, 91.3, 94.7, 98.1, 101.5, 104.9)
  
  # white girls 90th percentile wc values
  girls_wc_90_white <- c(
    52.5, 55.4, 58.2, 61.1, 64.0, 66.8, 69.7, 72.6, 75.5,
    78.3, 81.2, 84.1, 86.9, 89.8, 92.7, 95.5, 98.4)
  
  # black boys 90th percentile wc values
  boys_wc_90_black <- c(
    50.0, 53.2, 56.4, 59.6, 62.8, 66.1, 69.3, 72.5, 75.7,
    78.9, 82.1, 85.3, 88.5, 91.7, 94.9, 98.2, 101.4)
  
  # black girls 90th percentile wc values
  girls_wc_90_black <- c(
    50.1, 53.8, 57.5, 61.1, 64.8, 68.5, 72.2, 75.8, 79.5,
    83.2, 86.9, 90.5, 94.2, 97.9, 101.6, 105.2, 108.9)
  
  # latino boys 90th percentile wc values
  boys_wc_90_latino <- c(
    53.2, 56.7, 60.2, 63.6, 67.1, 70.6, 74.1, 77.6, 81.0,
    84.5, 88.0, 91.5, 95.0, 98.4, 101.9, 105.4, 108.9)
  
  # latina girls 90th percentile wc values
  girls_wc_90_latino <- c(
    53.5, 56.7, 59.9, 63.0, 66.2, 69.4, 72.6, 75.8, 78.9,
    82.1, 85.3, 88.5, 91.7, 94.8, 98.0, 101.2, 104.4)
  
  # index for given age
  idx <- match(age, ages)
  
  # select cutoff
  if (sex == "male") {
    cutoff <- switch(race,
                     "all"    = boys_wc_90_all[idx],
                     "white"  = boys_wc_90_white[idx],
                     "black"  = boys_wc_90_black[idx],
                     "latino" = boys_wc_90_latino[idx]
    )
  } else {
    cutoff <- switch(race,
                     "all"    = girls_wc_90_all[idx],
                     "white"  = girls_wc_90_white[idx],
                     "black"  = girls_wc_90_black[idx],
                     "latino" = girls_wc_90_latino[idx]
    )
  }
  
  # return category
  return(ifelse(wc >= cutoff, 1, 0))
}