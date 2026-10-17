#' IDF adult cutoff for waist circumference
#'
#' Categorizes adult waist circumference (WC) as below (0) or
#' at/above (1) the cutoff for central obesity based on IDF guidelines.
#'
#' @param sex Character. Biological sex, must be either "male" or "female".
#' @param wc Numeric. Measured waist circumference (cm).
#' @param race Character. Ethnicity group for cutoff values.
#'   Must be one of "all", "europid", "south_asian",
#'   "chinese", "japanese", "latino", "african", or "middle_east".
#'   Defaults to "all" if missing.
#'
#' @return Integer 0/1. Returns 1 if WC is at or above the cutoff
#'   for given sex and ethnicity; otherwise 0.
#'
#' @examples
#' wc_idfadults("male", 95)                     # defaults to "all"
#' wc_idfadults("female", 79, race = "europid")
#' wc_idfadults("female", 81, race = "south_asian")
#'
wc_idfadults <- function(sex, wc, race = "all") {
  # validate sex
  if (!(sex %in% c("male", "female"))) stop("sex must be 'male' or 'female'")
  
  # default race if missing
  if (is.null(race)) race <- "all"
  
  valid_races <- c("all", "europid", "south_asian", 
                   "chinese", "japanese", "latino", 
                   "african", "middle_east")
  if (!(race %in% valid_races)) {
    stop(paste("race must be one of:", paste(valid_races, collapse = ", ")))
  }
  
  # cutoff table
  cutoffs <- list(
    all         = c(male = 102, female = 88),  # ATP III generic (race non-specific)
    europid     = c(male = 94,  female = 80),
    south_asian = c(male = 90,  female = 80),
    chinese     = c(male = 90,  female = 80),
    japanese    = c(male = 90,  female = 80),
    latino      = c(male = 90,  female = 80),  # South/Central Americans → South Asian
    african     = c(male = 94,  female = 80),  # Sub-Saharan Africans → Europid
    middle_east = c(male = 94,  female = 80)   # Eastern Med/Middle East → Europid
  )
  
  cutoff <- cutoffs[[race]][sex]
  return(ifelse(wc >= cutoff, 1, 0))
}
