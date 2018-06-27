#' Get week number from julian date or day in year
#'
#'Many ALMaSS outputs has the simulation day number as it's only time indicator.
#'Often one would want to compute or plot stuff using dates. This function
#' translates from either julian date or day in year to week
#' @param day integer The simulation day or day in year from ALMaSS
#'
#' @return week as integer
#' @export
#'
get_week <- function(day) {
  if (missing(day)) {
    stop("No simulation day supplied")
  }
  day <- as.Date(day, origin = as.Date("2009-01-01"))
  the_week <- as.integer(lubridate::week(day))
  return(the_week)
}
