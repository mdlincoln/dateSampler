#' Predicate functions
#'
#' Predicate functions must take a year, month, and day integer as its
#' parameters, and return TRUE if the date is valid. Allows one to construct
#' complex date restrictions, such as only generating dates that are Mondays,
#' etc.
#'
#' @name predicate
#'
#' @param y Integer. Candidate year.
#' @param m Integer. Candidate month.
#' @param d Integer. Candidate day.
#'
#' @return \code{TRUE} if the date is valid, and \code{FALSE} if it is not and
#'   should be resampled.
NULL

#' @describeIn predicate An example predicate function that can be passed to
#'   \code{\link{sample_date}} so that it will return only those dates that fall
#'   on Mondays.
#' @export
is_monday <- function(y, m, d) {
  lubridate::wday(lubridate::ymd(paste(y, m, d, sep = "-"), quiet = TRUE)) == 2
}

#' @describeIn predicate Always returns TRUE. This is the default predicate used
#'   by \code{\link{sample_date}}. Used when initially checking values so that
#'   the supplied predicate never rejects a starting condition out-of-hand.
#' @export
null_predicate <- function(y, m, d) return(TRUE)
