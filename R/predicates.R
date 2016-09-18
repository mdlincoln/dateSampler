#' Predicate function
#'
#' An example predicate function that can be passed to \link{\code{sample_date}}
#' so that it will return only those dates that fall on Mondays.
#'
#' @param y Integer. Candidate year.
#' @param m Integer. Candidate month.
#' @param d Integer. Candidate day.
#'
#' @export
#'
#' @return \code{TRUE} if the date is valid, and \code{FALSE} if it is not and
#'   should be resampled.
is_monday <- function(y, m, d) {
  lubridate::wday(lubridate::ymd(paste(y, m, d, sep = "-"), quiet = TRUE)) == 2
}
