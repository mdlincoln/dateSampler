# External Functions ----

#' Produce random dates within a particular range of possibilities
#'
#' @param year_min Integer. Minimum year (Required)
#' @param year_max Integer. Maximum year
#' @param month_min Integer. Minimum month
#' @param month_max Integer. Maximum month
#' @param day_min Integer. Minimum day
#' @param day_max Integer. Maximum day
#' @param n Integer. Number of permuations to return. (Required)
#' @param quiet Boolean. Display messages when regenerating illegal dates?
#'
#' @export
#'
#' @return A vector of POSIXct values
#'
#' @examples
#' set.seed(100)
#' sample_date(1930, NA, NA, 5, quiet = TRUE)
#'
#' sample_date(1930, 2, NA, 5, quiet = TRUE)
sample_date <- function(year_min, year_max = year_min, month_min = 1, month_max = 12, day_min = 1, day_max = 31, n, quiet = FALSE) {

  # Confirm initial arguments are valid
  check_args(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Produce an initial round of candidates
  candidates <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Check for illegal dates in the candidates and re-sample as needed
  ill <- illegal_index(candidates$y, candidates$m, candidates$d)
  while (any(ill)) {
    ill_n <- sum(ill)
    if (ill_n == n)
      stop("All dates are illegal. Try setting different date limits.")
    if (!quiet)
      message("Regenerating ", ill_n, " illegal date", ifelse(ill_n > 1, "s...", "..."))
    candidates[ill,] <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n = ill_n)
    ill <- illegal_index(candidates$y, candidates$m, candidates$d)
  }

  # Parse all candidates into dates and return
  lubridate::fast_strptime(paste(candidates$y, candidates$m, candidates$d, sep = "-"), format = "%Y-%m-%d")
}

# Internal Functions ----

# Which months have only 30 days
month30 <- c(9, 4, 6, 11)

# Check for illegal dates with increasing specificity
illegal_index <- function(y, m, d) {
  # Reject any 31 days in 30 months
  (m %in% month30 & d >= 31) |
    # Reject any 30+ days in Feburary
    (m == 2 & d >= 30) |
    # Reject any 29+ days in leap year Februaries
    (lubridate::leap_year(y) & m == 2 & d >= 29) |
    # Reject any remaining unparsable dates
    is.na(lubridate::fast_strptime(paste(y, m, d, sep = "-"), format = "%Y-%m-%d"))
}

sample_ymd <- function(year_min, year_max, month_min, month_max, day_min, day_max, n) {

  # When year_min and year_max are the same, repeat the value instead of
  # sampling from a range
  srep <- function(c1, c2, n) {
    if (c1 == c2) {
      rep(c1, times = n)
    } else {
      sample(seq(c1, c2, by = 1), size = n, replace = TRUE)
    }
  }

  # Sample random years within range
  y <- srep(year_min, year_max, n)

  # Sample random months within range
  m <- srep(month_min, month_max, n)

  # Sample random days within range
  d <- srep(day_min, day_max, n)

  return(data.frame(y, m, d))
}

check_args <- function(year_min, year_max, month_min, month_max, day_min, day_max, n) {
  assertthat::is.count(n)

  dots <- list(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Confirm all arguments, if not NA, are single integers
  lapply(dots, function(x) {
    if (length(x) != 1) {
      stop(FALSE)
    } else if (is.na(x)) {
      return(FALSE)
    } else {
      assertthat::is.count(x)
    }
  })

  stopifnot(month_min <= month_max)
  stopifnot(day_min <= day_max)
  stopifnot(month_min >= 1 & month_min <= 12 & month_max >= 1 & month_max <= 12)
  stopifnot(day_min >= 1 & day_min <= 31 & day_max >= 1 & day_max <= 31)
}
