# External Functions ----

#' Produce random dates within a particular range of possibilities
#'
#' @param year_min Integer. Minimum year (Required)
#' @param year_max Integer. Maximum year
#' @param month_min Integer. Minimum month
#' @param month_max Integer. Maximum month
#' @param day_min Integer. Minimum day
#' @param day_max Integer. Maximum day
#' @param .p An optional predicate function taking a year, month, and day as its
#'   parameters, and returning TRUE if the date is valid. Allows one to
#'   construct complex date restrictions, such as only generating dates that are
#'   Mondays, etc.
#' @param n Integer. Number of permuations to return. (Required)
#' @param quiet Boolean. Display messages when regenerating illegal dates?
#'
#' @export
#'
#' @return A vector of POSIXct values
#'
#' @examples
#' set.seed(100)
#' # Generate random dates within the year 1930
#' sample_date(1930, n = 5)
#'
#' # Dates in March 1930
#' sample_date(1930, month_min = 3, month_max = 3, n = 5)
#'
#' # Dates between a given range
#' sample_date(year_min = 1930, year_max = 1931, day_min = 25, day_max = 31, n = 5)
#'
#' # Use a predicate function to return only Mondays
#' sample_date(1930, n = 5, .p = is_monday)
#'
sample_date <- function(year_min, year_max = year_min, month_min = 1, month_max = 12, day_min = 1, day_max = 31, n, .p = null_predicate, quiet = FALSE) {

  # Confirm initial arguments are valid
  check_args(year_min, year_max, month_min, month_max, day_min, day_max, n, .p)

  # Produce an initial round of candidates
  candidates <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Check for illegal dates in the candidates and re-sample as needed
  ill <- illegal_index(candidates$y, candidates$m, candidates$d, .p)
  while (any(ill)) {
    ill_n <- sum(ill)
    if (!quiet)
      message("Regenerating ", ill_n, " illegal date", ifelse(ill_n > 1, "s...", "..."))
    candidates[ill,] <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n = ill_n)
    ill <- illegal_index(candidates$y, candidates$m, candidates$d, .p)
  }

  # Parse all candidates into dates and return
  lubridate::ymd(paste(candidates$y, candidates$m, candidates$d, sep = "-"), quiet = TRUE)
}

# Internal Functions ----

# Which months have only 30 days
month30 <- c(9, 4, 6, 11)

# Returns true when the supplied combination is an illegal date
illegal_index <- function(y, m, d, .p) {

  # Reject any 31 days in 30 months
  (m %in% month30 & d >= 31) |
    # Reject any 30+ days in Feburary
    (m == 2 & d >= 30) |
    # Reject any 29+ days in leap year Februaries
    (lubridate::leap_year(y) & m == 2 & d >= 29) |
    # Reject any remaining unparsable dates
    is.na(lubridate::ymd(paste(y, m, d, sep = "-"), quiet = TRUE)) |
    !(.p(y, m, d))
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

check_args <- function(year_min, year_max, month_min, month_max, day_min, day_max, n, .p) {
  assertthat::is.count(n)
  stopifnot(is.function(.p))

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

  stopifnot(year_min <= year_max)
  stopifnot(month_min <= month_max)
  stopifnot(day_min <= day_max)
  # Reject out-of-bounds months
  stopifnot(month_min >= 1 & month_min <= 12 & month_max >= 1 & month_max <= 12)
  # Reject out-of-bounds days
  stopifnot(day_min >= 1 & day_min <= 31 & day_max >= 1 & day_max <= 31)
  if (all(is.na(lubridate::ymd(paste(year_min:year_max, month_min:month_max, day_min:day_max, sep = "-"), quiet = TRUE))))
    stop(paste0("The following ranges cannot return any valid dates:\n", "Year: ", year_min, "-", year_max, "\nMonth: ", month_min, "-", month_max, "\nDay: ", day_min, "-", day_max))
}

# Always returns TRUE. Used when initially checking values so that the supplied
# predicate never rejects a starting condition out-of-hand
null_predicate <- function(a, b, c) return(TRUE)
