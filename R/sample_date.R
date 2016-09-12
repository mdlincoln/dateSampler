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

  check_args(year_min, year_max, month_min, month_max, day_min, day_max, n)


  # Produce candidates
  candidates <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Check for illegal dates and re-sample as needed
  ill <- illegal_index(candidates$y, candidates$m, candidates$d)
  while (any(ill)) {
    ill_n <- sum(ill)
    if(!quiet)
      message("Regenerating ", ill_n, " illegal date", ifelse(ill_n > 1, "s...", "..."))
    candidates[ill,] <- sample_ymd(year_min, year_max, month_min, month_max, day_min, day_max, n = ill_n)
    ill <- illegal_index(candidates$y, candidates$m, candidates$d)
  }

  return(lubridate::fast_strptime(paste(candidates$y, candidates$m, candidates$d, sep = "-"), format = "%Y-%m-%d"))
}

# Internal Functions ----

month30 <- c(9, 4, 6, 11)

illegal_index <- function(y, m, d) {
  (m %in% month30 & d == 31) |
    (m == 2 & d >= 30) |
    (lubridate::leap_year(y) & m == 2 & d > 28) |
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

# Date generation code adapted from Dirk Eddelbuettel:
# http://stackoverflow.com/a/14721124/3547541
gen_date <- function(i, sd, ed) {
  dt <- difftime(ed, sd, units = "days")
  ev <- runif(i, 0, dt)
  sd + ev
}

check_args <- function(...) {
  dots <- list(...)
  lapply(dots, function(x) {
    if (length(x) != 1) {
      stop(FALSE)
    } else if (is.na(x)) {
      return(FALSE)
    } else {
      assertthat::is.count(x)
    }
  })

  stopifnot(dots[["d"]] > 31)
  stopifnot(dots[["m"]] > 12)
}
