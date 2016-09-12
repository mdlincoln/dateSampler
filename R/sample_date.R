#' Produce random dates within a particular range of possibilities
#'
#' @param year_min Integer. Minimum year (Required)
#' @param year_max Integer. Maximum year
#' @param m Integer. Month.
#' @param d Integer. Day.
#' @param n Integer. Number of permuations to return.
#'
#' @import lubridate
#'
#' @export
#'
#' @return A vector of POSIXct values
#'
#' @examples
#' sample_date(1930, NA, NA, 5)
#'
#' sample_date(1930, 2, NA, 5)
sample_date <- function(year_min = -9999, year_max = 9999, month_min = 1, month_max = 12, day_min = 1, day_max = 31, n) {

  check_args(year_min, year_max, month_min, month_max, day_min, day_max, n)

  # Check that year is present, and all other arguments are single integers
  stopifnot(!is.na(y))
  stopifnot(assertthat::is.count(y))
  stopifnot(is.count.NA(m))
  stopifnot(is.count.NA(d))
  stopifnot(is.count.NA(n))


  # Date generation code adapted from Dirk Eddelbuettel:
  # http://stackoverflow.com/a/14721124/3547541
  gen_date <- function(i, sd, ed) {
    dt <- difftime(ed, sd, units = "days")
    ev <- runif(i, 0, dt)
    sd + ev
  }

  if(is.na(m)) { # If there is no month...
    if(is.na(d)) { # ... and there is no day...
      # Set the early date to the year-01-01
      early <- lubridate::ymd(paste(y, "01", "01", sep = "-"))
      # And the late date to the last day of that same year
      late <- early + lubridate::years(1) - lubridate::days(1)
      gen_date(n, early, late)
    } else { # ... and there is a day ...
      early <- lubridate::ymd(paste(y, "01", d, sep = "-"))
      late <- early + lubridate::years(1) - months(1)
      gen_date(n, early, late)
    }
  } else {
    if(is.na(d)) {
      early <- lubridate::ymd(paste(y, m, "01", sep = "-"))
      late <- early + months(1) - lubridate::days(1)
      gen_date(n, early, late)
    } else {
      early <- lubridate::ymd(paste(y, m, d, sep = "-"))
      rep(early, n)
    }
  }
}

#' Return a data frame with these added
#'
#' Requires dplyr
#'
#' @param df The data frame to augment
#' @param y The column of \code{df} containing the year
#' @param m The column of \code{df} containing the month
#' @param d The column of \code{df} containing the day
#' @param n The number of replications
#' @param col_name Name of the new column with sampled dates to add to \code{df}
#'
#' @return A data frame
#'
#' @import dplyr
#'
#' @export
sample_date_df <- function(df, y, m, d, n, col_name = "sampled_date") {
  # Generate a dataframe with new dates
  new_dates <- df %>%
    rowwise() %>%
    do(data.frame(new_date = sample_date(y = .[[y]], m = .[[m]], d = .[[d]], n = n)))

  # Replicate the rows of df
  exp_df <- df %>%
    row_rep(n = n)

  # Join the new dates onto the original dataframe
  bind_cols(exp_df, new_dates)
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
