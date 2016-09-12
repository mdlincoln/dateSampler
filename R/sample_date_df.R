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
