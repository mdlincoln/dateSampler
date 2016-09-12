#' Return a data frame with these added
#'
#' Requires dplyr
#'
#' @param df The data frame to augment
#' @param n Integer. The number of replications
#' @param .name Name of the new column with sampled dates to add to \code{df}
#' @param ... Arguments to pass to \code{\link{sample_date}}
#' @param .id Name of the new column with iteration ids to add to \code{df}
#' @param quiet Boolean.
#'
#' @return The replicated data frame \code{df} with two new columns.
#'
#' @export
sample_date_df <- function(df, n, .name = "sampled_date", .id = "replicate", quiet = TRUE, ...) {
  assertthat::is.count(n)

  # Generate a column with new dates
  new_dates <- structure(unlist(Map(sample_date, ..., n = n, quiet = quiet)), class = "Date")

  # Replicate the rows of df
  exp_df <- row_rep(df, n = n, .id = .id)

  # Join the new dates onto the original dataframe
  exp_df[[.name]] <- new_dates
  return(exp_df)
}
