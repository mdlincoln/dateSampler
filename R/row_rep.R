#' Repeat rows
#'
#' Replicate the rows of a data frame n times.
#'
#' @param df A data frame
#' @param n Number of replicates to create
#' @param .id Name of the oclumn in which to store the integer replicate ID
#'
#' @return A data frame
#'
#' @export
#'
#' @examples
#' rep_iris <- row_rep(iris, 5)
#' names(rep_iris)
#' nrow(rep_iris)
row_rep <- function(df, n, .id = "replicate") {
  m <- nrow(df)
  ndf <- df[rep(seq_len(m), each = n),]
  ndf[[.id]] <- rep(seq_len(n), each = m)
  return(ndf)
}
