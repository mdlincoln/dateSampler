#' Repeat rows
#'
#' @export
row_rep <- function(df, n) {
  df[rep(1:nrow(df), each = n),]
}
