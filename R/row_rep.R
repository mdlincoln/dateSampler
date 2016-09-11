#' Repeat rows
#'
#' @export
row_rep <- function(df, n, .id = "replicate") {
  m <- nrow(df)
  ndf <- df[rep(seq_len(m), each = n),]
  ndf[[.id]] <- rep(seq_leng(n), each = m)
  return(ndf)
}
