#' Check values of in_out column
#'
#' Check that all values in in_out column are either 'inside' or 'outside'.
#' Otherwise, an error will report which cases need to be revised.
#'
#' @param df Data frame with a column named in_out.
#'
#' @return a data frame.
#' @export
#'
check_in_out <- function(df) {

  inout <- unique(df$in_out)
  values <- c("inside", "outside")

  if (!all(inout %in% values)) {
    stop("\n\nNot accepted values for in_out column: ",
         paste(inout[!inout %in% values], collapse = "\n"),
         "\nPlease use either 'inside' or 'outside'")
  }

  return(df)

}
