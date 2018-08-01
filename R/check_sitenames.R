#' Check site names against sites list
#'
#' If any site name does not match sitenames, an error
#' will report which these cases are so that they can be revised.
#'
#' @param df Data frame with a column named site.
#'
#' @return a data frame.
#' @export
#'
check_sitenames <- function(df) {

  sites <- unique(df$site)

  if (!all(sites %in% sitenames)) {
    stop("\n\n", paste(sites[!sites %in% sitenames], collapse = "\n"),
         "\n\ndo not match site names in sitenames.csv.
         \nPlease check site names in data frame.")
  }

  return(df)

}
