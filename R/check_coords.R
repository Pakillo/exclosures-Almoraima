#' Check site coordinates
#'
#' @param df Data frame with a column named in_out.
#'
#' @return a data frame.
#' @export
#' @importFrom stats complete.cases
#'
check_coords <- function(df) {

  coords <- df[, c("lat", "long")]

  if (!all(complete.cases(coords))) {
    warning("Some sites missing coordinates.")
    coords <- coords[complete.cases(coords), ]
  }

  stopifnot(coords$lat > 36 | coords$lat < 37)
  stopifnot(coords$long < -5 | coords$long > -6)

  return(df)

}
