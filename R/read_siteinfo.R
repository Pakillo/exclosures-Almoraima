#' Read site information
#'
#' Reads site information and check site names (see \code{\link{check_sitenames}}).
#'
#' @param df Path to data frame
#'
#' @return a data frame.
#' @export
#' @import readr
read_siteinfo <- function(df){

  sites <- read_csv(df)

  sites <- check_sitenames(sites)

  sites <- check_in_out(sites)

  sites <- check_coords(sites)

  write_csv(sites, "data/sites_info.csv")

  invisible(sites)

}
