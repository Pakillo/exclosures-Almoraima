#' Read browsing damage data
#'
#' Reads browsing damage data and check species names (see \code{\link{check_spnames}}).
#'
#' @param df Path to data frame
#'
#' @return a data frame.
#' @export
#' @import readr
read_damage <- function(df){

  damdata <- read_csv(df)

  damdata <- check_sitenames(damdata)

  damage <- check_spnames(damdata)

  if (any(damage$damage < 0 | damage$damage > 5))
    stop("\nDamage values out of 0-5 range! Please revise data frame.")

  write_csv(damage, "data/damage.csv")

  invisible(damage)

}
