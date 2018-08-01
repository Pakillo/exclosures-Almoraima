#' Check species names against spp2exclude and spp2include
#'
#' Check species names in a data frame. First, rows whose species name matches any in spp2exclude
#' are automatically removed from the data frame. Then remaining rows are checked
#' against spp2include: if any species name does not match spp2include, an error
#' will report which these cases are so that they can be revised.
#'
#' @param df Data frame with a column named species.
#' @param wide Logical. Data frame in wide format? Only TRUE for raw cover data (default is FALSE).
#' It wide = TRUE, species are assumed to start at the 5th column (until the end of the data frame).
#'
#' @return a data frame, possibly with some rows removed (those identified as species to exclude).
#' @export
#' @import dplyr
#'
check_spnames <- function(df, wide = FALSE) {

  if (wide) {

    df.clean <- suppressWarnings(dplyr::select(df, -one_of(spp2exclude)))
    spnames <- names(df.clean)[5:ncol(df.clean)]

  } else {

    df.clean <- dplyr::filter(df, ! species %in% spp2exclude)
    spnames <- unique(df.clean$species)

  }



  if (!all(spnames %in% spp2include)) {
    stop("\n\n", paste(spnames[!spnames %in% spp2include], collapse = "\n"),
         "\n\ndo not match species names in spp2include.csv.
         \nPlease check species names in data frame.")
  }

  return(df.clean)

}
