#' Read species info
#'
#' Read and prepare species information. Basically, create acronym for each species
#' using \code{\link[vegan]{make.cepnames}}. But note that 'Genista tridens' is
#' given acronym "Genitrids", and "Genista tridentata" is given acronym "Genitrid".
#'
#' @param sppdata Path to csv file containing species data.
#' @param outdf Path to save cleaned csv.
#'
#' @return A dataframe, and a csv file saved to disk.
#' @export
#' @importFrom vegan make.cepnames
#' @import readr
#' @import dplyr

read_sppinfo <- function(sppdata, outdf = "data/species_info.csv"){

  # read species info
  sp <- readr::read_csv(sppdata)

  # omit species to exclude, and check names of remaining spp
  sp <- check_spnames(sp)

  # make acronyms
  sp$acronym <- vegan::make.cepnames(sp$species)
  sp$acronym[sp$species == "Genista tridens"] <- "Genitrids"
  sp$acronym[sp$species == "Genista tridentata"] <- "Genitrid"

  if (anyDuplicated(sp$acronym))
    warning("There are duplicated acronyms! ", sp$acronym[duplicated(sp$acronym)])

  # reorder
  sp <- dplyr::select(sp, species, acronym, everything())

  # save data frame
  write_csv(sp, outdf)

  invisible(sp)

}

