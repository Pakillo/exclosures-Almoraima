#' Read raw cover data
#'
#' Read and process raw cover data to calculate total cover per species per transect.
#' The function performs some minimal checks, e.g. will give an error if column names
#' do not match species names in \code{spp2include.csv}.
#'
#' @param rawcover character. Path to a csv file containing the raw cover data. This data frame must fulfill certain conditions: (i) first four columns must be named \code{id, position, shrub_tree, section} and will be used to identify individual transects; (ii) Species raw cover data are stored in the 5th to nth column. See \code{\link[vegetools]{calculate_cover}} for more information about data formatting requirements.
#' @param tr.length Numeric. Transect length (if transect divided in sections, give section length).
#'
#' @return A data frame, and two csv files saved to the \code{data} folder:
#' \code{exclosure_cover_wide.csv} and \code{exclosure_cover_long.csv}.
#' @export
#' @import dplyr
#' @import readr
#' @importFrom vegetools calculate_cover
#' @importFrom tidyr gather spread
#'
#'
read_rawcover <- function(rawcover, tr.length){

  # first define some arguments
  split.cols <- c("site", "in_out", "shrub_tree", "section")
  first.spcol = length(split.cols) + 1

  ## Read raw data from CSV file
  raw <- readr::read_csv(rawcover, col_types =
                           cols(.default = "n",
                                site = "c", in_out = "c", shrub_tree = "c", section = "c",
                                monte = "c", council = "c", date = "c"))
  raw <- dplyr::select(raw, -monte, -council, -date)




  ##########################################################
  #### Correcting raw data: (see issue #11 on github) ####
  message("\nAssigning Erica sp. cover values to Erica species...\n")

  ## Erica sp. in site Laurel outside is actually Erica scoparia
  # so I'll add Erica sp. cover to Erica scoparia cover
  raw <- raw %>%
    mutate(`Erica scoparia` = ifelse(site == "Laurel" & in_out == "outside" & shrub_tree == "shrub",
                                     `Erica scoparia` + `Erica sp.`,
                                     `Erica scoparia`))

  ## and Erica sp. in site Faldas_Rubio2 inside must be Erica australis
  raw <- raw %>%
    mutate(`Erica australis` = ifelse(site == "Faldas_Rubio2" & in_out == "inside" & shrub_tree == "shrub",
                                      `Erica australis` + `Erica sp.`,
                                      `Erica australis`))

  ## now delete Erica sp. column
  raw <- dplyr::select(raw, -`Erica sp.`)
  ###############################################################



  # check
  if (!all(split.cols %in% names(raw)))
    cat("Expecting four first columns to be", split.cols)

  ## check species names
  raw.spp <- check_spnames(raw, wide = TRUE)

  ## check site names
  raw.cover <- check_sitenames(raw.spp)



  ## If all checks look right, calculate cover ##

  cover <- vegetools::calculate_cover(raw.cover,
                                     split.cols = split.cols,
                                     first.spcol = first.spcol,
                                     tr.length = tr.length,
                                     bare = TRUE,
                                     check.incremental = TRUE,
                                     precision = 0.05,
                                     sort.cols = TRUE,
                                     prop = FALSE,
                                     long.format = FALSE)


  ##############################################
  #### When section 1 or 2 are missing from a transect, they are actually all bare ground!! Need to add them up:

  all.sections <- expand.grid(apply(cover[, 1:length(split.cols)], 2, unique))     # get all possible transects and sections

  cover.all <- merge(cover, all.sections, all = TRUE)
  missed <- !complete.cases(cover.all[, -seq(1:length(split.cols))])   # identify those missing
  cover.all[missed, "bare"] <- tr.length    # missed transects are all bare ground
  cover.all[missed, (length(split.cols) + 1):(ncol(cover.all) - 1)] <- 0   # hence all species have 0 cover
  ######### end of patch


  #### Now sum cover for all sections within transects ####
  cover.total <- cover.all %>%
    group_by(site, in_out, shrub_tree) %>%
    summarise_each(funs(sum), -section)


  ## Save the data in wide shape
  readr::write_csv(cover.total, paste0("data/exclosure_cover_wide.csv"))




  #### Reshape data to long form ####

  cover.long <-  tidyr::gather(cover.total, "species", "cover", 4:ncol(cover.total))

  cover.long <- cover.long %>%
    dplyr::select(species, site, shrub_tree, in_out, cover) %>%   # Change the order of the columns

    ## assign species presence based on their cover
    # if cover > 0, present. If cover == 0, presence = 0. If cover == NA, presence == NA
    mutate(presence = ifelse(cover > 0, 1, 0))


  write_csv(cover.long, paste0("data/exclosure_cover_long.csv"))


  invisible(cover.long)

}



