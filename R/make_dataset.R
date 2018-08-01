#' Prepare full dataset
#'
#' Prepare and assemble full dataset, after having processed all data components (using read_* functions).
#'
#' @param spp Path to data frame.
#' @param cover Path to data frame.
#' @param damage Path to data frame.
#' @param sites Path to data frame.
#'
#' @return a data frame, and a csv file: \code{data/fulldata.csv}.
#' @export
#'
make_dataset <- function(spp = "data/species_info.csv",
                         cover = "data/exclosure_cover_long.csv",
                         damage = "data/damage.csv",
                         sites = "data/sites_info.csv"){


  #### Join sppinfo with cover data ####

  sppinfo <- read_csv(spp)
  cover.long <- read_csv(cover)

  cover.spp <- merge(cover.long, sppinfo, by = "species", all.x = TRUE)

  ## bare acronym is 'Bare'
  cover.spp$acronym[cover.spp$species == "bare"] <- "Bare"

  ## checks
  if (anyNA(cover.spp$acronym))
    warning("Some species missing acronym or trait information: ",
            cover.spp$species[is.na(cover.spp$acronym)])




  #### ADDING DATA ON HERBIVORY DAMAGE #####

  damage <- read_csv(damage)

  cover.spp.dam <- merge(cover.spp, damage, by = c("species", "site", "in_out"), all = TRUE)  # or use all.x?

  if (nrow(cover.spp.dam) != nrow(cover.spp)) {
    comparison <- dplyr::anti_join(cover.spp.dam, cover.spp)
    print(comparison)
    stop("There are some mismatches in damage dataset: species, site, or in_out must be wrong.
         Please revise damage dataset.")
  }


  ## Trees do not have damage info
  cover.spp.dam$damage[cover.spp.dam$shrub_tree == "tree"] <- NA


  #### Damage obs missing for some taxa?
  miss.damage <- which(is.na(cover.spp.dam$damage) & cover.spp.dam$presence == 1 &
                         cover.spp.dam$shrub_tree == "shrub" & cover.spp.dam$species != "bare")
  if (length(miss.damage) > 0) {
    print(cover.spp.dam[miss.damage, ])
    warning("Some taxa are missing damage information.")

  }







  #### ADDING SITE DATA ####

  siteinfo <- read_csv(sites)
  siteinfo <- dplyr::select(siteinfo, -aspect)

  ## Join
  fulldata <- merge(cover.spp.dam, siteinfo, by = c("site", "in_out"), all = TRUE)

  ## Transform in_out column into numeric (binary) format (inside = 1, outside = 0)
  fulldata <- fulldata %>%
    dplyr::rename(inside = in_out) %>%
    mutate(inside = ifelse(inside == "inside", 1, 0))




  ## Reorder columns ##
  fulldata <- dplyr::select(fulldata, site, town, lat, long, altitude, inside, shrub_height,
                            species, acronym, shrub_tree, cover, presence,
                            damage, everything())


  ## save data
  write_csv(fulldata, "data/fulldata.csv")

  invisible(fulldata)


}
