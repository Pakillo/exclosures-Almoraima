

## Save spp2exclude, spp2include and sitenames as internal data
#' @importFrom devtools use_data
save_internaldata <- function(){

  spp2exclude <- readr::read_csv("data-raw/spp2exclude.csv")
  spp2exclude <- spp2exclude$sp

  spp2include <- readr::read_csv("data-raw/spp2include.csv")
  spp2include <- spp2include$sp

  sitenames <- readr::read_csv("data-raw/sitenames.csv")
  sitenames <- sitenames$site

  ## gpplot2 theme
  # source("https://gist.githubusercontent.com/Pakillo/c89ac159a2b4e4009de0717b7ff2c745/raw/fd57e5d639792790eb30ef5786fc401119853056/themes.R")


  usethis::use_data(spp2exclude, spp2include, sitenames, internal = TRUE, overwrite = TRUE)

}


all.equal.vectorised <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
# from http://stackoverflow.com/a/9508558


