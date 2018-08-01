
## Save spp2exclude, spp2include and sitenames as internal data
exclosures:::save_internaldata()

library(exclosures)


#### READ AND PREPROCESS DATA #####

## Read site info
read_siteinfo("data-raw/sites_info_raw.csv")

## Read and prepare species info
read_sppinfo(sppdata = "data-raw/species_info_raw.csv")

## Read and process raw cover data
read_rawcover(rawcover = "data-raw/exclosures_cover_raw.csv",
              tr.length = 25)

## Read and prepare damage data
read_damage("data-raw/exclosure_damage_raw.csv")


## Prepare dataset
make_dataset()



#### EXPLORATORY ANALYSIS ####

rmarkdown::render("analyses/EDA.Rmd")


#### MANUSCRIPT ####

rmarkdown::render("manuscript/cercados_Almoraima/cercados_Almoraima.Rmd")



