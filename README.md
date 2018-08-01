Impactos de la herbivoría por ungulados en las comunidades de plantas leñosas del Parque Natural Los Alcornocales
================
2018-08-01

Research compendium (code and data) used for the following publication:

Rodríguez-Sánchez F, Luque-Oliva V & Jurado V. (2018) Impactos de la herbivoría por ungulados en las comunidades de plantas leñosas del Parque Natural Los Alcornocales. *Almoraima*, en prensa.

Compendium DOI:

CITATION: Rodríguez-Sánchez F, Luque-Oliva V & Jurado V. (2018) Research compendium for "Impactos de la herbivoría por ungulados en las comunidades de plantas leñosas del Parque Natural Los Alcornocales" (Version 1.0.0). Zenodo.

Installation
------------

In order to run the analyses you will need to install the package first:

``` r
devtools::install_github("Pakillo/exclosures-Almoraima")
```

Usage
-----

There is a [`makefile.R`](https://github.com/Pakillo/exclosures-Almoraima/blob/master/makefile.R) that runs each step of the analysis in the appropriate order.

``` r

library(exclosures)


#### READ AND PRE-PROCESS DATA #####

## Read site info
read_siteinfo("data-raw/sites_info_raw.csv")

## Read and prepare species info
read_sppinfo(sppdata = "data-raw/species_info_raw.csv")

## Read and process raw cover data
read_rawcover(rawcover = "data-raw/exclosures_cover_raw.csv", tr.length = 25)

## Read and prepare damage data
read_damage("data-raw/exclosure_damage_raw.csv")

## Prepare dataset
make_dataset()


#### EXPLORATORY ANALYSIS ####

rmarkdown::render("analyses/EDA.Rmd")

#### MANUSCRIPT ####

rmarkdown::render("manuscript/cercados_Almoraima/cercados_Almoraima.Rmd")
```
