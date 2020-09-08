##
##  Project Name:  Monitoring program of the coral reefs from the costarrican pacific coast
##
##  Objective:     
##
##  Approach:      
##
##                 
##
##  Authors:       Andrea Arriaga
##
##  Institutions:  Universidad de Costa Rica, CIMAR
##
##  Date:          2020-08-15
##

##  Notes:         1. This file is intended to provide a guide to the basic
##                    workflow of the project, attempting to 'integrate' the
##                    different steps necessary to conduct the analyses &
##                    create visual outputs

##
##  1. Set up the core functionality
##
  # clean up
    rm(list=ls())

  # call to core packages for data manipulation
    library(dplyr)
    library(tidyr)
    library(magrittr)
    library(purrr)
    library(lubridate)
    library(stringr)
    library(stringi)
    library(forcats)
    library(pbapply) # barra de progreso

  # for importing different formats
    library(readr)
    library(readxl)
    library(openxlsx)
    library(xlsx)

  # call to visualisation & output generation
    library(ggplot2)
    library(GGally)
    library(Cairo)
    library(extrafont)
    library(RColorBrewer)
    library(viridis)

  # functionality for spatial analyses
    library(raster)
    library(rgdal)
    library(sf)
    library(rgeos)
    library(cluster)

  # point to working directory        ## -- will need to adjust for local copy -- ##
# Andre
    setwd("/media/andrea/Windows/Users/karina/Documents/pacific_coral_reefs_cr")

  # set font for graphical outputs
    theme_set(theme_bw(base_family = "Helvetica"))
    CairoFonts(  # slight mod to example in ?CairoFonts page
               regular    = "Helvetica:style = Regular",
               bold       = "Helvetica:style = Bold",
               italic     = "Helvetica:style = Oblique",
               bolditalic = "Helvetica:style = BoldOblique"
               )

  # call to map theme
    source("R/theme_nothing.R")

  # create helper function for reviewing data
    quickview <- function(x, n = 3L) {head(data.frame(x), n = n)}

  # set utm details
    utm_details <-
      paste0("+proj=utm +zone=15 +south +datum=WGS84 +units=m",
             " +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% CRS()

##
## 2. Generate core data objects
##
 ## -- create core objects from monitoring -- ##
  # point to creation locale
    creation_locale <- "creation_code/"
    
  source(paste0(creation_locale, "fish/create_fish_removetraitscolumns.R"))#final product: fishes_biomass_regional_nofgs
  
 # source(paste0(creation_locale, "fish/create_fish_costa_rica.R"))       #final product: fishes_biomass_regional_nofgs_costa_rica  
  
  source(paste0(creation_locale, "fish/create_fish_bajacalifornia.R"))     #final product: fishes_biomass_regional_nofgs_baja y data_intermediate/bajacalifornia_sites_correctnames

  source(paste0(creation_locale, "fish/create_fish_correctnames.R"))       #final product: fishes_biomass_regional_correctnames
  
#  source(paste0(creation_locale, "fish/create_fish_sites_correctnames.R")) #final product: fishes_biomass_regional_sites_correctnames
  source(paste0(creation_locale, "sites/create_sites_mexico.R"))           #final product: sites_mx
## new objects creation
  source(paste0(creation_locale, "sites/create_sites.R"))                  #final product: fishes_sites
    
  # source(paste0(creation_locale, "mpas/create_fish_mpas.R")) 

# functional    
  source(paste0(creation_locale, "functional/create_fish_fgs.R"))          #final product: fishes_biomass_regional_fgs y 
  # source(paste0(creation_locale, "fish/create_fish_abundance.R"))   
  # 
  # # create biophy
  # source(paste0(monitoring_locale, "biophy/create_fish_biophy.R"))    

    
    
  # point to analysis locale
  analysis_locale <- "analysis_code/"
    
  # # analysis fish functional
  # source(paste0(analysis_locale, "functional/analysis_fish_functional.R")) 

##
## 3. Visualise abundance data
##
 ## -- review monitoring data -- ##
  # point to abundance locale
  # analysis_locale <- "analysis_code/"

  # visualise monitoring data
  #  source(paste0(analysis_locale, "fish/analysis_gnrl.R"))
  # 
  # # visualise sites
  #   source(paste0(analysis_locale, "sites/plot_fish_sites.R"))
  # 
  # # visualise taxa
  # source(paste0(analysis_locale, "taxa/analysis_fish_taxa.R")) 
  #   
  # # visualise mpas
  #   source(paste0(analysis_locale, "mpas/analysis_fish_mpas.R")) 
  #   
  # # visualise functional
  # source(paste0(analysis_locale, "functional/analysis_fish_fun.R"))
  # 
  # # visualise biophy
  # source(paste0(analysis_locale, "functional/analysis_fish_biophy.R")) 
    
##
## 4. Population dynamics
##


##
# ## 6.  Additional validation & testing
# ##
#   # set path to exploratory analysis code
#     testing_locale <- "testing_code/"
# 
# ##
# ## 7. Generate dissemination materials & Presentations
# ##
#   # point to reporting locale
#     reporting_locale <- "reporting/"


##
## 8. Clean up workspace
##
  # remove paths
    # rm(creation_locale)

  