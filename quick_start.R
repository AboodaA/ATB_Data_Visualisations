##Getting started

#The motivation behind these scripts was to streamline the generation of quick visualisations which tie the data to the yearly 
# ATB, published by the US-based NREL, with climatological data for given locations. We start by defining a "Level 1" administrative division
#state/province. We then download solar insolation data and wind speed data ("Typical Meteorological Year" data). The levels of solar insolation 
# and wind speed for a given location allow us to classify the locale according to NREL's 1-10 scale for different technologies. 
# For now, we are focusing only on land-based wind power and utility scale solar. 

#You will need to load the following packages:
library(ggmap)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(terra)
library(nasapower)
library(geodata)
library(stringi)


#Make sure you're in the correct directory 
setwd((dirname(rstudioapi::getSourceEditorContext()$path)))

#Source the scripts--this is where we define all the functions we will use
source("downloading_and_comparing_ATB_data_2024_to_2015.R")
source("atb_financial_data_extraction.R")
source("downloading_climatological_data.R")
source("functions_binning_intermittent_renewables.R")
source("functions_getting_preparing_shape_files.R")
source("functions_creating_map_plots.R")


#This one function ties all the pieces together
#Run the next line, the output will be put into the correct folders
generate_climatology_maps()