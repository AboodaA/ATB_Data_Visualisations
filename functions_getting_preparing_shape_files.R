##The functions here are basically the workhorse of the collection of functions/package. We do two things with them. 
# First, to download the geographic shape file from a specific country, zoom in on state/province, and attach coordinates to each polygon at county/district level.  
# Second, to find the annual averages of solar insolation (GHI) and wind speed to give each county a category as defined by the ATB: a solar resource category and a wind resource category
# Finally, you can generate maps of LCOE at county level for each technology type
# We begin by sourcing the files which contain the functions we will rely on

#Downloading climatology data through the NASA POWER package in R
source("downloading_climatological_data.R")
#The functions which create maps
source("functions_creating_map_plots.R")
#The functions which define the resource categories for the intermittent renewables
source("functions_binning_intermittent_renewables.R")
#Functions to find the financial data associated with each technology class for a given region
source("atb_financial_data_extraction.R")
#Then the other methods 
library(ggmap)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(terra)
library(nasapower)
library(geodata)
library(stringi)


#First, download the files for Levels 1 and 2 SPDFs covering Canada 

get_state_shape_by_name <- function(country_decision)
{
  #We determine below where the saved files are stored
  #All downloaded shape files will be stored there
  path_for_shape_files = paste0(getwd(), "/ATB_geographic_files/")
  options(geodata_default_path = path_for_shape_files)
  
  #Since the downloading of the files can take some resources, let's check first if it exists in the folder
  
  if(country_decision == 1)
  {
    country_name = "CAN"
  }
  
  else if(country_decision == 2)
  {
    country_name = "USA"
  }
  
  else if(country_decision == 3)
  {
    country_name = "MEX"
  }

  #What would the file name look like? This is determined by the geodata package
  expected_filename = paste0("gadm41_", country_name, "_2_pk.rds")
  #I am going to divide this into two lines
  #The additional "/gadm/" is created by the geodata package, I need to find a way to fix thatr
  full_expected_file_path = paste0(path_for_shape_files,"gadm/", expected_filename)
  expected_file_path_no_file = paste0(path_for_shape_files,"gadm/")
  
  #Does the file exist from a previous download? 
  if(!file.exists(full_expected_file_path))
  {
    #If there is no file with the matching country, we will download that country
    geodata::gadm(country = country_name, level = 2, path = path_for_shape_files)
  }
  
  #Read the file in
  #We need to convert this into a SPDF using the following method from the SF library
  #This will allow us to spTransform it, later
  #For some reason, the following two lines must be separated (??)
  print(paste("Please have a look at the folder in   ", expected_file_path_no_file, "  to make sure the files are there"))
  complete_country = readRDS(full_expected_file_path)
  print("We have read in the file")
  Sys.sleep(2)
  complete_country2 = st_as_sf(complete_country)
  rm(complete_country)
  print("Quick pause")
  Sys.sleep(2)
  #Notice how the data is organised within the shape files
  #We also need to transform the shape file to make the coordinates usable 
  #Note that we need to do this to the complete country shape file before we can subset the state
  print("We are now going to project the shape file onto a proper coordinate system")
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  complete_country3 = st_transform(complete_country2, WGS84)
  rm(complete_country2)
  #Note that for larger regions covering longer distances, the spatial transformation becomes increasingly important
  # but projections to the right coordinate system are useful even for smaller areas
  #At this stage, we can extract the names of the provinces/states
  #Flatten the names of possible states/provinces to remove French characters etc.
  complete_country3$NAME_1 = complete_country3$NAME_1
  complete_list_of_states = unique(complete_country3$NAME_1)
  print("The possible states/provinces to choose from are are")
  print(complete_list_of_states)
  print("Pausing, again")
  Sys.sleep(2)
  statename = readline(prompt = "Please enter the name of the state/province you want to look at  ")
  #Let's just take only the part we are interested in 
  simpleshape = complete_country3[which(complete_country3$NAME_1 == statename),]
  #We do not need the rest of the country 
  print("Quick tidying")
  return(simpleshape)
}


#We will have to extract the centroids of each of the county-level polygons
#

find_centroids_for_counties <- function(state_or_province)
{
  #The input is a file similar to the one we just downloaded, but also
  # transformed into an SF object which makes it possible to add columns quickly
  sorp_df = state_or_province
  #Add longitude, latitude
  sorp_df$longitude = 0
  sorp_df$latitude = 0 
  for(i in 1:nrow(sorp_df))
  {
    #Find the centroid of each polygon/county-level division and attach it to the right row
    sorp_df$longitude[i] = st_centroid(sorp_df$geometry[i,], of_largest_polygon = TRUE, inside_polygon = TRUE)[[1]][1]
    sorp_df$latitude[i] = st_centroid(sorp_df$geometry[i,], of_largest_polygon = TRUE, inside_polygon = TRUE)[[1]][2]
    #We ought to print something here because some places have more counties than others
    numofcounties = nrow(sorp_df)
    print(paste("We are at site ", i, "of a total of ", numofcounties, " county/municipality sites"))
  }
  return(sorp_df)
}


#Now one function to tie them all together
#We will later make a second function which 
generate_climatology_maps <- function()
{
  countrydecision = readline(prompt = "Choose a country. Enter 1 for Canada, 2 for the United States and 3 for Mexico.  ")
  basic_sf_shape = get_state_shape_by_name(country_decision = countrydecision)
  #We will use the province/state name in a bit
  name_of_province_state = unique(basic_sf_shape$NAME_1)
  print("We now have the basic shape and will extract the coordinates from it")
  sf_shape_plus_coords = find_centroids_for_counties(basic_sf_shape)
  print("With the coordinates found, we will now find the GHI and wind speeds required")
  sf_shape_with_climatology = get_climatology(sf_shape_plus_coords)
  print("We have all the data we need to create the first two maps")
  generate_map_of_solar(sf_shape_with_climatology)
  generate_map_of_wind_resource(sf_shape_with_climatology)
  print("Note that if you want to do print out the LCOE maps and data, you need to categorise the intermittent renewables.")
  request_for_binning = readline(prompt = "Do you want to categorize the intermittent resources? (Y/N)   ")
  if(request_for_binning == "y" | request_for_binning == "Y")
  {
    #Prepare the categories   
    sf_shape_with_categories = categorize_renewable_resources(sf_shape_with_climatology)
    generate_map_of_solar_categories(sf_shape_with_categories)
    generate_map_of_wind_categories(sf_shape_with_categories)
    
    print("To run the LCOE analysis at a later time, you can keep the Simple Features object generated by this function, or you can do that right away.")
    lcoe_question = readline(prompt = "Do you want to extract the expected LCOE for the counties now? (Y/N)  ")
    if(lcoe_question == "y" | lcoe_question == "Y")
    {
      #The following function spits out the LCOE data for a specific shape file of a given province
      region_by_region_lcoe(sf_shape_with_categories)
      #This will now to go to the maps of LCOE 
      year_to_extract = as.numeric(readline(prompt = "Enter a year between 2022 and 2050 "))
      generate_map_of_lcoe_given_year(chosen_year = year_to_extract, state_name = name_of_province_state, sf_with_categories = sf_shape_with_categories )
    }
    
    
  }
  
  #We could turn this on or off--do we want the user to continue to have access to this data?
  return(sf_shape_with_categories)
}

