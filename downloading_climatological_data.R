##A script which takes GIS data, adds solar insolation and GHI data to it
# and then converts that into a colour map
# The colour maps are going to be based on a bin of GHI
# A second type of colour map takes in the LCOE input and colour maps it 

#A very simple download: assuming we know longitude and latitude
get_climatological_data <- function(longitude, latitude)
{
  annual_data = nasapower::get_power(community = "re", pars = c("WS10M", "ALLSKY_SFC_SW_DWN"), temporal_api = "climatology", 
                                     lonlat = c(longitude, latitude))
  
  annual_data = data.frame(annual_data)
  #We want only the 
  annual_data = annual_data$ANN
  
  return(annual_data)
  
}


#We will be running the above function over rows of data in a dataframe, not individual sites
get_climatology <- function(df_with_coordinates)
{
  #The assumed input here is a dataframe which has columns defining longitude and latitude for each polygon--the polygons are the
  df_with_coordinates$GHI_annual = 0 
  df_with_coordinates$WS10M_annual = 0 
  print("Now going to download climatological data")
  for(i in 1:nrow(df_with_coordinates))
  {
    climatology_downloaded = get_climatological_data(df_with_coordinates$longitude[i], df_with_coordinates$latitude[i])
    df_with_coordinates$GHI_annual[i] = climatology_downloaded[2]
    df_with_coordinates$WS10M_annual[i] = climatology_downloaded[1]
    numberofcounties = nrow(df_with_coordinates)
    print(paste("We are at site ", i, "out of a total of ", numberofcounties))
  }
  
  return(df_with_coordinates)
}
