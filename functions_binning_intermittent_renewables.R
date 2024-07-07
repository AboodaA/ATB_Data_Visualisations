##Functions which take average insolation data and average wind speed data and classify it as a bin
#The data for categorizing the solar insolation per area can be found here: https://atb.nrel.gov/electricity/2024/utility-scale_pv
#The data for categorizing the wind speeds per area can be found here: https://atb.nrel.gov/electricity/2023/land-based_wind#resource_categorization



#Very simple function, takes GHI solar insolation and turns it into a bin

bin_the_GHI <- function(GHI_input)
{
  #Take in the NASA POWER values for average GHI, ALLSKY_SFC_SW_DWN, and
  # turn it into a bin 
  #The units used are the same 
  binned_category = 0
  
  if(GHI_input > 5.75)
  {
    binned_solar = 1
  }
  
  else if(GHI_input < 5.75 & GHI_input > 5.5)
  {
    binned_solar = 2 
  }
  
  else if(GHI_input < 5.5 & GHI_input > 5.25)
  {
    binned_solar = 3 
  }
  
  else if(GHI_input < 5.25 & GHI_input > 5)
  {
    binned_solar = 4 
  }
  
  else if(GHI_input < 5 & GHI_input > 4.75)
  {
    binned_solar = 5 
  }
  
  
  else if(GHI_input < 4.75 & GHI_input > 4.5)
  {
    binned_solar = 6 
  }
  
  else if(GHI_input < 4.75 & GHI_input > 4.25)
  {
    binned_solar = 7 
  }
  
  else if(GHI_input < 4.25 & GHI_input > 4)
  {
    binned_solar = 8 
  }
  
  else if(GHI_input < 4 & GHI_input > 3.75)
  {
    binned_solar = 9 
  }
  
  #Note that we need to fit in the "equal to" here, just in case. 
  else if(GHI_input <= 3.75)
  {
    binned_solar = 10 
  }
  
  return(binned_solar)
}



bin_the_wind <- function(avg_wind_speed)
{
  #Each of the wind speeds puts a region into a category/bin defined by average wind speed
  # and also the range of wind speeds (difference between max and min)
  bins_for_avg_wind_speeds = c(9.52, 8.87, 8.66, 8.45, 8.2, 7.84, 7.36, 6.8, 6.21, 5.13)
  spreads_for_wind_speeds = c(3.88, 0.24, 0.2, 0.22, 0.28, 0.45, 0.52, 0.57, 0.63, 4.18)
  
  #We will need to adjust the 
  hellman_exponent = 1/7
  #This is "hub height (assuming 120 m) / measured height of wind (should be 10 m) and up
  # to the Hellman exponent
  adjustment_factor = (120/10)^hellman_exponent
  adjusted_wind_speed = avg_wind_speed*adjustment_factor
  
  #The NREL ATB wind speed categories 
  #The function which.min returns the index of the element within the vector being sampled 
  # here "spreads for wind speeds" which has the lowest "distance" from the provided number
  
  binned_wind = which.min(abs(adjusted_wind_speed - bins_for_avg_wind_speeds))
  
  
  #The below functionality is paused for now--would have to change a few things in the download function
  # for the NASA POWER data
  #wind_speed_range_index = spreads_for_wind_speeds[binned_wind]
  #wind_range = abs(min_wind_speed - max_wind_speed)
  #wind_speed_range = abs(min_wind_speed - max_wind_speed)
  
  #For now, I will simply print a warning message if the range of wind speeds is too 
  # high comparatively. In future, this can be developed further
  #if(wind_speed_range >= 1.11*expected_wind_speed_range)
  #{
  #  print("The spread of wind speeds is larger than expected")
  #}
  return(binned_wind)
}


categorize_renewable_resources <- function(sf_with_climatology)
{
  sf_with_climatology$solar_bins = 0 
  sf_with_climatology$wind_bins = 0 
  number_of_districts = nrow(sf_with_climatology)
  print(paste("We have ", number_of_districts, " to go through"))
  for(i in 1:nrow(sf_with_climatology))
  {
    sf_with_climatology$solar_bins[i] = bin_the_GHI(sf_with_climatology$GHI_annual[i])
    sf_with_climatology$wind_bins[i] = bin_the_wind(sf_with_climatology$WS10M_annual[i])
    print(paste("We are at site ", i))
  }
  
  return(sf_with_climatology)
}
