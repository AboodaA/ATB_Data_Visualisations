##Simple functions which prepare/save the plots
library(ggplot2)
library(openxlsx)

#Find the SF object and plot the map
generate_map_of_solar <- function(shape_with_climatology)
{
  #Let's extract the name of the state to print out the file name
  name_of_state = unique(shape_with_climatology$NAME_1)
  #Clean out blank spaces in state/province names
  name_of_state = gsub(" ", "_", name_of_state)
  path_for_plots = "images_plots/"
  full_file_name = paste0("solar_resource_map_", name_of_state, ".png")
  #Remember that we always need to keep the column names consistent
  plot_to_be_saved = ggplot2::ggplot(shape_with_climatology) + geom_sf(aes(fill = GHI_annual))
  ggplot2::ggsave(filename = full_file_name, path = path_for_plots, plot = plot_to_be_saved, width = 6, height = 5)
  print(paste("We printed the solar resource map for the state of  ", name_of_state))
  print(paste("See the files in ", path_for_plots, " for the files"))
}

generate_map_of_wind_resource <- function(shape_with_climatology)
{
  name_of_state = unique(shape_with_climatology$NAME_1)
  #Clean out blank spaces in state/province names
  name_of_state = gsub(" ", "_", name_of_state)
  path_for_plots = "images_plots/"
  full_file_name = paste0("wind_resource_map_", name_of_state, ".png")
  #Remember that we always need to keep the column names consistent
  plot_to_be_saved = ggplot2::ggplot(shape_with_climatology) + geom_sf(aes(fill = WS10M_annual))
  ggplot2::ggsave(filename = full_file_name, path = path_for_plots, plot = plot_to_be_saved, width = 6, height = 5)
  print(paste("We printed the wind resource map for the state of  ", name_of_state))
  print(paste("See the files in ", path_for_plots, " for the files"))
}

generate_map_of_solar_categories <- function(shape_with_bins)
{
  name_of_state = unique(shape_with_bins$NAME_1)
  #Clean out blank spaces in state/province names
  name_of_state = gsub(" ", "_", name_of_state)
  path_for_plots = "images_plots/"
  full_file_name = paste0("solar_resource_categories_", name_of_state, ".png")
  #Remember that we always need to keep the column names consistent
  plot_to_be_saved = ggplot2::ggplot(shape_with_bins) + geom_sf(aes(fill = solar_bins))
  ggplot2::ggsave(filename = full_file_name, path = path_for_plots, plot = plot_to_be_saved, width = 6, height = 5)
  print(paste("We printed the solar resource categories map for the state/province of  ", name_of_state))
  print(paste("See the files in ", path_for_plots, " for the files"))
}


generate_map_of_wind_categories <- function(shape_with_bins)
{
  name_of_state = unique(shape_with_bins$NAME_1)
  #Clean out blank spaces in state/province names
  name_of_state = gsub(" ", "_", name_of_state)
  path_for_plots = "images_plots/"
  full_file_name = paste0("wind_resource_categories_", name_of_state, ".png")
  #Remember that we always need to keep the column names consistent
  plot_to_be_saved = ggplot2::ggplot(shape_with_bins) + geom_sf(aes(fill = wind_bins))
  ggplot2::ggsave(filename = full_file_name, path = path_for_plots, plot = plot_to_be_saved, width = 6, height = 5)
  print(paste("We printed the wind categories map for the state/province of  ", name_of_state))
  print(paste("See the files in ", path_for_plots, " for the files"))
}

#This function will create county-level maps of a state and colour them based on the LCOE
#We assume that this function will only be called after the LCOE data has been 
generate_map_of_lcoe_given_year <- function(chosen_year, state_name, sf_with_categories)
{
  difference_in_years = chosen_year - 2022 
  #Which column of data will we extract? The fourth column contains the 
  column_index = 4 + difference_in_years
  #Now we extract the lcoe from the previously printed data
  print("Please note that the maps will focus on the 'Moderate' cost sensitivity, for now.")
  lcoe_xlsx_file = paste0(state_name, "LCOE_summaries.xlsx")
  lcoe_xlsx_file_wp = paste0("ATB_data_files/", lcoe_xlsx_file)
  print(lcoe_xlsx_file_wp)
  lcoe_solar_data = openxlsx::read.xlsx(lcoe_xlsx_file_wp, sheet = "LCOE_solar")
  lcoe_wind_data = openxlsx::read.xlsx(lcoe_xlsx_file_wp, sheet = "LCOE_LandBased_wind")
  lcoe_solar_data = lcoe_solar_data[which(lcoe_solar_data$CostCase == "Moderate"),]
  lcoe_wind_data = lcoe_wind_data[which(lcoe_wind_data$CostCase == "Moderate"),]
  #Make sure you only extract the relevant columns
  lcoe_solar_data = lcoe_solar_data[,c(1:3, column_index),]
  lcoe_wind_data = lcoe_wind_data[,c(1:3, column_index),]
  colnames(lcoe_solar_data) = c("NAME_2", "solar_bins", "CostCase", "LCOE_Year")
  colnames(lcoe_wind_data) = c("NAME_2", "wind_bins", "CostCase", "LCOE_Year")
  #Now combine the data frames 
  combined_lcoe_df_solar = merge(sf_with_categories, lcoe_solar_data, by = "NAME_2")
  combined_lcoe_df_wind = merge(sf_with_categories, lcoe_wind_data, by = "NAME_2")
  
  solar_lcoe_file = paste0("lcoe_plots/lcoe_solar_resources", "_", state_name, ".png")
  wind_lcoe_file = paste0("lcoe_plots/lcoe_wind_resources", "_", state_name, ".png")
  
  #Now print out the plots
  plot_to_be_saved = ggplot2::ggplot(combined_lcoe_df_solar) + geom_sf(aes(fill = LCOE_Year))
  ggplot2::ggsave(filename = solar_lcoe_file, path = "images_plots/", plot = plot_to_be_saved, width = 6, height = 5)
  plot_to_be_saved = ggplot2::ggplot(combined_lcoe_df_wind) + geom_sf(aes(fill = LCOE_Year))
  ggplot2::ggsave(filename = wind_lcoe_file, path = "images_plots/", plot = plot_to_be_saved, width = 6, height = 5)
}