##Take the two intermittent technologies looked at here--Utility scale solar PV, onshore wind--and calculate the LCOE
#How does this calculated LCOE compare with what is predicted in the 2024 ATB? 
#For now, we will just focus on the 2024 ATB file 
#We need the following packages
library(openxlsx)
library(stringi)

#We take one equation for the LCOE from this: https://atb.nrel.gov/electricity/2023/equations_&_variables
#Note that it is the yearly LCOE, not the same as the lifetime LCOE
#Yearly LCOE: VOM -PTC + (FCR X CAPEX + FOM)/(8760*CF)
#Note that the FCR, "Fixed Charge Rate," is "Capital Recovery Factor" X "Project Finance Factor"


#Ranges for the data: Where in the spreadsheet to find the data you're looking for. Note that we have to change this for the wind, sadly.
# vom_range = "K279:AO308"
# fom_range = "K247:AO276"
# capex_range = "K151:AO180"
# capfac_range = "K87:AO116"
# #For the FCR, we will extract both the CRF and the PFF (Capital Recovery and Project Finance)
# pff_range = "K389:AO391"
# #We will take the "Real" CRF which varies by price sensitivity/case but not by technology
# crf_range = "K79:AO81"
# #Where is the spreadsheet which 
# path_for_downloaded_data = "ATB_data_files/"
# complete_2024_path = paste0(path_for_downloaded_data, "ATB_2024_data.xlsx")
# 
# #For the time being, we will focus only the 2024 data since we didn't look into the metadata in
# # other years
# 
# 

#Ideally, we would be able to compare the 

#One task here: find a way to make it so it doesn't have to reopen the excel spreadsheet every time it gets called to produce a number for a specific county

#First just make sure that the data is there
check_and_extract_lcoe <- function()
{
  if(!file.exists("ATB_data_files/lcoe_summary.xlsx"))
  {
    print("We are going to extract the LCOE data first")
    lcoe_summary = openxlsx::read.xlsx("ATB_data_files/ATB_2024_data.xlsx", sheet = "Summary_LCOE")
    openxlsx::write.xlsx(lcoe_summary, file = "ATB_data_files/lcoe_summary.xlsx")
  }
  
  else
  {
    #print("We already have the LCOE data in the right folder")
    lcoe_summary = openxlsx::read.xlsx("ATB_data_files/lcoe_summary.xlsx")
  }
  
  return(lcoe_summary)
  
}

simple_lcoe_data_extraction <- function(technology_choice, category_class)
{
  #We can also save the LCOE summary data from the ATB in a separate spreadsheet 
  # See the other script
  #Please note that this very simply script can be thrown off if the exact name of the technology is not spelled correctly.
  
  #First check that the data is there

  efd = check_and_extract_lcoe()
  #The first 5 characters of the column "technology detail" can be done away with, they spell out "Class"
  efd$TechDetail = stringi::stri_sub(efd$TechDetail, 6)
  
  #Assuming that the input was for solar
  if(technology_choice == 1)
  {
    chosen_tech = efd[which(efd$Technology == "UtilityPV"),]
  }
  
  else if(technology_choice == 2)
  {
    chosen_tech = efd[which(efd$Technology == "LandbasedWind"),]
  }
  
  #We only want the data which correspond to our chosen technology AND category/class
  chosen_tech = chosen_tech[which(chosen_tech$TechDetail == category_class),]
  #At this stage, we need only the market case and the 29 columns showing the price
  chosen_tech =  chosen_tech[,8:36]
  return(chosen_tech)
}

# chosen_tech above reads in the type of generation technology and the category of the technology per region
#We need to run this over each and every region within a state/province 
region_by_region_lcoe <- function(sf_shape_with_categories)
{
  #We are going to assume that both/all generation technologies are wanted for the first part of this, which is used
  #to spit out the data for now
  
  #The input to this function is defined by the output of the function "generate_climatology_maps" in the file 
  # "functions_getting_preparing_shape_files". 
  name_of_state = unique(sf_shape_with_categories$NAME_1)
  relevant_data_solar = data.frame(sf_shape_with_categories$NAME_2, sf_shape_with_categories$solar_bins)
  relevant_data_wind = data.frame(sf_shape_with_categories$NAME_2, sf_shape_with_categories$wind_bins)
  colnames(relevant_data_solar) = c("NAME_2", "solar_bins")
  colnames(relevant_data_wind) = c("NAME_2", "wind_bins")
  print("We are working on")
  print(name_of_state)
  #We will have separate sheets for each type of generation technology--this might have to be a separate function
  # if we have an arbitrarily high number of generation technologies
  
  #We want to know the name of the district/county and the solar/wind resource categorisation for it
  
  #relevant_data_solar$CostCase = 0
  #relevant_data_wind = cbind(sf_shape_with_categories$NAME_2, sf_shape_with_categories$wind_bins)
  #print(relevant_data_solar)
  #Now, for each district/county we have three rows, one for each cost case
  #We also need 
  blank_slate_solar = data.frame(matrix(0, nrow = 3, ncol = 32))
  blank_slate_wind = data.frame(matrix(0, nrow = 3, ncol = 32))
  for(i in 1:nrow(relevant_data_solar))
  {
  county_level_data = relevant_data_solar[i,]
  county_level_data = rbind(county_level_data, county_level_data, county_level_data)
  county_level_data$CostCase = c("Advanced", "Moderate", "Conservative")
  extracted_data_solar = simple_lcoe_data_extraction(1, unique(county_level_data$solar_bins))
  combined_data = cbind(county_level_data, extracted_data_solar)
  #Before we wrap everything together, we need to make sure that all the column names are the same
  colnames(blank_slate_solar) = colnames(combined_data)
  blank_slate_solar = rbind(blank_slate_solar, combined_data)
  }
  
  print("We have done part 1")

  
  for(i in 1:nrow(relevant_data_wind))
  {
    county_level_dataW = relevant_data_wind[i,]
    county_level_dataW = rbind(county_level_dataW, county_level_dataW, county_level_dataW)
    county_level_dataW$CostCase = c("Advanced", "Moderate", "Conservative")
    extracted_data_wind = simple_lcoe_data_extraction(2, unique(county_level_dataW$wind_bins))
    combined_dataW = cbind(county_level_dataW, extracted_data_wind)
    #Before we wrap everything together, we need to make sure that all the column names are the same
    colnames(blank_slate_wind) = colnames(combined_dataW)
    blank_slate_wind = rbind(blank_slate_wind, combined_dataW)
  }
   
  
  #Remove the first three rows of fluff
  blank_slate_solar = blank_slate_solar[-(1:3),]
  blank_slate_wind = blank_slate_wind[-(1:3),]
  #Now to save everything
  specific_file = paste0(name_of_state, "LCOE_summaries.xlsx")
  complete_path = paste0("ATB_data_files/", specific_file)

  wb = openxlsx::createWorkbook()
  solar_sheet = openxlsx::addWorksheet(wb, sheetName = "LCOE_solar")
  openxlsx::writeData(wb, solar_sheet, blank_slate_solar)
  wind_sheet = openxlsx::addWorksheet(wb, sheetName = "LCOE_LandBased_wind")
  openxlsx::writeData(wb, sheet = wind_sheet, blank_slate_wind)
  openxlsx::saveWorkbook(wb, file = complete_path, overwrite = TRUE)
  print("See the ATB Data Files folder for the LCOE summaries")

} 