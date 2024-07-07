library(readxl)
library(tidyverse)
##Functions used to retrieve ATB data published by the US' NREL 

#A meta function " %notin% which is always useful: the opposite of %in%
`%notin%` <- Negate(`%in%`)


list_of_urls = list()
#Counting down from 2024 
list_of_urls[1] = "https://data.openei.org/files/6006/2024%20v1%20Annual%20Technology%20Baseline%20Workbook%20Original%206-24-2024.xlsx"
#Here is 2023 
list_of_urls[2] = "https://data.openei.org/files/5865/2023-ATB-Data_Master_v9.0.xlsx"
#Here is 2022
list_of_urls[3] = "https://data.openei.org/submissions/5716"
#Here is 2021
list_of_urls[4] = "https://data.openei.org/files/4129/2021-ATB-Data_Master_new.xlsm"
#Here is 2020
list_of_urls[5] = "https://atb-archive.nrel.gov/electricity/2020/files/2020-ATB-Data.xlsm"
#Here is 2019
list_of_urls[6] = "https://data.nrel.gov/system/files/115/2019-ATB-data.xlsm"
#Here is 2018
list_of_urls[7] = "https://data.nrel.gov/system/files/89/2018-ATB-data-interim-geo.xlsm"
#Here is 2017
list_of_urls[8] = "https://data.nrel.gov/system/files/71/2017-ATB-data.xlsm"
#Here is 2016
list_of_urls[9] = "https://www.nrel.gov/docs/fy16osti/66944-DA.xlsm"
#Here is 2015 
list_of_urls[10] = "https://www.nrel.gov/docs/fy15osti/64077-DA.xlsm"

order_of_years = rev(2015:2024)

table_of_urls_years = data.frame(cbind(list_of_urls, order_of_years))

retrieve_ATB_data_for_a_year <- function(year_input)
{
  #This is the standard format of the file name we will save
  filename_to_put_down = paste0("ATB_", year_input, "_data", ".xlsx")
  path_to_atb_data = "ATB_data_files/"
  full_filename = paste0(path_to_atb_data, filename_to_put_down)
  
  #First we ought to check and see that the file does not already exist; maybe we already downloaded it
  if(file.exists(full_filename))
  
  {
    print("We already have the file!")
  }
  
  else 
  {  
  #Which row/year do we want the data from? 
  row_to_find = table_of_urls_years[which(table_of_urls_years$order_of_years == year_input),]
  url_to_download = row_to_find[,1][[1]]
  print(url_to_download)
  #How will we save the file name of the data?
  filename_to_put_down = paste0("ATB_", year_input, "_data", ".xlsx")
  download.file(url_to_download,destfile = filename_to_put_down)
  print(paste("For the year", year_input, sep = "  "))
  print(paste("Look for the file", filename_to_put_down, "in ", path_to_atb_data, sep = "  "))
  sheets_of_data = readxl::excel_sheets(path = filename_to_put_down)
  #print(sheets_of_data)
  }
  
}







#Just a simple function to test things out for now
determine_year_to_input <- function()
{
  yeartohold = readline(prompt = "Enter a year between 2015 and 2024 inclusive")
  return(yeartohold)
}



interface_with_user_atb_data <- function()
{
  print("Starting the comparisons with the ")
}



# askinguser <-function()
# {
#   print("Here we will ask you to specify a year")
#   workingdirectory = getwd()
#   print(paste("Your working directory is:", workingdirectory, sep = " "))
#   yearheld = determine_year_to_input()
#   retrieve_ATB_data_for_a_year(year_input = yearheld)
#   
# }

#This function will take input from the user to determine two years in as input, then it will check to see if the file for each of the two years 
# exists. It will download the relevant file if it does not. Next, it will look and see which technologies exist in both years
compare_two_years <- function()
{
  firstyear = readline(prompt = "Which year is the year to begin (longer ago)?   ")
  earlierfilename = paste0("ATB_", firstyear, "_data", ".xlsx")
  secondyear = readline(prompt = "Which year is the more recent, ending the comparison?  ")
  laterfilename = paste0("ATB_", secondyear, "_data", ".xlsx")
  if(file.exists(earlierfilename))
  {
    print(paste("The file  ", earlierfilename, "is in the correct folder", sep = " "))
  }
  
  else if(!file.exists(earlierfilename))
  {
    print("We will have to download the file")
    print(earlierfilename)
    retrieve_ATB_data_for_a_year(firstyear)
  }
  
  if(file.exists(laterfilename))
  {
    print(paste("The file  ", laterfilename, "is in the correct folder", sep = " "))
  }
  
  else if(!file.exists(laterfilename))
  {
    retrieve_ATB_data_for_a_year(secondyear)
  }
  
  #Now find out what the sheet names are for each of the two years
  sheets_first_file = openxlsx::getSheetNames(earlierfilename)
  sheets_second_file = openxlsx::getSheetNames(laterfilename)
  for(i in 1:length(sheets_second_file))
  {
    if(sheets_second_file[i] %notin% sheets_first_file)
    {
      print(sheets_second_file[i])
    }
  }
  
}