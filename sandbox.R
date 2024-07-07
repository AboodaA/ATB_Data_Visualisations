#Random data for Alberta 

random_alberta_data = data.frame(matrix(0, ncol = 2, nrow = 19))
random_alberta_data[,1] = runif(n = 19, min = 0.01, max = 1)
random_alberta_data[,2] = runif(n = 19, min = 0.01, max = 1)


##
can = readRDS("ATB_geographic_files/gadm/gadm41_CAN_2_pk.rds")
cansf = st_as_sf(can)
abt = cansf[which(cansf$NAME_1 == "Alberta"),]
#Let's look at our first map
ggplot2::ggplot(abt) + geom_sf()

abt_t2 = cbind(abt, random_alberta_data)

#What do the centroids look like?
sc = sf::st_centroid(abt)
# 1 centroid per each polygon!

#Create a climatology map and then map it to see how that works
gcm = generate_climatology_maps()


#What does the data look like when you extract the financial data? 
extracted_fin_data = extract_financial_data_atb2024("solar")

extracted_lcoe = check_and_extract_lcoe()



##See below to think about ways to enhance the retrieval of the financial data which define the LCOE 
# #The first function below extracts the data needed to calculate the LCOE from other data
# #The second function below will take the LCOE directly from the sheet "LCOE-Summary" from the ATB summary sheet
# 
# 
# #There must be a better way to arrange this function! 
# extract_financial_data_atb2024 <-function(generation_technology)
# {
#   if(generation_technology == "solar")
#   {
#     print("We will now read in the data from the downloaded source")
#     #We are going to add additional columns so that it is easier to read
#     vom_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = vom_range, col_names = FALSE)
#     vom_data$type = "VOM"
#     
#     fom_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = fom_range, col_names = FALSE)
#     fom_data$type = "FOM"
#     
#     capex_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = capex_range, col_names = FALSE)
#     capex_data$type = "CAPEX"
#     
#     capfac_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = capfac_range, col_names = FALSE)
#     capfac_data$type = "CAPACITY_FACTOR"
#     
#     pff_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = pff_range, col_names = FALSE)
#     pff_data$type = "PFF"
#     
#     crf_data = readxl::read_xlsx(path = complete_2024_path, sheet = "Solar - Utility PV", range = crf_range, col_names = FALSE)
#     crf_data$type = "CRF"
#     
#     combined_data = rbind(vom_data, fom_data, capex_data, capfac_data, pff_data, crf_data)
#     colnames(combined_data) = c("Technology", "Sensitivity_Case", {2022:2050}, "TYPE")
#   }
# 
# return(combined_data)
# }
# 
# 
