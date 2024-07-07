#The URL for the Excel spreadsheet where all of the 2024 ATB data is stored:
#available from the web
atb2024_for_download = "https://data.openei.org/files/6006/2024%20v1%20Annual%20Technology%20Baseline%20Workbook%20Original%206-24-2024.xlsx"
#Download it and save it to the working directory. This is a Base R function
atb2024_downloaded = download.file(atb2024_for_download, "atb24_downloaded.xlsx")
#The data is not organised in the most friendly fashion, we will break it down here 


#A more programmatic approach 


