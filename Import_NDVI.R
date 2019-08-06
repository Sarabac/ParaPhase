Import_NDVI = function(conn, Zone_ID, MODIS.FILES, Threshold){
  # conn: DBI connection to a database
  # Zone_ID: ID of the the Zone in the database
  # MODIS.DIR: path to a directory containing MODIS NDVI Rasters
  # Threshold: [0,1] minimun overlapping proportion of a field
  #             on a cell
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(DBI)
  
  modis = tibble(dir=MODIS.FILES) %>% 
    mutate(name = basename(dir)) %>% 
    mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
    extract_date() %>%
    #IDfile: to find each layer in the raster stack and the extracted dataframe
    mutate(IDfile = paste("X", row_number(), sep=""))
  
  #### Extract NDVI
  # use also the previous Year for winter Crops
  LPISyearCrop = dbGetQuery(conn,
      "select distinct Year as Y1, Year-1 as Y0
      from MaxWeight where Zone_ID=?", param=Zone_ID)
  YearList = union(LPISyearCrop$Y1, LPISyearCrop$Y0)
  pb <- txtProgressBar(min=0, max=length(YearList), style=3) 
  for(current_Year in YearList){
    infoNDVI = filter(modis, Year==current_Year)
    
    print(paste("NDVI", "Year:", current_Year))
    if(!nrow(infoNDVI)){next} # if no data for this year
    
    maskRaster=create_Mask(conn, Zone_ID, Threshold, current_Year)
    if(is.null(maskRaster)){next}
    
    # use the function ExtractRaster from the script Utils.R
    rawData = ExtractRaster(infoNDVI$dir, infoNDVI$IDfile, maskRaster)
    ndvi = rawData %>%
      gather("IDfile","NDVI", -Position_ID)%>% 
      mutate(NDVI=NDVI/10000) %>%  #retrive the good NDVI from MODIS
      inner_join(infoNDVI, by="IDfile") %>% # get the data of the layer
      dplyr::select(Position_ID, NDVI_Value = NDVI, NDVI_Date=Date)
      
    dbWriteTable(conn, "NDVI", ndvi, append=TRUE)
    
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
}



