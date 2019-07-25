Import_NDVI = function(conn, Zone_ID, MODIS.DIR, Threshold){
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(sp)
  library(rgdal)
  library(lubridate)
  library(DBI)
  
  
  ###### IMPORT DATA ########
  #### Rule for the mandatory column name for the raster 'infos'
  # path to the raster: 'dir', name of the data : 'source'
  # day of the year: 'DOY', year : 'YEAR'
  #### it is possible to add any other culumn
  
  PixelCrop = tbl(conn, "PixelCrop")
  LPISyearCrop = PixelCrop %>%
    dplyr::select(Year, Crop,Winter) %>% distinct() %>% collect() %>% drop_na()
  
  modis = tibble(dir=list.files(MODIS.DIR, "_NDVI_.*\\.tif$", full.names = TRUE)) %>% 
    mutate(name = basename(dir)) %>% 
    mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
    extract_date() %>% 
    mutate(source="NDVI")%>% 
    mutate(IDfile = paste("X", row_number(), sep=""))
  
  PixelID = Load_RasterID(conn, Zone_ID)
  names(PixelID) = "Pixel_ID"
  CellFrame = tibble(Pixel_ID=1:ncell(PixelID))
  
  #### Extract NDVI
  # extract also the previous Year for winter Crops
  YearList = unique(c(LPISyearCrop$Year, LPISyearCrop$Year-1))
  pb <- txtProgressBar(min=0, max=length(YearList), style=3) 
  for(current_Year in YearList){
    infoNDVI = filter(modis, Year==current_Year&source=="NDVI")
    
    print(paste("NDVI", "Year:", current_Year))
    if(nrow(infoNDVI)){
      
      selectedID = PixelCrop %>%
        filter(Year==current_Year&weight>Threshold) %>% pull(Pixel_ID)
      maskValues = CellFrame %>%
        mutate(value = if_else(Pixel_ID%in%selectedID, TRUE, FALSE))
      maskRaster = setValues(PixelID, maskValues$value)
      rawData = ExtractRaster(infoNDVI$dir, infoNDVI$IDfile,
                              "NDVI", PixelID, maskRaster)
      ndvi = inner_join(rawData, infoNDVI, by="IDfile") %>%
        dplyr::select(Pixel_ID, Date, NDVI) %>%
        mutate(Zone_ID = Zone_ID) %>% 
        mutate(NDVI=NDVI/10000) #retrive the good NDVI from MODIS
      dbWriteTable(conn, "NDVI", ndvi, append=TRUE)
    }
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
}
