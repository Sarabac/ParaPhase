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
  
  PixelCrop = tbl(conn, "MaxWeight") %>% filter(Zone_ID==!!Zone_ID)
  LPISyearCrop = PixelCrop %>% # The years avalaibles in the LPIS shapefiles
    dplyr::select(Year) %>% distinct() %>% collect() %>% drop_na()
  
  modis = tibble(dir=MODIS.FILES) %>% 
    mutate(name = basename(dir)) %>% 
    mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
    extract_date() %>%
    #IDfile: to find each layer in the raster stack and the extracted dataframe
    mutate(IDfile = paste("X", row_number(), sep=""))
  
  # create the raster containing the cells index
  PixelID = Load_RasterID(conn, Zone_ID)
  names(PixelID) = "Pixel_ID"
  # create the dataframe containing the cells index
  CellFrame = tibble(Coord=1:ncell(PixelID)) %>% 
    left_join(dbGetQuery(conn, # get the position ID
          "Select Position_ID, Coord from Position where Zone_ID=?",
          param=Zone_ID), by = "Coord")
  
  #### Extract NDVI
  # use also the previous Year for winter Crops
  YearList = unique(c(LPISyearCrop$Year, LPISyearCrop$Year-1))
  pb <- txtProgressBar(min=0, max=length(YearList), style=3) 
  for(current_Year in YearList){
    infoNDVI = filter(modis, Year==current_Year)
    
    print(paste("NDVI", "Year:", current_Year))
    if(!nrow(infoNDVI)){next} # if no data for this year
    
    selectedID = PixelCrop %>% # select the position with a good weight
      filter(Year==current_Year&weight>Threshold) %>% pull(Position_ID)
    maskValues = CellFrame %>%# test for each position if it have been selected
      mutate(value = if_else(Position_ID%in%selectedID, TRUE, FALSE))
    maskRaster = setValues(PixelID, maskValues$value)
    # use the function ExtractRaster from the script Utils.R
    rawData = ExtractRaster(infoNDVI$dir, infoNDVI$IDfile,
                            "NDVI", PixelID, maskRaster)
    ndvi = rawData %>%
      # join the cell index to the Position_ID
      inner_join(CellFrame, by=c("Pixel_ID"="Coord")) %>% 
      mutate(NDVI=NDVI/10000) %>%  #retrive the good NDVI from MODIS
      inner_join(infoNDVI, by="IDfile") %>% # get the data of the layer
      dplyr::select(Position_ID, NDVI_Value = NDVI, NDVI_Date=Date)
      
    dbWriteTable(conn, "NDVI", ndvi, append=TRUE)
    
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
}
