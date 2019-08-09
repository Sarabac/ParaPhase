Import_NDVI = function(conn, Zone_ID, MODIS.FILES){
  # conn: DBI connection to a database
  # Zone_ID: ID of the the Zone in the database
  # MODIS.DIR: path to a directory containing MODIS NDVI Rasters
  # Threshold: [0,1] minimun overlapping proportion of a field
  #             on a cell
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(DBI)
  
  minMaxDate = dbGetQuery(conn, "
            SELECT MAX(Ending) AS `maxDate`, MIN(Beginning) AS `minDate`
            FROM Weighting w inner join Position p
            on w.Position_ID=p.Position_ID
            where Zone_ID = ?
            ", params = Zone_ID)
  
  modis = tibble(dir=MODIS.FILES) %>% 
    mutate(name = basename(dir)) %>% 
    mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
    extract_date() %>%
    filter(minMaxDate$maxDate >= Date & minMaxDate$minDate <= Date) %>% 
    #IDfile: to find each layer in the raster stack and the extracted dataframe
    mutate(IDfile = paste("X", row_number(), sep=""))
  positions = dbGetQuery(conn, "select w.position_ID from weighting w
                         inner join position p on w.Position_ID=p.Position_ID
                         where Zone_ID=?", params = Zone_ID)
  maskRaster=maskFromPosition(conn, positions$Position_ID)
  # separate each year
  
  YearList = unique(modis$Year)
  
  pb <- txtProgressBar(min=0, max=length(YearList), style=3) 
  for(current_Year in YearList){
    infoNDVI = filter(modis, Year==current_Year)
    
    print(paste("NDVI", "Year:", current_Year))
    if(!nrow(infoNDVI)){next} # if no data for this year
    
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



