Import_Phases = function(conn, Zone_ID, PHASE.DIR, Threshold){
  # conn: DBI connection to a database
  # Zone_ID: ID of the the Zone in the database
  # PHASE.DIR: path to a directory containing Phase DOY Rasters
  # Threshold: [0,1] minimun overlapping proportion of a field
  #             on a cell
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(DBI)
  
  PixelCrop = tbl(conn, "MaxWeight")
  LPISyearCrop = PixelCrop %>%
    dplyr::select(Year, Crop,Winter) %>% distinct() %>% collect() %>% drop_na()
  
  phase = tibble(dir = list.files(PHASE.DIR, "\\.tif$", full.names = T))%>% 
    mutate(name = basename(dir)) %>%
    mutate(Crop = extract_n(name, 3), Year = extract_n(name, 4),
           # Phenology have a lenght 1 or 2
           P = coalesce(extract_n(name, 2),extract_n(name, 1))) %>%
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
  
  ##### Extract phase informations ####
  pb <- txtProgressBar(min=0, max=nrow(LPISyearCrop), style=3) 
  for(i in 1:nrow(LPISyearCrop)){
    current_Year = LPISyearCrop$Year[i]
    current_Crop = LPISyearCrop$Crop[i]
    print(paste("Phase", "Year:", current_Year, "Crop:", current_Crop))
    winter = LPISyearCrop$Winter[i]
    if(winter){#should consider the year before for winter crop
      selectedYear=c(current_Year, current_Year-1)
    }else{
      selectedYear=c(current_Year)
    }
    selectedID = PixelCrop %>%
      filter(current_Year& Crop==current_Crop& weight>Threshold) %>% pull(Position_ID)
    maskValues = CellFrame %>%
      mutate(value = if_else(Position_ID%in%selectedID, TRUE, FALSE))
    maskRaster = setValues(PixelID, maskValues$value)
    
    for (CY in selectedYear){
      infoPhase = filter(phase, Year==CY&
                           Crop==current_Crop)
      if(!winter){
        infoP = infoPhase # keep everything
      }else if(CY==current_Year-1){
        infoP= infoPhase %>% # remove phases before september
          filter(quantile(stack(dir, quick=TRUE), 0.5)[,1] >240)#days
      }else{#CY==current_Year
        infoP= infoPhase %>% # remove phases after september
          filter(quantile(stack(dir, quick=TRUE), 0.5)[,1] <=240)#days
      }
      if(!nrow(infoP)){next}#if no data
      
      rawData = ExtractRaster(infoP$dir, infoP$IDfile,
                              "DOY", PixelID, maskRaster)
      Phase = rawData %>% 
        inner_join(CellFrame, by=c("Pixel_ID"="Coord")) %>% 
        inner_join(infoP, by="IDfile") %>%
        extract_date() %>% 
        dplyr::select(Position_ID, Crop, Phase_Date=Date, Phase_Code=P)
      dbWriteTable(conn, "Phase", Phase, append=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
}