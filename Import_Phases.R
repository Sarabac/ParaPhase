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

  phase = tibble(dir = PHASE.FILES)%>% 
    mutate(name = basename(dir)) %>%
    mutate(Crop = extract_n(name, 3), Year = extract_n(name, 4),
           # Phenology have a lenght 1 or 2
           P = coalesce(extract_n(name, 2),extract_n(name, 1))) %>%
    #IDfile: to find each layer in the raster stack and the extracted dataframe
    mutate(IDfile = paste("X", row_number(), sep=""))
  

  ##### Extract phase informations ####
  LPISyearCrop = dbGetQuery(conn,
       "select distinct Year, Crop, Winter
       from MaxWeight where Zone_ID=?", param=Zone_ID)
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
    
    maskRaster = create_Mask(conn, Zone_ID, Threshold, current_Year, current_Crop)
    
    for (CY in selectedYear){
      infoPhase = filter(phase, Year==CY& Crop==current_Crop)
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
      rawData = ExtractRaster(infoP$dir, infoP$IDfile, maskRaster)
      if(!nrow(rawData)){next}# if no weight is higher than the threshold
      test <<- rawData %>% 
        gather("IDfile","DOY", -Position_ID)%>% 
        inner_join(infoP, by="IDfile")
      Phase = rawData %>% 
        gather("IDfile","DOY", -Position_ID)%>% 
        inner_join(infoP, by="IDfile") %>%
        extract_date() %>% 
        dplyr::select(Position_ID, Crop, Phase_Date=Date, Phase_Code=P)
      dbWriteTable(conn, "Phase", Phase, append=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
}