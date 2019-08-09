
Import_Erosion = function(conn, Zone_ID, EROSION.FILE){
  # Erosion_Files: shapefile containing a DATUM attribut
  # it is the dateof the erosion event
  library(tidyverse)
  library(sf)
  source("Utils.R")
  posi = dbGetQuery(conn, "select Position_ID from Position where Zone_ID =?", params = Zone_ID)
  Model = maskFromPosition(conn, posi$Position_ID)
  
  shape = st_read(EROSION.FILE)
  
  # Julian Date: the number of days since "1970-01-01"
  D_origin = as.Date("1970-01-01")
  ero = shape %>%
    transmute(Date = julian.Date(as.Date(DATUM, "%d.%m.%Y"))) %>% 
    st_transform(crs(Model))
  #raterise the erosion shapefile
  eroRaster = fasterize::fasterize(ero, Model, field = "Date")
  
  names(Model) = "Position_ID"
  names(eroRaster) = "JulianDate"
  # take only the erosion that occured in a LPIS field
  ErosionEvent = as.data.frame(stack(Model, eroRaster), na.rm=TRUE) %>% 
    # retrive the Date as a character vector
    mutate(Event_Date = as.character(as.Date(JulianDate, origin=D_origin))) %>% 
    dplyr::select(Position_ID, Event_Date)
  
  dbWriteTable(conn, "ErosionEvent", ErosionEvent, append=TRUE)
}