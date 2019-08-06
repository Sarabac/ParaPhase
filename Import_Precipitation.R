library(tidyverse)
library(rdrop2) # to load data from a dropbox folder 
library(raster)
source("Utils.R")

PRECI.CRS = CRS('+init=EPSG:31467')

typePreci = function(name){
  # extract from the name of the file: "10MM", "SUM", "MAX"
    typ = str_split(name, "_", simplify = TRUE)[,1]
    return(str_remove(typ, "RADOLANGT"))
}

extractInfosPrecis = function(paths){
  tibble(path = paths) %>% 
    mutate(name = basename(path)) %>% 
    mutate(Year = extract_n(name, 4),
           DOY = extract_n(name, 3),
           Type = typePreci(name)) %>% 
    extract_date()
}

selectPreciPeriod = function(conn, Zone_ID, TIME_PERIODE, paths){
  tbl(conn, "ErosionDate") %>%
    filter(Zone_ID==!!Zone_ID) %>% collect() %>% 
    crossing(extractInfosPrecis(paths)) %>% 
    filter(as.Date(Date)<=as.Date(Event_Date)) %>% 
    filter(as.Date(Event_Date)-as.Date(Date) <= TIME_PERIODE)
}

loadPreciFromDropbox = function(
    conn, Zone_ID, TIME_PERIODE,
    # Time_Period, number of day to consider
    #before the erosion event
    dropbox_dir="RadolanIndex", 
    PRECI.DIR = "Precipitation"
  ){
  token = drop_auth()
  filePreci = drop_dir(dropbox_dir, dtoken = token)%>%
    pull(path_display)
  
  selectedFiles = selectPreciPeriod(conn, Zone_ID, TIME_PERIODE, filePreci) %>% 
    mutate(path_local = paste(PRECI.DIR, name, sep="/"),
           downloaded = file.exists(path_local))
    
  dir.create(PRECI.DIR, showWarnings = FALSE)
  Preci2Download = filter(selectedFiles, !downloaded)
  print("download begging")
  for(i in 1:nrow(Preci2Download)){
    print(paste(i, nrow(Preci2Download), sep="/"))
    drop_download(
      Preci2Download$path[i],
      Preci2Download$path_local[i],
      overwrite = TRUE)
  }
  print("download finished")
  return(selectedFiles$path_local)
}

Import_Precipitation = function(conn, Zone_ID, paths){
  Rpreci = stack(paths)
  crs(Rpreci) = PRECI.CRS
  position_erosion = tbl(conn, "ErosionEvent") %>% 
    inner_join(tbl(conn, "Position"), by="Position_ID") %>% 
    filter(Zone_ID==!!Zone_ID) %>% 
    pull(Position_ID)
  maskEro = maskFromPosition(conn, position_erosion)
  preci = ExtractRaster(Rpreci, NULL, maskEro) %>% 
    gather("path", "Value", -Position_ID)
  
  preci4database = preci %>% 
    bind_cols(extractInfosPrecis(preci$path)) %>% 
    transmute(Position_ID, Type, Value, Preci_Date = Date)
  
  dbWriteTable(conn, "Precipitation", preci4database, append=TRUE)
}

