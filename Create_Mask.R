#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
Create_Mask = function(conn, LPIS.DIR, MODIS.MODEL, ZONE_NAME=""){
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(sf)
  library(rgdal)
  library(DBI)
  
  
  ###### IMPORT DATA ########
  shapefiles = tibble(direc = list.files(LPIS.DIR, ".*\\.shp",
                          full.names = TRUE)) %>%
    pull(direc) %>%
    sapply(st_read)
  
  
  shape = NULL
  for (i in 1:length(shapefiles)){
    sha = shapefiles[[i]]
    if("K_ART"%in%colnames(sha)){sha = rename(sha, NU_CODE=K_ART)}
    sh = sha %>% transmute(LPIS_code=as.factor(NU_CODE), Year=ANTRAGSJAH)
    if(is.null(shape)){
      shape = sh
    }else{
      shape = rbind(sh, shape)
    }
  }
  field = shape %>% mutate(Field_NR = row_number())
  

  
  #### IMPORT RASTER ####
  MRaster = raster(MODIS.MODEL)
  field = st_transform(field, crs(MRaster))
  
  ModelRaster = crop(MRaster, field)
  names(ModelRaster) = ZONE_NAME
  #plot(Mcropped)
  ###### Create the raster containing the IDs 
  Zone_ID=Save_RasterID(conn, ModelRaster)
  field4Database = field %>%
    st_drop_geometry() %>%
    mutate(Zone_ID=Zone_ID)
  dbWriteTable(conn, "Field", field4Database, append=TRUE)
  RasterID=Load_RasterID(conn, Zone_ID)
  
  pixels = extract(RasterID, field, df=TRUE,weight=TRUE,
                   normalizeWeights=FALSE)
  
  renamedP = pixels %>% rename(Field_NR = 1, Coord = 2) %>% 
    mutate(Zone_ID = Zone_ID)
  Position = renamedP %>% dplyr::select(Zone_ID, Coord) %>% distinct()
  dbWriteTable(conn, "Position", Position, append=TRUE)
  pixel4database = renamedP %>%
    inner_join(dbGetQuery(conn,
        "Select Position_ID, Coord from Position where Zone_ID=?",
        param=Zone_ID), by="Coord") %>% 
    inner_join(dbGetQuery(conn,
        "Select Field_ID, Field_NR from Field where Zone_ID=?",
        param=Zone_ID), by="Field_NR") %>% 
    dplyr::select(Field_ID, Position_ID, weight)
  
  dbWriteTable(conn, "Pixel", pixel4database, append=TRUE)

}
