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
    sapply(st_read) %>% sapply(function(x){mutate(x, Year_ID = row_number())})
  
  
  shape = NULL
  for (i in 1:length(shapefiles)){
    sha = shapefiles[[i]]
    if("K_ART"%in%colnames(sha)){sha = rename(sha, NU_CODE=K_ART)}
    sh = sha %>% transmute(Year_ID, LPIS_code=as.factor(NU_CODE), Year=ANTRAGSJAH)
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
  dbAppendTable(conn, "Field", field %>%
                 st_drop_geometry() %>%
                 mutate(Zone_ID=Zone_ID))
  RasterID=Load_RasterID(conn, Zone_ID)
  
  pixels = extract(RasterID, field, df=TRUE,weight=TRUE,
                   normalizeWeights=FALSE)
  saveRDS(pixels, paste("test_p", ".rds", sep=""))
  
  dbWriteTable(conn, "Pixel", pixels, append=TRUE)

}
