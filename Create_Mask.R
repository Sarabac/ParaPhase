#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
Create_Mask = function(conn, LPIS.DIR, MODIS.MODEL){
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
  field = shape %>% mutate(Field_ID = row_number())
  

  
  #### IMPORT RASTER ####
  MRaster = raster(MODIS.MODEL)
  field = st_transform(field, crs(MRaster))
  
  ModelRaster = crop(MRaster, field)
  #plot(Mcropped)
  ###### Create the raster containing the IDs 
  Zone_ID=Save_RasterID(conn, ModelRaster)
  dbWriteTable(conn, "Field", field %>%
                 st_drop_geometry() %>%
                 mutate(Zone_ID=Zone_ID),
               append=TRUE)
  RasterID=Load_RasterID(conn, Zone_ID)
  
  pixels = extract(RasterID, field, df=TRUE,weight=TRUE,
                   normalizeWeights=FALSE)%>%
    rename( Field_ID=ID, Pixel_ID=layer) %>%
    mutate(Zone_ID = Zone_ID)
  #saveRDS(pixels, paste(OUTPUT, ".rds", sep=""))
  
  dbWriteTable(conn, "Pixel", pixels, append=TRUE)
  # the view to create the masks
  dbExecute(conn, "drop view IF EXISTS PixelCrop")
  dbExecute(conn, "
            CREATE VIEW IF NOT EXISTS PixelCrop
            AS Select distinct
            Pixel_ID, PhenoID as Crop, Year, Winter,
            max(weight) AS  weight
            from Field f inner join Pixel p
            on f.Field_ID=p.Field_ID
            inner join Crop c
            on c.LPIS_code = f.LPIS_code
            GROUP BY Pixel_ID, PhenoID, Year;
            ")
}
