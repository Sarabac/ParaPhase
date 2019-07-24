#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
source("Variables.R")
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(DBI)

####
CropCode = read.csv("CropCode.csv", sep=";")

###### IMPORT DATA ########
shapefiles = tibble(direc = list.files(LPIS.DIR, ".*\\.shp",
                        full.names = TRUE)) %>% 
  filter(str_detect(direc, "Uckermark")) %>% #small region for testing
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

conn = connect(OUT.SQLITE)
dbWriteTable(conn, "Field", st_drop_geometry(field),
             overwrite=TRUE)
dbWriteTable(conn, "CropCode", CropCode, overwrite=TRUE)
dbDisconnect(conn)

#### IMPORT RASTER ####
MRaster = raster(MODIS.MODEL)
field = st_transform(field, crs(MRaster))

ModelRaster = crop(MRaster, field)
#plot(Mcropped)
###### Create the raster containing the IDs 
EmptyRaster = raster(extent(ModelRaster), nrow(ModelRaster),
                    ncol(ModelRaster), crs(ModelRaster))
cellraster = setValues(EmptyRaster, 1:ncell(ModelRaster))
writeRaster(cellraster, paste(OUTPUT, ".tif", sep=""))# pixel ID raster

pixels = extract(cellraster, field, df=TRUE,weight=TRUE,
                 normalizeWeights=FALSE)
#saveRDS(pixels, paste(OUTPUT, ".rds", sep=""))

conn = connect(OUT.SQLITE)
dbWriteTable(conn, "Pixel", 
             rename(pixels, Field_ID=ID, Pixel_ID=layer),
             overwrite=TRUE)
# the view to create the masks
dbExecute(conn, "drop view IF EXISTS PixelCrop")
dbExecute(conn, "
          CREATE VIEW IF NOT EXISTS PixelCrop
          AS Select distinct
          Pixel_ID, PhenoID as Crop, Year, Winter,
          max(weight) AS  weight
          from Field f inner join Pixel p
          on f.Field_ID=p.Field_ID
          inner join CropCode c
          on c.LPIS_code = f.LPIS_code
          GROUP BY Pixel_ID, PhenoID, Year;
          ")
dbDisconnect(conn)
