#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(DBI)
##### VARIABLES ######
MODIS.MODEL = "L:/Lucas/phenology/_fParaPhase/_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
FIELD = "L:/Lucas/phenology/_fParaPhase/_input/LPIS/Brandenburg/FBS2014_EPSG25833.shp"
OUTPUT = "output/Brandenburg2"
OUT.SQLITE = "output/Pixels_Time"

###### IMPORT DATA ########
MRaster = raster(MODIS.MODEL)
field = readOGR(FIELD) %>% 
  spTransform(crs(MRaster))


ModelRaster = crop(MRaster, field)
#plot(Mcropped)

CellData = tibble(layer = 1:ncell(ModelRaster), W = 0)
EmptyRaster = raster(extent(ModelRaster), nrow(ModelRaster),
                    ncol(ModelRaster), crs(ModelRaster))
cellraster = setValues(EmptyRaster, CellData$layer)

pixels = extract(cellraster, field, df=TRUE,weight=TRUE,
                 normalizeWeights=FALSE)
saveRDS(pixels, paste(OUTPUT, ".rds", sep=""))

#pixels = readRDS("output/pixels_Brandenburg.rds")
### Create the mask and Pixel_id raster
weightValue = pixels %>% 
  group_by(layer) %>% #some pixels are on 2 fields
  filter(weight==max(weight)) %>% # take only the max value
  filter(row_number()==1) %>% #some max are the same, assign to the first layer
  ungroup() %>% 
  right_join(CellData, by = "layer") %>% # the other pixel receive weight = 0
  mutate(value = coalesce(weight, W)) %>% 
  arrange(layer)# make sur this is the good cell order

WeightRaster = setValues(EmptyRaster, weightValue$value)
#plot(WeightRaster)
#plot(FieldIDRaster)
writeRaster(WeightRaster, paste(OUTPUT, "_mask.tif", sep=""))
writeRaster(cellraster, paste(OUTPUT, "_pixelid.tif", sep=""))

### Save field data in the database ###
FieldData = field@data %>% 
  mutate(Field_ID = row_number())
PixelData = weightValue %>% # do not use the 'pixel' table
  # because 'weightValue' have been filtrer
  # the doubles have been renoved
  dplyr::select(Field_ID=ID, Pixel_ID=layer, weight) %>% 
  drop_na()

conn = dbConnect(RSQLite::SQLite(), paste(OUT.SQLITE, ".sqlite", sep=""))
dbWriteTable(conn, "Field", FieldData, overwrite=TRUE)
dbWriteTable(conn, "Pixel", PixelData, overwrite=TRUE)

dbExecute(conn, "
           CREATE VIEW IF NOT EXISTS PixelField
           AS
           Select distinct f.*, p.Pixel_ID, p.weight
           from Field f inner join Pixel p
           on f.Field_ID=p.Field_ID
           ")
dbDisconnect(conn)
