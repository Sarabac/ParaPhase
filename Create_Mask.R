#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "L:/Lucas/phenology/_fParaPhase"
setwd(W.DIR)
library(tidyverse)
library(raster)
library(sp)
library(rgdal)
##### VARIABLES ######
MODIS.MODEL = "_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
FIELD = "_input/LPIS/Brandenburg/FBS2014_EPSG25833.shp"
OUTPUT = "Brandenburg2"



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

#pixels = readRDS("pixels_Brandenburg.rds")

weightValue = pixels %>% 
  group_by(layer) %>% #some pixels are on 2 fields
  summarise(weight = max(weight)) %>% # take only the max value
  right_join(CellData, by = "layer") %>% # the other pixel receive weight = 0
  mutate(value = coalesce(weight, W)) %>% 
  arrange(layer)# make sur this is the good cell order

WeightRaster = setValues(EmptyRaster, weightValue$value)
#plot(WeightRaster)
writeRaster(WeightRaster, paste(OUTPUT, ".tif", sep=""))



####### TRASH ####

#NullRaster[pixels$ID] = pixels$weight
#plot(WeightRaster)
#fieldArea = rgeos::gArea(field, byid = TRUE)
#enframe(fieldArea)

#area(ModelRaster)



#pix = brick(AllStack)
#pixels = extract(pix, field, df=TRUE, weight=TRUE)

#Vcell = velox::velox(cellraster)
#VSelectCells = Vcell$extract(field, df=TRUE, small = TRUE)
#VSelectCells

#cellnum = cellFromPolygon(AllStack[[1]], field, weights = TRUE)
#polyg = rasterToPolygons(AllStack)

#VS = velox::velox(AllStack)
#res = VS$extract(field, df=TRUE, small = TRUE, fun = function(x){median(x, 0.5,na.rm = TRUE)[[1]]} )

#pol = VS$rasterize(spdf = field, field = "GROESSE_P")
#VS$nbands
