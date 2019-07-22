MODIS.DIR = "L:/Lucas/phenology/_fParaPhase/_input/MODIS"
PHASE.DIR="L:/Lucas/phenology/PhenoWin/_DOY"
LPIS.DIR="L:/Lucas/phenology/_fParaPhase/_input/LPIS/Brandenburg"
MODEL = "Brandenburg2.tif" #name ofthe mask produced by "create mask"
TH = 0.75 # threshold for the mask
OUTPUT = "output/Brandenburg3"
OUT.SQLITE = "output/Pixels_Time2"
YEARS = c(2015)

MODIS.MODEL = "L:/Lucas/phenology/_fParaPhase/_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
FIELD = "L:/Lucas/phenology/_fParaPhase/_input/LPIS/Brandenburg/FBS2014_EPSG25833.shp"

connect = function(o){dbConnect(RSQLite::SQLite(), paste(o, "sqlite", sep="."))}
