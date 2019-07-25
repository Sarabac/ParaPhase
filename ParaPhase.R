#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd("L:/Lucas/phenology/ParaPhase")

MODIS.DIR = "L:/Lucas/phenology/_fParaPhase/_input/MODIS"
PHASE.DIR="L:/Lucas/phenology/PhenoWin/_DOY"
LPIS.DIR="L:/Lucas/phenology/_fParaPhase/_input/LPIS/Uckermark"
MODIS.MODEL = "L:/Lucas/phenology/_fParaPhase/_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
OUT.SQLITE = "ParaPhase.sqlite"
TH = 0.75 # threshold for the mask

conn = DBI::dbConnect(RSQLite::SQLite(), OUT.SQLITE)
#create the table containing crop informations
dbWriteTable(conn, "Crop",
             read.csv("CropCode.csv", sep=";"),
             overwrite=TRUE)
Zone_ID = Create_Mask()

DBI::dbDisconnect(conn)
