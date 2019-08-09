#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
# package fasterize
setwd("/home/luxis/Dropbox/Kuhn/phenology/ParaPhase")
source("Import_Weight.R")
source("Import_NDVI.R")
source("Import_Phases.R")
source("Import_Erosion.R")
source("Import_Precipitation.R")
library(tidyverse)
library(DBI)
MODIS.FILES = list.files("/home/luxis/Dropbox/MODIS",
                      "_NDVI_.*\\.tif$", full.names = TRUE)
PHASE.FILES = list.files("/home/luxis/Dropbox/Kuhn/phenology/PhenoWin1/_DOY",
                     "\\.tif$", full.names = TRUE)
LPIS.FILES = list.files("/home/luxis/Dropbox/Kuhn/phenology/LPIS/Koennern2",
                      ".*epsg25832\\.shp$", full.names = TRUE)
PRECI.FILES = list.files("/home/luxis/Dropbox/RadolanIndex",
                         "\\.asc$", full.names = TRUE)
EROSION.FILE = "/home/luxis/Dropbox/Kuhn/EROSION_SA/Erosionseregnisse_LSA.shp"
MODIS.MODEL = "/home/luxis/Dropbox/MODIS/MOD09Q1_NDVI_2010_001.tif"
ZONE_NAME = "Koennern"
OUT.SQLITE = "ParaPhase.sqlite"
Threshold = 0.5 # threshold for the masks
PARAMETRIZATION_PERIODE = 7 # days

conn = dbConnect(RSQLite::SQLite(), OUT.SQLITE)

# initialise the database ?r updates its Views
sql_init = read_file("Init_Database.sql")
sql_list = str_split(sql_init, ";", simplify=TRUE)
for(i in 1:(length(sql_list)-1)){#last instruction is just a space
  dbExecute(conn, sql_list[i])
}
#create or update the table containing crop informations
CropCode = read.csv("CropCode.csv", sep=";") %>% drop_na(LPIS_code,PhenoID)
dbExecute(conn, "DELETE FROM Crop") # delete previous entries
dbAppendTable(conn, "Crop", CropCode)

# fill the tables : Zone, Position, Field, Weighting
Zone_ID = Import_Weight(conn, LPIS.FILES, MODIS.MODEL, ZONE_NAME=ZONE_NAME)
# fill the table Phase
Import_Phases(conn, Zone_ID, PHASE.FILES)
# fill the table NDVI
Import_NDVI(conn, Zone_ID, MODIS.FILES)
# read the view Filtered_NDVI_Phase_Range in the sqlite database to see the result
dir.create("output", showWarnings = FALSE)

tes =tbl(conn, "Weighted_NDVI_Phase_Range") %>% 
  filter(Zone_ID==!!Zone_ID) %>%
  collect()

pp =tbl(conn, "Filtered_NDVI_Phase_Range") %>% 
  filter(Zone_ID==!!Zone_ID) %>%
  collect() %>% 
  write.csv2(paste(ZONE_NAME, ".csv", sep=""))
# fill the table ErosionEvent
Import_Erosion(conn, Zone_ID, EROSION.FILE)
# load the precipitation from the dropbox folder
# precipitation_paths = loadPreciFromDropbox(conn, Zone_ID, PARAMETRIZATION_PERIODE)
# fill the table Precipitation
Import_Precipitation(conn, Zone_ID, PRECI.FILES, PARAMETRIZATION_PERIODE)

dbDisconnect(conn)
