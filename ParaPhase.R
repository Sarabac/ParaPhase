#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd("L:/Lucas/phenology/ParaPhase")
source("Import_Weight.R")
source("Import_NDVI.R")
source("Import_Phases.R")
library(tidyverse)
library(DBI)
MODIS.FILES = list.files("L:/Lucas/phenology/_fParaPhase/_input/MODIS",
                      "_NDVI_.*\\.tif$", full.names = TRUE)
PHASE.FILES = list.files("L:/Lucas/phenology/PhenoWin/_DOY",
                     "\\.tif$", full.names = TRUE)
LPIS.FILES = list.files("L:/Lucas/phenology/_fParaPhase/_input/LPIS/Uckermark",
                      ".*\\.shp", full.names = TRUE)
MODIS.MODEL = "L:/Lucas/phenology/_fParaPhase/_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
ZONE_NAME = "Uckermark"
OUT.SQLITE = "ParaPhase.sqlite"
Threshold = 0.75 # threshold for the masks

conn = dbConnect(RSQLite::SQLite(), OUT.SQLITE)

# initialise the database ór updates its Views
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
# fill the table NDVI
Import_NDVI(conn, Zone_ID, MODIS.FILES, Threshold)
# fill the table Phase
Import_Phases(conn, Zone_ID, PHASE.FILES, Threshold)
# read the view Filtered_NDVI_Phase_Range in the sqlite database to see the result
dbDisconnect(conn)
