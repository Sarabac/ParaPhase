#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd("L:/Lucas/phenology/ParaPhase")
source("Create_Mask.R")
source("Import_NDVI.R")
source("Import_Phases.R")
library(tidyverse)
library(DBI)
MODIS.DIR = "L:/Lucas/phenology/_fParaPhase/_input/MODIS"
PHASE.DIR="L:/Lucas/phenology/PhenoWin/_DOY"
LPIS.DIR="L:/Lucas/phenology/_fParaPhase/_input/LPIS/Uckermark"
MODIS.MODEL = "L:/Lucas/phenology/_fParaPhase/_input/MODIS/MOD09Q1_NDVI_2010_001.tif"
OUT.SQLITE = "ParaPhase.sqlite"
Threshold = 0.75 # threshold for the masks

conn = dbConnect(RSQLite::SQLite(), OUT.SQLITE)

# initialise the database ór updates its Views
sql_init = read_file("Init_Database.sql")
sql_list = str_split(sql_init, ";", simplify=TRUE)
for(i in 1:(length(sql_list)-1)){#last instruction is just a space
  dbExecute(conn, sql_list[i])
}
#create the table containing crop informations
CropCode = read.csv("CropCode.csv", sep=";") %>% drop_na(LPIS_code,PhenoID)
dbExecute(conn, "DELETE FROM Crop") # delete previous entries
dbAppendTable(conn, "Crop", CropCode)

# fill the tables : Zone, Position, Field, Weighting
Zone_ID = Create_Mask(conn, LPIS.DIR, MODIS.MODEL, ZONE_NAME="URC")
# fill the table NDVI
Import_NDVI(conn, Zone_ID, MODIS.DIR, Threshold)
# fill the table Phase
Import_Phases(conn, Zone_ID, PHASE.DIR, Threshold)
# read the view Filtered_NDVI_Phase_Range in the sqlite database to see the result
dbDisconnect(conn)
