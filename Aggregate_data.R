#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "L:/Lucas/phenology/_fParaPhase"
setwd(W.DIR)
library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(lubridate)
library(DBI)
##### VARIABLES ######
MODIS.DIR = "_input/MODIS/"
FIELD = "_input/LPIS/Brandenburg/FBS2014_EPSG25833.shp"
PHASE.DIR="L:/Lucas/phenology/PhenoWin/_DOY"
MODEL = "Brandenburg2.tif"
TH = 0.7
OUT.SQLITE = "Pixels_Time3"
YEARS = c(2015)

######## FUNCTIONS ########
extract_n = function(dat, n){
  # extract a number of length n from a character vector
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

extract_date = function(dat){
  mutate(dat, DOY = round(as.numeric(DOY))) %>%
    mutate(Date = as.Date(paste(Year,"01-01",sep="-")) + days(DOY - 1)) %>% 
    mutate(Date = as.character(Date))
}
###### Connect to Database

conn = dbConnect(RSQLite::SQLite(), paste(OUT.SQLITE, "sqlite", sep="."))
#create the view that link Phases and NDVI
dbGetQuery(conn, "
           CREATE VIEW IF NOT EXISTS pixel_date
            AS
            Select distinct n.pixel_id, n.date, NDVI, Crop, P
            from NDVI n inner join DOY p
            on n.pixel_id=p.pixel_id and n.date=p.date
           ")

###### IMPORT DATA ########
#### Rule for the mandatory column name for the raster 'infos'
# path to the raster: 'dir', name of the data : 'source'
# day of the year: 'DOY', year : 'YEAR'
#### it is possible to add any other culumn

modis = tibble(dir=list.files(MODIS.DIR, "_NDVI_.*\\.tif$", full.names = TRUE)) %>% 
  mutate(name = basename(dir)) %>% 
  mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>% 
  mutate(source="NDVI") %>% 
  gather("key", "value", -dir) # to fusion with the phase table
phase = tibble(dir = list.files(PHASE.DIR, "\\.tif$", full.names = T))%>% 
  mutate(name = basename(dir)) %>%
  mutate(Crop = extract_n(name, 3), Year = extract_n(name, 4),
          # Phenology have a lenght 1 or 2
          P = coalesce(extract_n(name, 2),extract_n(name, 1))) %>% 
  mutate(source="DOY")%>% 
  gather("key", "value", -dir)# to fusion with the modis table

infos = rbind(modis, phase) %>% spread("key", "value") %>% 
  filter(Year%in%YEARS) # filter on the selected year

# Import the mask and apply the threshold
modelMask = raster(MODEL) > TH # create a boolean raster


len = dim(infos)[1]
pb <- txtProgressBar(min=0, max=len, style=3) 
for(i in 1:len){
  #remove the useless columns
  info = infos[i,c(!is.na(infos[i,]))]
  
  Mraster = raster(info$dir)
  #make the raster fit the mask
  Mrepro = projectRaster(Mraster, modelMask)
  #plot(AllStack)
  masked = mask(Mrepro, modelMask, maskvalue=FALSE)
  #plot(masked)
  # extract all the pixels except the NA values
  RawData = as.data.frame(masked, na.rm = TRUE) %>% 
    rename(!!info$source := 1) %>% # the name of the value
    rownames_to_column("PIXEL_ID") %>% # keep the pixel id
    mutate(PIXEL_ID = as.numeric(PIXEL_ID)) %>% 
    cbind(dplyr::select(info, -source, -dir)) %>% # infos are new columns
    extract_date() #create the date column
  # save the data in the database
  dbWriteTable(conn, info$source, RawData, append=TRUE)
  
  setTxtProgressBar(pb, i)
}
dbDisconnect(conn)
