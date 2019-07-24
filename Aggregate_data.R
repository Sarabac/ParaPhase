#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
source("Variables.R")
library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(lubridate)
library(DBI)

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

ExtractRaster = function(files, IDfile, value_name,
                         PixelID, maskRaster){
  
  Mraster = stack(files, quick=TRUE)
  
  names(Mraster) = IDfile
  #make the raster fit the mask
  Mrepro = projectRaster(Mraster, maskRaster)
  Mstack = stack(PixelID, Mrepro)
  #plot(AllStack)
  masked = mask(Mstack, maskRaster, maskvalue=FALSE)
  #plot(masked)
  # extract all the pixels except the NA values
  return(as.data.frame(masked, na.rm = TRUE)%>%
           gather("IDfile",!!value_name, -Pixel_ID))
}
###### Connect to Database

conn = dbConnect(RSQLite::SQLite(), paste(OUT.SQLITE, "sqlite", sep="."))


###### IMPORT DATA ########
#### Rule for the mandatory column name for the raster 'infos'
# path to the raster: 'dir', name of the data : 'source'
# day of the year: 'DOY', year : 'YEAR'
#### it is possible to add any other culumn

PixelCrop = tbl(conn, "PixelCrop")
LPISyearCrop = PixelCrop %>%
  dplyr::select(Year, Crop,Winter) %>% distinct() %>% collect() %>% drop_na()

modis = tibble(dir=list.files(MODIS.DIR, "_NDVI_.*\\.tif$", full.names = TRUE)) %>% 
  mutate(name = basename(dir)) %>% 
  mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
  extract_date() %>% 
  mutate(source="NDVI")%>% 
  mutate(IDfile = paste("X", row_number(), sep=""))
phase = tibble(dir = list.files(PHASE.DIR, "\\.tif$", full.names = T))%>% 
  mutate(name = basename(dir)) %>%
  mutate(Crop = extract_n(name, 3), Year = extract_n(name, 4),
          # Phenology have a lenght 1 or 2
          P = coalesce(extract_n(name, 2),extract_n(name, 1))) %>% 
  mutate(source="Pheno")%>% 
  mutate(IDfile = paste("X", row_number(), sep=""))
#infos = bind_rows(modis, phase) %>% mutate(IDfile = paste("X", row_number(), sep=""))

PixelID = raster(paste(OUTPUT, ".tif", sep=""))
names(PixelID) = "Pixel_ID"
CellFrame = tibble(Pixel_ID=1:ncell(PixelID))

#### Extract NDVI
# extract also the previous Year for winter Crops
YearList = unique(c(LPISyearCrop$Year, LPISyearCrop$Year-1))
pb <- txtProgressBar(min=0, max=length(YearList), style=3) 
for(current_Year in YearList){
  infoNDVI = filter(modis, Year==current_Year&source=="NDVI")
  
  print(paste("NDVI", "Year:", current_Year))
  if(nrow(infoNDVI)){
    
    selectedID = PixelCrop %>%
      filter(Year==current_Year&weight>TH) %>% pull(Pixel_ID)
    maskValues = CellFrame %>%
      mutate(value = if_else(Pixel_ID%in%selectedID, TRUE, FALSE))
    maskRaster = setValues(PixelID, maskValues$value)
    rawData = ExtractRaster(infoNDVI$dir, infoNDVI$IDfile,
                            "NDVI", PixelID, maskRaster)
    ndvi = inner_join(rawData, infoNDVI, by="IDfile") %>%
      dplyr::select(Pixel_ID, Date, NDVI) %>% 
      mutate(NDVI=NDVI/10000) #retrive the good NDVI from MODIS
    dbWriteTable(conn, "NDVI", ndvi, append=TRUE)
  }
  setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
}

##### Extract phase informations ####

pb <- txtProgressBar(min=0, max=nrow(LPISyearCrop), style=3) 
for(i in 1:nrow(LPISyearCrop)){
  current_Year = LPISyearCrop$Year[i]
  current_Crop = LPISyearCrop$Crop[i]
  winter = LPISyearCrop$Winter[i]
  if(winter){#should consider the year before for winter crop
    selectedYear=c(current_Year, current_Year-1)
  }else{
    selectedYear=c(current_Year)
  }
  
  print(paste("Phase", "Year:", current_Year, "Crop:", current_Crop))
  
  for (CY in selectedYear){
    infoPhase = filter(phase, Year==CY&
                         Crop==current_Crop&
                         source=="Pheno")
  
    
    selectedID = PixelCrop %>%
      filter(Year==current_Year& Crop==current_Crop& weight>TH) %>%
      pull(Pixel_ID)
    maskValues = CellFrame %>%
      mutate(value = if_else(Pixel_ID%in%selectedID, TRUE, FALSE))
    maskRaster = setValues(PixelID, maskValues$value)
    
    if(!winter){
      infoP = infoPhase # keep everything
    }else if(CY==current_Year-1){
      infoP= infoPhase %>% # remove phases before september
        filter(quantile(stack(dir, quick=TRUE), 0.5) >240)#days
    }else{#CY==current_Year
      infoP= infoPhase %>% # remove phases after september
        filter(quantile(stack(dir, quick=TRUE), 0.5) <=240)#days
    }
    if(!nrow(infoP)){next}
    
    rawData = ExtractRaster(infoP$dir, infoP$IDfile,
                            "DOY", PixelID, maskRaster)
    Phase = inner_join(rawData, infoP, by="IDfile") %>%
      extract_date() %>% 
      dplyr::select(Pixel_ID, Date, Phase=P, Crop)
    dbWriteTable(conn, "Phase", Phase, append=TRUE)
  
  setTxtProgressBar(pb, i)
  }
}


dbExecute(conn, "drop view IF EXISTS Previous_Phase")
dbExecute(conn, "
          CREATE VIEW IF NOT EXISTS Previous_Phase
          AS
          Select n.Pixel_ID, n.Date as NDVI_Date, max(p.Date) as Pre_P_Date, Crop
            from NDVI n inner join Phase p
            on n.Pixel_ID=p.Pixel_ID
            where n.Date >= p.Date
            group by n.Pixel_ID, n.Date, Crop
           ")
dbExecute(conn, "drop view IF EXISTS next_Phase")
dbExecute(conn, "
          CREATE VIEW IF NOT EXISTS next_Phase
          AS
          Select n.Pixel_ID, n.Date as NDVI_Date, min(p.Date) as Next_P_Date, Crop
          from NDVI n inner join Phase p
          on n.Pixel_ID=p.Pixel_ID
          where n.Date < p.Date
          group by n.Pixel_ID, n.Date, Crop
          ")
dbExecute(conn, "drop view IF EXISTS NDVI_Phase_Range")
dbExecute(conn, "
          CREATE VIEW IF NOT EXISTS NDVI_Phase_Range
          AS
Select n.Pixel_ID, n.NDVI_Date, NDVI,
Pre_P_Date, b.Phase as Pre_P, Next_P_Date, a.Phase as Next_P, n.Crop
from Previous_Phase p inner join next_Phase n
on n.Pixel_ID=p.Pixel_ID and n.NDVI_Date=p.NDVI_Date and n.Crop=p.Crop
inner join ndvi
on ndvi.Pixel_ID=n.Pixel_ID and n.NDVI_Date=ndvi.Date
inner join Phase a
on n.Pixel_ID=a.Pixel_ID and Next_P_Date=a.Date and n.Crop=a.Crop
inner join Phase b
on p.Pixel_ID=b.Pixel_ID and Pre_P_Date=b.Date and p.Crop=b.Crop
")
dbExecute(conn, "drop view IF EXISTS Filtered_NDVI_Phase_Range")
dbExecute(conn, "
          CREATE VIEW IF NOT EXISTS Filtered_NDVI_Phase_Range
          AS
Select *, julianday(NDVI_Date)-julianday(Pre_P_Date) as NDays,
(julianday(NDVI_Date)-julianday(Pre_P_Date))/
(julianday(Next_P_Date)-julianday(Pre_P_Date))
as Relative_NDays,
julianday(Next_P_Date)-julianday(Pre_P_Date) as Phase_Period
from NDVI_Phase_Range
where julianday(Next_P_Date)-julianday(Pre_P_Date) < 150
           ")

dbDisconnect(conn)

