
conn
Zone_ID = 1
TIME_PERIODE = 2 # Days

library(tidyverse)
library(rdrop2) # to load data from a dropbox folder 
library(raster)
source("Utils.R")

PRECI.DIR = "Precipitation"
typePreci = function(name){
    typ = str_split(name, "_", simplify = TRUE)[,1]
    return(str_remove(typ, "RADOLANGT"))
  }

token = drop_auth()
filePreci = drop_dir("RadolanIndex", dtoken = token)%>%
  mutate(Year = extract_n(name, 4),
         DOY = extract_n(name, 3),
         Type = typePreci(name),
         path_local = paste(PRECI.DIR, name, sep="/"),
         downloaded = file.exists(path_local)) %>% 
  extract_date()

tbl(conn, "ErosionDate") %>%
  filter(Zone_ID==!!Zone_ID) %>% collect() %>% 
  crossing(filePreci) %>% 
  filter(as.Date(Date)<as.Date(Event_Date)) %>% 
  filter(as.Date(Event_Date)-as.Date(Date) < TIME_PERIODE)
  
dir.create(PRECI.DIR, showWarnings = FALSE)
selectedPreci = filePreci[1:5,]
for(i in 1:nrow(selectedPreci)){
  drop_download(
    selectedPreci$path_display[i],
    selectedPreci$path_local[i],
    overwrite = TRUE)
}


precipitation_ref = 31467
precipitation_crs = sf::st_crs(precipitation_ref)
link = "https://www.dropbox.com/s/qzmbcpljv4otpir/RADOLANGT10MM_2006_001.asc?dl=0"

ra2 = raster(link)
ra = raster("RADOLANSUM_2018_201.asc")
crs(ra) = CRS('+init=EPSG:31467')
plot(ra)


