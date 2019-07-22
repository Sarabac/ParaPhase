W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
source("Variables.R")
library(tidyverse)
library(DBI)


conn = dbConnect(RSQLite::SQLite(), paste(OUT.SQLITE, "sqlite", sep="."))

NDVI = tbl(conn, "NDVI") %>% 
  select(Pixel_ID, Date, NDVI) %>% 
  collect()
Phase = tbl(conn, "DOY") %>% 
  select(Pixel_ID,Crop,Date,P) %>% 
  arrange(Pixel_ID,Crop,Date) %>% 
  filter(Crop=="201") %>% 
  collect() %>% 
  group_by(Pixel_ID,Crop) %>% 
  mutate(Porder=row_number()) %>% 
  ungroup()
dbDisconnect(conn)

g = full_join(NDVI, Phase, by=c("Pixel_ID", "Date"))%>%
  group_by(Pixel_ID) %>% 
  arrange(Pixel_ID, Date, P) %>% 
  mutate(next_P = P) %>% 
  fill(P, .direction ="down")%>%
  fill(next_P, .direction ="up") %>% 
  mutate(Period = paste(P, next_P, sep="_"))

clean = res2 %>%
  drop_na(P, next_P) %>% 
  filter(P!=next_P) %>% 
  mutate(Period = as.factor(Period))

with_day = clean %>% 
  group_by(Pixel_ID, P_order) %>% 
  mutate(nday=lubridate::)

DOY = Phase %>% 
  arrange(Pixel_ID,Crop,Date,P) %>% 
  group_by(Pixel_ID,Crop) %>% 
  mutate(next_P = c(P[-1], 0), next_Date = c(Date[-1],0)) %>%
  filter(row_number()!=n()) %>% 
  ungroup() %>% 
  mutate(period = as.factor(paste(P,next_P, sep="_")))

test = DOY %>% 
  group_by(Pixel_ID, period) %>% 
  expand(Date=if_else(
    
  ))
