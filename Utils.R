
library(tidyverse)
library(raster)
library(DBI)

Save_RasterID = function(conn, ModelRaster){
  # put the raster definition in the Zone table of the databes
  Rext = extent(ModelRaster)
  Zone_ID = dbGetQuery(# Get the next Zone ID
    conn, "Select coalesce(max(Zone_ID) + 1,1) from Zone")[[1]]
  Rext = extent(ModelRaster)
  Rtable = tibble(
    Zone_ID = Zone_ID,
    Name = names(ModelRaster),
    xmin = Rext@xmin, xmax = Rext@xmax,
    ymin = Rext@ymin, ymax = Rext@ymax,
    nrow = nrow(ModelRaster),
    ncol = ncol(ModelRaster),
    CRS = crs(ModelRaster, asText=T)
  )
  dbWriteTable(conn, "Zone", Rtable, append=TRUE)
  return(Zone_ID)
}

Load_RasterID = function(conn, Zone_ID){
  # Create a raster from the raster designed by Zone_ID in the Zone Table
  # values of the cells are the cell index
  Rtable = dbGetQuery(
    conn,"Select * from Zone where Zone_ID=?", param=Zone_ID)
  Rext = extent(Rtable$xmin, Rtable$xmax, Rtable$ymin, Rtable$ymax)
  EmptyRaster = raster(Rext, Rtable$nrow,
                       Rtable$ncol, CRS(Rtable$CRS))
  RasterID = setValues(EmptyRaster, 1:(Rtable$nrow*Rtable$ncol))
  names(RasterID)=Rtable$Name
  return(RasterID)
}

extract_n = function(dat, n){
  # extract a number of length n from a character vector
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

extract_date = function(dat){
  # turn Year and DOY into a character date
  mutate(dat, DOY = round(as.numeric(DOY))) %>%
    mutate(Date = as.Date(paste(Year,"01-01",sep="-")) + (DOY - 1)) %>% 
    mutate(Date = as.character(Date))
}

ExtractRaster = function(files, IDfile, maskRaster){
  # files: list of files
  # IDfiles: list of names of the files and the columns
  # maskRaster: raster of the position ID and NA for filtered values
  
  Mraster = stack(files, quick=TRUE)
  
  names(Mraster) = IDfile
  names(maskRaster) = "Position_ID"
  #make the raster fit the mask
  Mrepro = projectRaster(Mraster, maskRaster)
  Mstack = stack(Mrepro, maskRaster)
  #plot(masked)
  # extract all the pixels except the NA values
  return(as.data.frame(Mstack, na.rm = TRUE))
}

create_Mask = function(conn, Zone_ID, Threshold, Year=NULL, Crop=NULL){
  # create a mask raster based on filtered values
  MaxWeight = tbl(conn, "MaxWeight") %>% filter(Zone_ID==!!Zone_ID)
  PixelID = Load_RasterID(conn, Zone_ID)
  
  # create the dataframe containing the cells index
  CellFrame = MaxWeight %>%
    dplyr::select(Position_ID, Coord) %>%
    distinct() %>% collect() %>% 
    # link the index of the pixel to it position ID in the database
    right_join(tibble(Coord=1:ncell(PixelID)), by = "Coord")
  
  selectedID = MaxWeight %>% # select the position with a good weight
    filter(weight>Threshold)
  #if a Year is selected
  if (!is.null(Year)){
    selectedID = selectedID %>% filter(Year==!!Year)
  }
  # if a crop is selected
  if (!is.null(Crop)){
    selectedID = selectedID %>% filter(Crop==!!Crop)
  }
  selectedID = selectedID %>%  pull(Position_ID)# take the ID
  maskValues = CellFrame %>%# set NA to not selected positions
    mutate(value = ifelse(Position_ID%in%selectedID, Position_ID, NA))
  maskRaster = setValues(PixelID, arrange(maskValues, Coord)$value)
}
