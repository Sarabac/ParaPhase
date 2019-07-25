
Save_RasterID = function(conn, ModelRaster){
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