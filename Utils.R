
Save_RasterID = function(conn, ModelRaster){
  Rext = extent(ModelRaster)
  dbExecute(conn,
            "
            create table if not exists Zone(
            Zone_ID INTEGER PRIMARY KEY ,
            xmin REAL NOT NULL,
            xmax REAL NOT NULL,
            ymin REAL NOT NULL,
            ymax REAL NOT NULL,
            nrow INTEGER NOT NULL,
            ncol INTEGER NOT NULL,
            CRS VARCHAR NOT NULL )
            ")
  Zone_ID = dbGetQuery(# Get the next Zone ID
    conn, "Select coalesce(max(Zone_ID) + 1,1) from Zone")[[1]]
  Rext = extent(ModelRaster)
  Rtable = tibble(
    Zone_ID = Zone_ID,
    xmin = Rext@xmin, xmax = Rext@xmax,
    ymin = Rext@ymin, ymax = Rext@ymax,
    nrow = nrow(ModelRaster),
    ncol = ncol(ModelRaster),
    CRS = crs(ModelRaster, asText=T)
  )
  dbAppendTable(conn, "Zone", Rtable)
  return(Zone_ID)
}

Load_RasterID = function(conn, Zone_ID){
  Rtable = dbGetQuery(
    conn,"Select * from Zone where Zone_ID=?", param=Zone_ID)
  Rext = extent(Rtable$xmin, Rtable$xmax, Rtable$ymin, Rtable$ymax)
  EmptyRaster = raster(Rext, Rtable$nrow,
                       Rtable$ncol, CRS(Rtable$CRS))
  RasterID = setValues(EmptyRaster, 1:(Rtable$nrow*Rtable$ncol))
  return(RasterID)
}