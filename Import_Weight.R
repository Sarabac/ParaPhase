#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
Import_Weight = function(conn, LPIS.FILES, MODIS.MODEL, ZONE_NAME=""){
  # conn: DBI connection to a database
  # LPIS.DIR: path to a directory containing LPIS shapefiles
  #           their extent will be used to crop the Modis Model
  # MODIS.MODEL: path to a raster that will be used 
  #                as a model (projection and sampling)
  # Zone Name: character, name of the extent of the studyed zone
  #             can be used to find the zone in the database and
  #             to filter the data
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(sf)
  library(DBI)
  
  
  ###### IMPORT field shapefiles ########
  shapefiles = tibble(direc = LPIS.FILES) %>%
    pull(direc) %>%
    sapply(st_read)
  
  ###### Concatenate all polygons #######
  shape = NULL
  for (i in 1:length(shapefiles)){
    sha = shapefiles[[i]]
    # the name of the LPIS Crop ID column can change
    # this "if" loop fix it
    if("K_ART"%in%colnames(sha)){sha = rename(sha, NU_CODE=K_ART)}
    sh = sha %>% transmute(LPIS_code=as.factor(NU_CODE), Year=ANTRAGSJAH)
    if(is.null(shape)){# if first element
      shape = sh
    }else{# use the rbind method of the sf package
      shape = rbind(sh, shape)
    }
  }
  # Field_NR is corresponding to the polygon ID in the raster::extract
  # returned value
  field = shape %>% mutate(Field_NR = row_number())
  
  #### IMPORT RASTER ####
  MRaster = raster(MODIS.MODEL)
  field = st_transform(field, crs(MRaster))
  
  ModelRaster = crop(MRaster, field)
  names(ModelRaster) = ZONE_NAME
  # also used in the 'name' column in the database
  
  ###### insert the raster in the Zone table in the Database
  # Save_RasterID is a function from the Utils script
  Zone_ID=Save_RasterID(conn, ModelRaster)
  # Zone_ID is a number corresponding to the Zone_ID field in the database
  field4Database = field %>%
    st_drop_geometry() %>%# convert to dataframe
    mutate(Zone_ID=Zone_ID)
  dbWriteTable(conn, "Field", field4Database, append=TRUE)
  RasterID=Load_RasterID(conn, Zone_ID)
  # the cell value of RasterID is the cell index
  weighting = tibble(
    Zone_ID=numeric(), Field_NR=numeric(),
    Coord=numeric(), weight = numeric()
    )
  pb <- txtProgressBar(min=0, max=nrow(field), style=3)
  for(NR in field$Field_NR){
    weighting = extract(RasterID,
                        field[NR,],
                        df=TRUE,weight=TRUE,
                        normalizeWeights=FALSE)%>%
      rename(Field_NR = 1, Coord = 2, weight = 3) %>%
      mutate(Zone_ID = Zone_ID, Field_NR=NR) %>% 
      bind_rows(weighting)
    # format of weighting : Field_NR, Coord, weight, Zone_ID
    setTxtProgressBar(pb, NR)
  }
  
  Position = weighting %>% dplyr::select(Zone_ID, Coord) %>%
    distinct()# important because different field can overlap the same position
  dbWriteTable(conn, "Position", Position, append=TRUE)
  
  weighting4database = weighting %>%
    inner_join(dbGetQuery(conn, # retrive the Position ID automatically created
        "Select Position_ID, Coord from Position where Zone_ID=?",
        param=Zone_ID), by="Coord") %>% 
    inner_join(dbGetQuery(conn, # retrive the Field ID automatically created
        "Select Field_ID, Field_NR from Field where Zone_ID=?",
        param=Zone_ID), by="Field_NR") %>% 
    dplyr::select(Field_ID, Position_ID, weight) # remove joining columns
  
  dbWriteTable(conn, "Weighting", weighting4database, append=TRUE)
  return(Zone_ID) # to know in wich Zone we are working on
}
