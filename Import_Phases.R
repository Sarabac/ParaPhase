inject_phase = function(conn, phases){
  # get the weighting corresponding to a phase
  # and append the phase to the database
  # phase: Position_ID, Crop, Phase_Date, Phase_Code
  W_limits = dbGetQuery(conn, "
                          select W_ID, w.Position_ID,
                          Beginning, Ending, PhenoID as Crop
                          from Weighting w 
                          inner join Field f
                          on w.Field_ID=f.Field_ID
                          inner join Crop
                          on f.LPIS_code=Crop.LPIS_code") %>% 
    drop_na()
  
  phases4database = inner_join(W_limits, phases, 
                               by = c("Position_ID", "Crop")) %>% 
    filter(Phase_Date>=Beginning&Phase_Date<=Ending) %>% 
    dplyr::select("W_ID", "Crop", "Phase_Code", "Phase_Date")
  dbWriteTable(conn, "Phase", phases4database, append=TRUE)
}

Import_Phases = function(conn, Zone_ID, PHASE.DIR, declaration_day = "05-31",
                         P_beggining = c(10), P_ending = c(24)){
  # conn: DBI connection to a database
  # Zone_ID: ID of the the Zone in the database
  # PHASE.DIR: path to a directory containing Phase DOY Rasters
  # declaration_day: date of declaration of the crop on the field
  # P_beggining: code of the beginning Phase (vector)
  # P_ending: code of the ending Phase (vector)
  source("Utils.R")
  library(tidyverse)
  library(raster)
  library(DBI)

  # select all the position where there is a weighting
  posi_id = dbGetQuery(conn,"
                       select distinct p.Position_ID
                       from Position p
                       inner join Weighting w 
                       on p.Position_ID=w.Position_ID
                       where Zone_ID = ?",
                       param=Zone_ID)
  # select informations required to link a weighting and
  # it first and last phase
  weighting = dbGetQuery(conn, "
                         select W_ID, w.Position_ID,PhenoID as Crop, Year
                         from Weighting w inner join Position p
                         on w.Position_ID=p.Position_ID
                         inner join Field f
                         on w.Field_ID=f.Field_ID
                         inner join Crop
                         on f.LPIS_code=Crop.LPIS_code
                         where p.Zone_ID= ?
                         ", param = Zone_ID) %>% 
    mutate(declaration_date = as.Date(paste(Year, declaration_day, sep="-")))
  # create the mask of the position of each weighting
  posiMask = maskFromPosition(conn, posi_id$Position_ID)
  
  phase = tibble(dir = PHASE.FILES)%>% 
    mutate(name = basename(dir)) %>%
    mutate(Crop = extract_n(name, 3), Year = extract_n(name, 4),
           # Phenology have a lenght 1 or 2
           P = coalesce(extract_n(name, 2),extract_n(name, 1))) %>%
    #IDfile: to find each layer in the raster stack and the extracted dataframe
    mutate(IDfile = paste("X", row_number(), sep="")) %>% 
    # remove phases separated to the weighting by too much years
    filter(Year>=(min(weighting$Year) - 2)&Year<=(max(weighting$Year) + 2))
  
  # phases that are not in the limit
  other_phases = phase %>% 
    filter(!P%in%c(P_beggining, P_ending))
  
  # take only the first and last phase
  phase_limits = phase %>% 
    filter(P%in%c(P_beggining, P_ending)) %>% 
    mutate(beggining = P%in%P_beggining)
  if(nrow(phase_limits)){
    print("extract dates of the first and last phase")
    rawData = ExtractRaster(phase_limits$dir, phase_limits$IDfile, posiMask)
    
    pData = rawData %>% 
      gather("IDfile","DOY", -Position_ID)%>% 
      inner_join(phase_limits, by="IDfile") %>%
      extract_date() %>% 
      dplyr::select(Position_ID, Crop, Date, P, beggining)
    
    begin = filter(pData, beggining) %>% 
      dplyr::select(Position_ID, Crop, beginning=Date)
    end = filter(pData, !beggining) %>% 
      dplyr::select(Position_ID, Crop, ending=Date)
    
    print("Update weighting with beggining and ending date")
    begin_end = weighting %>% 
      left_join(begin, by=c("Position_ID", "Crop")) %>% 
      group_by(W_ID) %>% 
      filter(declaration_date>=beginning) %>% 
      filter(beginning==max(beginning)) %>% 
      left_join(end, by=c("Position_ID", "Crop")) %>% 
      filter(declaration_date<=ending) %>% 
      filter(ending==min(ending)) %>% 
      drop_na() %>% 
      # make sur that the time periode is less than 1 year
      filter(as.Date(ending) - as.Date(beginning) < 366) %>% 
      filter(row_number()==1) %>% 
      ungroup()
    
    dbExecute(conn, 
              'update Weighting set Beginning=?, Ending=? WHERE W_ID=?',
              param = list(
                begin_end$beginning, begin_end$ending, begin_end$W_ID
                ))
    print("weighting updated")
    phases = pData %>% rename(Phase_Date=Date, Phase_Code=P)
    inject_phase(conn, phases)
  }
  if(nrow(other_phases)){
    print("extract dates of all the other phases")
    rawData2 = ExtractRaster(other_phases$dir, other_phases$IDfile, posiMask)
    print("save the new phases")
    pData2 = rawData2 %>% 
      gather("IDfile","DOY", -Position_ID)%>% 
      inner_join(other_phases, by="IDfile") %>%
      extract_date() %>% 
      dplyr::select(Position_ID, Crop, Phase_Date = Date, Phase_Code = P)
    inject_phase(conn, pData2)
  }
  print("finished")
}
