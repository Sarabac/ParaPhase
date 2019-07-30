create table if not exists Zone(
  -- define an empty raster that cover the studyed Zone
  Zone_ID INTEGER PRIMARY KEY,
  Name VARCHAR,
  -- the extent of the raster --
  xmin REAL NOT NULL,
  xmax REAL NOT NULL,
  ymin REAL NOT NULL,
  ymax REAL NOT NULL,
  -- -- --
  nrow INTEGER NOT NULL,
  ncol INTEGER NOT NULL,
  CRS VARCHAR NOT NULL); -- raster projection

create table if not exists Crop(
  -- link the LPIS code and the ID from the Phenological rasters
  -- the data is loaded from CropCode.csv
  LPIS_code INTEGER,
  PhenoID INTEGER,
  German VARCHAR,
  English VARCHAR,
  Winter BOOLEAN); -- is it a winter crop ?

create table if not exists Field(
  -- data contaned in the field shapefiles
  Zone_ID INTEGER,
  Field_ID INTEGER PRIMARY KEY,
  Field_NR INTEGER, -- ID given by the raster::extract function
  Year INTEGER NOT NULL, -- LPIS Year
  LPIS_code INTEGER,
  FOREIGN KEY (Zone_ID) REFERENCES Zone(Zone_ID) ON DELETE CASCADE
);

create table if not exists Position(
  -- link the cell number and the corresponding Zone
  Zone_ID INTEGER,
  Position_ID INTEGER PRIMARY KEY,
  Coord INTEGER NOT NULL,-- the cell number in the Zone raster
  UNIQUE(Zone_ID, Coord),
  FOREIGN KEY (Zone_ID) REFERENCES Zone(Zone_ID) ON DELETE CASCADE);

create table if not exists Weighting(
  -- link the cell position and the Field
  Position_ID INTEGER,
  Field_ID INTEGER,
  weight FLOAT,  -- indicate the proportion of the cell covered by the Field
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE,
  FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE);

create table if not exists NDVI(
  -- NDVI value in a given cell in a given Date
  NDVI_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  NDVI_Value FLOAT NOT NULL,
  NDVI_Date Date,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

create table if not exists Phase(
  -- Phase transition in a given cell in a given date for a given crop
  Phase_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Crop INTEGER,
  Phase_Code INTEGER,
  Phase_Date Date,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

create table if not exists ErosionEvent(
  -- Phase transition in a given cell in a given date for a given crop
  Event_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Event_Date Date,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

create table if not exists Precipitation(
  -- Phase transition in a given cell in a given date for a given crop
  Preci_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Preci_Date Date,
  Type VARCHAR,
  Value INTEGER,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

Drop view IF EXISTS MaxWeight;
CREATE VIEW IF NOT EXISTS MaxWeight
-- Weight for a given crop in a given Year in a given position
AS
Select distinct
  g.Zone_ID, g.Position_ID, Coord, PhenoID AS Crop, Year, Winter,
  max(weight) AS  weight
  from Weighting p
  inner join Position g
  on g.Position_ID=p.Position_ID
  inner join Field f
  on f.Field_ID=p.Field_ID
  inner join Crop c
  on c.LPIS_code = f.LPIS_code
  GROUP BY g.Position_ID, PhenoID, Year;

Drop view IF EXISTS Previous_Phase;
CREATE VIEW IF NOT EXISTS Previous_Phase
AS
Select NDVI_ID, Phase_ID, Phase_Code AS Pre_P,
max(Phase_Date) as Pre_P_Date, Crop
from NDVI n inner join Phase p
on n.Position_ID=p.Position_ID
where NDVI_Date >= Phase_Date
group by NDVI_ID, Crop;


Drop view IF EXISTS Next_Phase;
CREATE VIEW IF NOT EXISTS Next_Phase
AS
Select NDVI_ID, Phase_ID, Phase_Code AS Next_P,
min(Phase_Date) as Next_P_Date, Crop
from NDVI n inner join Phase p
on n.Position_ID=p.Position_ID
where NDVI_Date < Phase_Date
group by NDVI_ID, Crop;

Drop view IF EXISTS NDVI_Phase_Range;
CREATE VIEW IF NOT EXISTS NDVI_Phase_Range
AS
Select distinct
z.Zone_ID, z.Name AS Zone_Name, posi.Coord, p.Crop,
NDVI_Value as NDVI, NDVI_Date,
Pre_P, Pre_P_Date,
Next_P, Next_P_Date
FROM NDVI
INNER JOIN Previous_Phase p
ON NDVI.NDVI_ID=p.NDVI_ID
INNER JOIN Next_Phase n
ON NDVI.NDVI_ID=n.NDVI_ID and p.Crop=n.Crop
INNER JOIN Position posi
ON NDVI.Position_ID=posi.Position_ID
INNER JOIN Zone z
ON posi.Zone_ID=z.Zone_ID;
/*
/!\ if a year is missing, it will consider the closest date of the closest year
to avoid that, the View Filtered_NDVI_Phase_Range ensure that the
time periode between two phases is less than a year
*/
Drop view IF EXISTS Filtered_NDVI_Phase_Range;
CREATE VIEW IF NOT EXISTS Filtered_NDVI_Phase_Range
AS
Select NDVI_Phase_Range.*, English as CropName,
Pre_P||"-"||Next_P as Transition,
julianday(NDVI_Date)-julianday(Pre_P_Date) as NDays,
(julianday(NDVI_Date)-julianday(Pre_P_Date))/
(julianday(Next_P_Date)-julianday(Pre_P_Date))
as Relative_NDays,
julianday(Next_P_Date)-julianday(Pre_P_Date) as Phase_Period
from NDVI_Phase_Range
inner join Crop on Crop=PhenoID
where julianday(Next_P_Date)-julianday(Pre_P_Date) < 300;
-- the time periode between two phases is less than half a year

Drop view IF EXISTS ErosionDate;
CREATE VIEW IF NOT EXISTS ErosionDate
AS
Select distinct Zone_ID, Event_Date
from ErosionEvent e
INNER JOIN Position p
on e.Position_ID=p.Position_ID;
