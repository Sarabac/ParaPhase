create table if not exists Zone(
  Zone_ID INTEGER PRIMARY KEY,
  Name VARCHAR,
  xmin REAL NOT NULL,
  xmax REAL NOT NULL,
  ymin REAL NOT NULL,
  ymax REAL NOT NULL,
  nrow INTEGER NOT NULL,
  ncol INTEGER NOT NULL,
  CRS VARCHAR NOT NULL );

create table if not exists Crop(
  LPIS_code INTEGER,
  PhenoID INTEGER,
  German VARCHAR,
  English VARCHAR,
  Winter BOOLEAN);

create table if not exists Field(
  Zone_ID INTEGER FOREIGN KEY REFERENCES Zone(Zone_ID),
  Field_ID INTEGER PRIMARY KEY,
  Field_NR INTEGER,
  Year INTEGER NOT NULL, -- LPIS Year
  LPIS_code INTEGER);

create table if not exists Position(
  Zone_ID INTEGER FOREIGN KEY REFERENCES Zone(Zone_ID),
  Position_ID INTEGER PRIMARY KEY,
  Coord INTEGER NOT NULL); -- the cell number in the Zone raster

create table if not exists Pixel(
  Pixel_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER FOREIGN KEY REFERENCES Position(Position_ID),
  Field_ID INTEGER FOREIGN KEY REFERENCES Field(Field_ID),
  weight REAL NOT NULL);

create table if not exists NDVI(
  NDVI_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER FOREIGN KEY REFERENCES Position(Position_ID),
  NDVI_Value REAL NOT NULL,
  NDVI_Date Date);

create table if not exists Phase(
  Phase_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER FOREIGN KEY REFERENCES Position(Position_ID),
  Phase_Code INTEGER,
  Phase_Date Date);

Drop view IF EXISTS MaxWeight;
CREATE VIEW IF NOT EXISTS MaxWeight
AS
Select distinct
  g.Position_ID, Coord, g.Zone_ID, PhenoID AS Crop, Year
  max(weight) AS  weight
  from Pixel p
  inner join Position g
  on g.Position_ID=p.Position_ID
  inner join Field f
  on f.Field_ID=p.Field_ID
  inner join Crop c
  on c.LPIS_code = f.LPIS_code
  GROUP BY g.Position_ID, Coord, g.Zone_ID, Crop, Year;

Drop view IF EXISTS Previous_Phase;
CREATE VIEW IF NOT EXISTS Previous_Phase
AS
SELECT NDVI_ID, Phase_ID AS Pre_Phase_ID
FROM (-- select the closest previous phase date in each position and each crop
  Select NDVI_ID, n.Position_ID as Pre_Position
  max(Phase_Date) as Pre_P_Date, Crop as Pre_Crop
  from NDVI n inner join Phase p
  on n.Position_ID=p.Position_ID
  where NDVI_Date >= Phase_Date
  group by n.Position_ID, NDVI_ID, Crop
)
INNER JOIN Phase
ON Position_ID=Pre_Position AND Phase_Date=Pre_P_Date AND Crop=Pre_Crop
GROUP BY NDVI_ID ORDER BY Phase_ID  LIMIT 1;
-- if 2 transitions the same day... unlikely

Drop view IF EXISTS Next_Phase;
CREATE VIEW IF NOT EXISTS Next_Phase
AS
SELECT NDVI_ID, Phase_ID AS Next_Phase_ID
FROM (-- select the closest next phase date in each position and each crop
  Select NDVI_ID, n.Position_ID as Pre_Position
  min(Phase_Date) as Pre_P_Date, Crop as Pre_Crop
  from NDVI n inner join Phase p
  on n.Position_ID=p.Position_ID
  where NDVI_Date < Phase_Date
  group by n.Position_ID, NDVI_ID, Crop
)
INNER JOIN Phase
ON Position_ID=Pre_Position AND Phase_Date=Pre_P_Date AND Crop=Pre_Crop
GROUP BY NDVI_ID ORDER BY Phase_ID LIMIT 1;
-- if 2 transitions the same day... unlikely

Drop view IF EXISTS NDVI_Phase_Range;
CREATE VIEW IF NOT EXISTS NDVI_Phase_Range
AS
Select z.Zone_ID, z.Name AS Zone_Name, posi.Coord,
NDVI_Value as NDVI, NDVI_Date,
a.Phase_Code AS Pre_P, a.Phase_Date AS Pre_P_Date,
b.Phase_Code AS Next_P, b.Phase_Date AS Next_P_Date
FROM NDVI
INNER JOIN Pre_Phase p
ON NDVI.NDVI_ID=p.NDVI_ID
INNER JOIN Next_Phase n
ON NDVI.NDVI_ID=n.NDVI_ID
INNER JOIN Phase a -- previous phase
ON a.Phase_ID=Pre_Phase_ID
INNER JOIN Phase b -- next phase
ON b.Phase_ID=Next_Phase_ID
INNER JOIN Position posi
ON NDVI.Position_ID=posi.Position_ID
INNER JOIN Zone z
ON posi.Zone_ID=z.Zone_ID;

Drop view IF EXISTS Filtered_NDVI_Phase_Range;
CREATE VIEW IF NOT EXISTS Filtered_NDVI_Phase_Range
AS
Select *, julianday(NDVI_Date)-julianday(Pre_P_Date) as NDays,
(julianday(NDVI_Date)-julianday(Pre_P_Date))/
(julianday(Next_P_Date)-julianday(Pre_P_Date))
as Relative_NDays,
julianday(Next_P_Date)-julianday(Pre_P_Date) as Phase_Period
from NDVI_Phase_Range
where julianday(Next_P_Date)-julianday(Pre_P_Date) < 150;
