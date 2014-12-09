EMISR
=====

EMISR is an R package.

#Installation


`install.packages("devtools")`

`library(devtools)`

`install_github("ldbk/EMISR")`

#Description

EMISR is a R package to extract and to analyse the data of the Environmental
Marine Information System (EMIS) and the Global Marine Information System
(GMIS). EMIS and GMIS are two activities of the European Commission - DG
JRC (Joint Research Centre), developed within the Water Resources Unit of
the Institute for Environment and Sustainability (IES).

The installation of this package can be long (the package size is about 230 Mo) 
because it includes the world MPA database and the world EEZ database in rdata format.
To install it, open a R console :

The shapefile of the marine protected area (MPA) was downloaded from
<http://www.protectedplanet.net> the 03/12/2014.
The shapefile was converted in SpatialPolygonsDataFrame using rgdal:

`mpa<-readOGR("EMISR/data/mpa/","mpa",encoding="Latin1")`

then mpa was splitted in two: mpa1 and mpa2 (file size limitation of github).
See `?mpa` in R for details and references.

The shapefile of the Exclusice Economic Zone of countries EEZ boundaries 
was downloaded from #' \url{http://www.marineregions.org} the 05/12/2014.
The shapefile was converted in SpatialPolygonsDataFrame using rgdal:

`eez<-readOGR("World_EEZ_v8_20140228","World_EEZ_v8_2014_HR",encoding="Latin1")`

then eez was saved in the rdata format.
This database can be accessed using webservices at <http://www.marineregions.org/webservices.php>.See `?eez` for details and references.


