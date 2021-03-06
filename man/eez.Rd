% Generated by roxygen2 (4.0.2): do not edit by hand
\name{eez}
\alias{eez}
\title{Exclusice Economic Zone of countries boundaries}
\format{A SpatialPolygonsDataFrame with 249 features}
\source{
\url{http://www.marineregions.org}
}
\description{
The shapefile of the Exclusice Economic Zone of countries EEZ boundaries
was downloaded from
\url{http://www.marineregions.org} the 05/12/2014.
The shapefile was converted in SpatialPolygonsDataFrame using rgdal:
\cote{ eez<-readOGR("World_EEZ_v8_20140228","World_EEZ_v8_2014_HR",encoding="Latin1")}
then eez was saved in the rdata format.
This database can be accessed using webservices at \url{http://www.marineregions.org/webservices.php}.
}
\details{
14 variables described the EEZ (names, countries...), see the references for more information.
}
\examples{
\dontrun{
     #extraction of EEZ of the european countries
	europe<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","CzechRepublic","Denmark",
               "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
               "Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland",
               "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")
     eez_europe<-eez[eez@data$Country\%in\%europe,]
     plot(eez_europe)
  }
}
\references{
Claus S., N. De Hauwere, B. Vanhoorne, F. Souza Dias, F. Hernandez, and J. Mees (Flanders Marine Institute) (2014). MarineRegions.org. Accessed at http://www.marineregions.org on 2014-12-05.
}

