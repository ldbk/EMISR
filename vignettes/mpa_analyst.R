## ----, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE,comment="#>",fig.width=6,fig.height=6)
library(EMISR)

## ------------------------------------------------------------------------
 wdpaid<-555540558
 mpa<-mpa2[mpa2@data$wdpaid==wdpaid,]
 proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

## ------------------------------------------------------------------------
 #build a terrestrial polygon around the mpa
 mappoly<-map("worldHires",fill=T,plot=FALSE,xlim=c(extent(mpa)@xmin-1,extent(mpa)@xmax+1),ylim=c(extent(mpa)@ymin-1,extent(mpa)@ymax+1))
 IDs <- sapply(strsplit(mappoly$names, ":"), function(x) x[1])
 coast<- map2SpatialPolygons(mappoly, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
 #plot at global scale
 map("worldHires",col="light grey",fill=T)
 points(coordinates(mpa),cex=2,col="blue",pch="+")
 title(paste("MPA",mpa@data$wdpaid,"(",mpa@data$name,")","in",mpa@data$country),cex=.5)
 #local plot
 plot(mpa,xlim=c(extent(mpa)@xmin-1,extent(mpa)@xmax+1),ylim=c(extent(mpa)@ymin-1,extent(mpa)@ymax+1),axes=T,col="red")
 map("worldHires",add=T,col="light grey",fill=T)
 plot(mpa,add=T,col="blue")
 title(paste("MPA",mpa@data$wdpaid,"(",mpa@data$name,")","in",mpa@data$country),cex=.5)


## ----,eval=FALSE---------------------------------------------------------
#  #extraction of the MODIS sea surface temperature at 2 km between 2009 and 2012 on the Pantelleria marine protected area (Italy)
#  pantelleria_sst<-mpaextract("EMIS_T_SST","2km","2009-01","2012-12",555540558)
#  #extraction of the MODIS sea surface temperature at 2 km between 2009 and 2012 on the Pantelleria marine protected area (Italy)
#  pantelleria_chl<-mpaextract("EMIS_A_CHLA","2km","2009-01","2012-12",555540558)

## ----,eval=TRUE----------------------------------------------------------
plt<-mpaprocessplot(imgs=pantelleria_sst,mpa=pantelleria_mpa,name="EMIS_T_SST",unite="oC",logscale=FALSE)
#map of the whole series
plt[[1]]+latticeExtra::layer(sp.polygons(coast,fill="grey",col="grey"))+latticeExtra::layer(sp.polygons(pantelleria_mpa))
#map of the average SST
plt[[2]]+latticeExtra::layer(sp.polygons(coast,fill="grey",col="grey"))+latticeExtra::layer(sp.polygons(pantelleria_mpa))
#map of the climatology
plt[[3]]+latticeExtra::layer(sp.polygons(coast,fill="grey",col="grey"))+latticeExtra::layer(sp.polygons(pantelleria_mpa))
#boxplot of the climatology
plt[[4]]

## ----,eval=TRUE----------------------------------------------------------
datstat<-mpaprocessstat(imgs=pantelleria_sst,mpa=pantelleria_mpa,name="EMIS_T_SST",unite="oC")
#the statistics
datstat[[1]]
#time series decomposition
datstat[[2]]

