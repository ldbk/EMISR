#' This function compute a set of indicators based on a series of map of a EMIS/GMIS parameter
#' and draw some maps and plots related to the parameter. 
#'
#' The parameters are monthly data available on EMIS or GMIS.
#' "name" gives the short name of the parameter processed.
#' "timerange" is the time range of the parameter.
#' "pixelnb" gives the number of 4 by 4 km pixels covering the study area: this area is the geographical extent of the MPA polygon (ie a rectangle encompassing the MPA).
#' "fracnbna" is the percentage of missing data in the MPA polygon over the time range. 
#' "fractsna" is the percentage of missing value for the time series of the parameters. This time serie is calculated using the monthly mean of the pixel values INSIDE the MPA polygon.
#' "mean" is the average value of the parameter INSIDE the MPA polygon over the time range.
#' "sd" is the standard deviation of the parameter values INSIDE the MPA polygon over the time range.
#' "min" is the minimum of the parameter values INSIDE the MPA polygon over the time range.
#' "max" is the maximum of the parameter values INSIDE the MPA polygon over the time range.
#' The monthly time series X(t) of the parameter is decomposed as  X(t)=S(t)+T(t)+I(t) (see Vantrepotte and Melin, 2009), where S, T, and I represent, respectively, a seasonal signal, a trend, and an irregular (or residual) component (or remainder). These components are presented in the figure seasonal composition. In this framework:
#' "varseason" is the percentage of variance associated the seasonal component and "vartrend" the percentage of variance associated with the linear component (the trend).
#' A statistical test (Kendall's tau test) is performed on the linear component in order to assess the significance of a monotic trend:
#' "trendtest" gives the p.value associated to this test.
#' "senslope" gives the value of the Sen slope (alternately, Theil slope). This value is the median
#' slope joining all pairs of observations and is expressed by
#' quantity per year.
#
#' References:
#' Vantrepotte, V. & Melin, F. Temporal variability of 10-year global SeaWiFS time-series of phytoplankton chlorophyll a concentration ICES J. Mar. Sci., 2009, 66, 1547-1556
#      
#' @param name A character vector of the shortname of the variable
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "2km"
#' @param startdate A character vector of the month who begins the series("YYYY-MM")
#' @param enddate A character vector of the month who ends the series("YYYY-MM")
#' @param wdpaid An integer of the ID of the marine protected area corresponding to the wdpaid of the mpa 
#' 
#' @export
#' @return A list of object containing the plot and the statistics
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'      #analysis of the MODIS sea surface temperature at 2 km 2005 and 2006 on the Pantelleira mpa (Italy)
#'	pltstat<-mpaprocess("EMIS_T_SST","2km","2005-01","2007-12",555540558)
#'	#map of the whole series
#'	pltstat[[1]]$pltall
#'	#map of the average SST
#'	pltstat[[1]]$pltmean
#'	#map of the climatology
#'	pltstat[[1]]$pltclim
#'	#some statistics
#'	pltstat[[2]]$stat
#'	#time series decomposition of the parameters averaged on the MPA area
#'	pltstat[[2]]$pltts
#'   } 
#'
mpaprocess<-function(name="EMIS_A_CHLA",resolution="4km",startdate="2005-09",enddate="2005-10",wdpaid=555540558){ 
 #check the mpa
 checkmpa1<- length(which(mpa1@data$wdpaid==wdpaid))
 checkmpa2<- length(which(mpa2@data$wdpaid==wdpaid))
 if((checkmpa1+checkmpa2)!=1){
  	print(paste("The MPA", wdpaid, "do not exist in the mpa database. Please check your wdpaid"))
 }else{
 	#extract the polygon of the mpa corresponding to the wdpaid
 	if(wdpaid<=4000){
  		mpa<-mpa1[mpa1@data$wdpaid==wdpaid,]
  		proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")
 	}else{
  		mpa<-mpa2[mpa2@data$wdpaid==wdpaid,]
  		proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")
 	}
 	#download the parameter from EMIS/GMIS
 	imgs<-mpaextract(name=name,resolution=resolution,startdate=startdate,enddate=enddate,wdpaid=wdpaid) 
 	#
 	if(resolution=="4km"){data_emis<-data_emis_4km}
        if(resolution=="2km"){data_emis<-data_emis_2km}
 	idvar<-grep(name,data_emis$shortname)
	if(length(grep("log",data_emis$unit[idvar],ignore.case=TRUE))>0){
                unite<-substr(data_emis$unit[idvar],7,nchar(data_emis$unit[idvar]))
        }else{
		unite<-data_emis$unit[idvar]
	}
  	#plot the data
	plt<-mpaprocessplot(imgs=imgs,mpa=mpa,name=name,unite=unite,logscale=FALSE)
 	stat<-mpaprocessstat(imgs=imgs,mpa=mpa,name=name,unite=unite)
	return(list(plt,stat))
 }
}

#' This function draw figures based on a series of map of a EMIS/GMIS parameter for a marine protected area
#'
#' @param imgs A rasterstack of the series of map extracted using mpaextract 
#' @param mpa A SpatialPolygonsDataFrame of the marine protected area of interest
#' @param name A character vector of the variable name
#' @param unite A character vector of the unit of variable name
#' @param logscale A boolean defining the use of a logscale on a graph (TRUE) or not (FALSE)
#' 
#' @export
#' @return A list of plots
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'      #analysis of the MODIS sea surface temperature at 2 km 2009 and 2012 on the Pantelleira mpa (Italy)
#'	plt<-mpaprocessplot(imgs=pantelleria_sst,mpa=pantelleria_mpa,name="EMIS_T_SST",unite="oC",logscale=FALSE)
#'	#map of the whole series
#'	plt[[1]]
#'	#map of the average SST
#'	plt[[2]]
#'	#map of the climatology
#'	plt[[3]]
#'	#boxplot of the climatology
#'	plt[[4]]
#'   } 
#'
mpaprocessplot<-function(imgs,mpa,name,unite,logscale){
 if(dim(imgs)[3]<12){
  	print("Series is too short (ie less than 12 months)")
 }else{
 	#precomputation
	 #average
	 imgsmean<-mean(imgs,na.rm=T)
	 # monthly climatology
	 imgsclim<-stackApply(imgs,indices=as.numeric(substr(names(imgs),7,8)),fun=mean,na.rm=TRUE)
	 names(imgsclim)<-paste("Mean",unique(as.numeric(substr(names(imgs),7,8))))
 	#plot
	 titre<-paste(name," (",unite,")",sep="")
	 pltall<-levelplot(imgs,zscaleLog=logscale,contour=T,col.regions=topo.colors(100),names=substr(names(imgs),2,8),main=titre)
	 titre<-paste(name," (",unite,") ",substr(names(imgs)[1],2,8),"-",substr(names(imgs)[dim(imgs)[3]],2,8)," average",sep="")
	 pltmean<-levelplot(imgsmean,margin=F,zscaleLog=logscale,contour=T,col.regions=topo.colors(100),main=titre)
	 titre<-paste(name," (",unite,") monthly climatology",sep="")
	 pltclim<-levelplot(imgsclim,zscaleLog=logscale,col.regions=topo.colors(100),main=titre,names=names(imgsclim))
	 titre<-paste(name," (",unite,") monthly boxplot",sep="")
	 pltbw<-bwplot(crop(imgsclim,extent(mpa)),scales=list(x=list(labels=names(imgsclim))),main=titre)
 	#build terretrial polygon
	 mappoly<-map("worldHires",fill=T,plot=FALSE,xlim=c(extent(mpa)@xmin-1,extent(mpa)@xmax+1),ylim=c(extent(mpa)@ymin-1,extent(mpa)@ymax+1))
	 IDs <- sapply(strsplit(mappoly$names, ":"), function(x) x[1])
	 coast<- map2SpatialPolygons(mappoly, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
 	 #add the coast and the mpa to the maps
 	 pltall<-pltall+layer(sp.polygons(coast,fill="grey",col="grey"))+layer(sp.polygons(mpa))
 	 pltmean<-pltmean+layer(sp.polygons(coast,fill="grey",col="grey"))+layer(sp.polygons(mpa))
 	 pltclim<-pltclim+layer(sp.polygons(coast,fill="grey",col="grey"))+layer(sp.polygons(mpa))
         #return the plot in a list
         return(list(pltall=pltall,pltmean=pltmean,pltclim=pltclim,pltbw=pltbw))
 }
}

#' This function compute some indicators for a series of map of a EMIS/GMIS parameter for a marine protected area
#'
#' The parameters are monthly data available on EMIS or GMIS.
#' "name" gives the short name of the parameter processed.
#' "timerange" is the time range of the parameter.
#' "pixelnb" gives the number of 4 by 4 km pixels covering the study area: this area is the geographical extent of the MPA polygon (ie a rectangle encompassing the MPA).
#' "fracnbna" is the percentage of missing data in the MPA polygon over the time range. 
#' "fractsna" is the percentage of missing value for the time series of the parameters. This time serie is calculated using the monthly mean of the pixel values INSIDE the MPA polygon.
#' "mean" is the average value of the parameter INSIDE the MPA polygon over the time range.
#' "sd" is the standard deviation of the parameter values INSIDE the MPA polygon over the time range.
#' "min" is the minimum of the parameter values INSIDE the MPA polygon over the time range.
#' "max" is the maximum of the parameter values INSIDE the MPA polygon over the time range.
#' The monthly time series X(t) of the parameter is decomposed as  X(t)=S(t)+T(t)+I(t) (see Vantrepotte and Melin, 2009), where S, T, and I represent, respectively, a seasonal signal, a trend, and an irregular (or residual) component (or remainder). These components are presented in the figure seasonal composition. In this framework:
#' "varseason" is the percentage of variance associated the seasonal component and "vartrend" the percentage of variance associated with the linear component (the trend).
#' A statistical test (Kendall's tau test) is performed on the linear component in order to assess the significance of a monotic trend:
#' "trendtest" gives the p.value associated to this test.
#' "senslope" gives the value of the Sen slope (alternately, Theil slope). This value is the median
#' slope joining all pairs of observations and is expressed by
#' quantity per year.
#
#' References:
#' Vantrepotte, V. & Melin, F. Temporal variability of 10-year global SeaWiFS time-series of phytoplankton chlorophyll a concentration ICES J. Mar. Sci., 2009, 66, 1547-1556

#' @param imgs A rasterstack of the series of map extracted using mpaextract 
#' @param mpa A SpatialPolygonsDataFrame of the marine protected area of interest
#' @param name A character vector of the variable name
#' @param unite A character vector of the unit of variable name
#' 
#' @export
#' @return A list of 2 objects: a dataframe and a plot
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'      #analysis of the MODIS sea surface temperature at 2 km 2009 and 2012 on the Pantelleira mpa (Italy)
#'	stat<-mpaprocessstat(imgs=pantelleria_sst,mpa=pantelleria_mpa,name="EMIS_T_SST",unite="oC")
#'	#statistics
#'	stat[[1]]
#'	#plot of the time series decomposition of the parameter averaged on the MPA surface
#'	stat[[2]]
#'   } 
#'
mpaprocessstat<-function(imgs,mpa,name,unite){
  #############################################
  # compute indicators"
  # check data availability in the polygon and create a dataframe with the corresponding pixel"
  datinpoly<-extract(imgs,mpa,df=TRUE,cellnumbers=TRUE)
  tabdat<-as.vector(as.matrix(datinpoly[,3:ncol(datinpoly)]))
  pixelnb<-nrow(datinpoly)
  #compute the time series for the given pixels
  tabts<-apply(datinpoly[,3:ncol(datinpoly)],2,mean,na.rm=T)
  #number of missing value
  fracnbna<-sum(is.na(tabts))/length(tabts)
  #if more than 20% of the value are not present do not time series analysis
  if(fracnbna>.2){
  print("not enough data for time series analysis")
   vseason<-NA	
   vtrend<-NA	
   vtot<-NA	
   testtrend<-data.frame(p.value=NA,sen.slope=NA)	
   pltts<-NA
  }else{
   startyear<-as.numeric(substr(names(imgs)[1],2,5))
   startmonth<-as.numeric(substr(names(imgs)[1],7,8))
   tsm<-na.approx(ts(tabts, frequency = 12, start = c(startyear, startmonth)),na.rm=FALSE)
   tsmdec<-stl(tsm, s.window=12,na.action=na.omit)
   tsseason<-tsmdec$time.series[,1]
   tsremind<-tsm-tsseason
   linfit<-lm(tsremind~c(1:length(tsremind)))
   tstrend<-ts(predict(linfit), frequency = 12, start = c(startyear,startmonth))
   tsremind<-tsm-tsseason-tstrend
   #check this
   pipo1<-data.frame(time=as.numeric(time(tsm)),value=as.numeric(tsm),name="Time series",type="Original time series and trend")
   pipo2<-data.frame(time=as.numeric(time(tsseason)),value=as.numeric(tsseason),name="Seasonal component",type="Seasonal and remainder components")
   pipo3<-data.frame(time=as.numeric(time(tstrend)),value=as.numeric(tstrend),name="Trend",type="Original time series and trend")
   pipo4<-data.frame(time=as.numeric(time(tsremind)),value=as.numeric(tsremind),name="Residual",type="Seasonal and residual components")
   pipoall<-rbind(pipo1,pipo2,pipo3,pipo4)
   pltts<-ggplot(pipoall,aes(x=time,y=value,group=name))+geom_line()+facet_grid(name~., scales="free")+ylab(paste(name," (",unite,")",sep=""))
   vseason<- var(tsseason,na.rm=T)
   vtrend<- var(tstrend,na.rm=T)
   vremind<- var(tsremind,na.rm=T)
   vtot<-vseason+vtrend+vremind
   #some stat
   #test linear trend
   testtrend<-mannKen(tstrend)
   #return results
  }
  datstat<-data.frame(name=name,unite=unite,timerange=paste(substr(names(imgs)[1],2,8),substr(names(imgs)[dim(imgs)[3]],2,8),sep="/"),pixelnb=pixelnb,fracnbna=round(sum(is.na(tabdat))/length(tabdat)*100,2),fractsna=fracnbna*100,mean=mean(tabdat,na.rm=T),sd=sd(tabdat,na.rm=T),min=min(tabdat,na.rm=T),max=max(tabdat,na.rm=T),varseason=100*vseason/vtot,vartrend=100*vtrend/vtot,trendtest=testtrend$p.value,sen.slope=testtrend$sen.slope)
  return(list(stat=datstat,pltts=pltts))
}
