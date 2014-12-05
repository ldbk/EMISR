#' This function downloads a variable from GMIS for a month on a spatial area
#' using the wcs-t service
#'
#' @param name A character vector of the shortname of the variable
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "9km"
#' @param date A character vector of the month ("YYYY-MM")
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A rasterlayer object of the map of the variable for the given month 
#' @keywords GMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'	#extraction of the MODIS chlorophyll a concentration for Septembre 2009 in the vicinity of the Bird Island area (South Africa)
#'	img<-getgmisdata("GMIS_A_CHLA","4km","2005-09",xmin=26.1,xmax=26.5,ymin=-34,ymax=-33.5)
#'	plot(img)
#'   } 
#'
getgmisdata<-function(name="GMIS_A_CHLA",resolution="4km",date="2005-09",xmin=15,xmax=20.5,ymin=30,ymax=32.5){
  checkparameter<-data.frame(name=FALSE,resolution=FALSE,date=FALSE,bbox=FALSE)
  #check the spatial resolution
  if(resolution%in%c("4km","9km")){
	checkparameter$resolution<-TRUE
   	if(resolution=="4km"){data_gmis<-data_gmis_4km}
   	if(resolution=="9km"){data_gmis<-data_gmis_9km}
   	#check variable name
   	idvar<-grep(name,data_gmis$shortname)
   	if(length(idvar)==0){
    		checkparameter$name<-FALSE
    		print("Variable name do not exist on GMIS")
        }else{
    		checkparameter$name<-TRUE
        }
  }else{
   	print("Spatial resolution should be 4km or 9km")
  }

  #check time format
  n0<-nchar(date)==7
  n1<-substr(date,1,1)%in%1:2
  n2<-substr(date,2,2)%in%c(9,0)
  n3<-substr(date,3,3)%in%0:9
  n4<-substr(date,4,4)%in%0:9
  n5<-substr(date,5,5)=="-"
  n6<-substr(date,6,6)%in%c(0,1)
  n7<-substr(date,7,7)%in%0:9
  if(!(n0&n1&n2&n3&n4&n5&n6&n7)){
     checkparameter$date<-FALSE
     print("Date format is not correct (YYYY-MM)")
  }else{
     checkparameter$date<-TRUE
  }

  #check bbox format
  n1<-is.numeric(xmin)
  n2<-length(xmin)==1
  n3<-is.numeric(xmax)
  n4<-length(xmax)==1
  n5<-is.numeric(ymin)
  n6<-length(ymin)==1
  n7<-is.numeric(ymax)
  n8<-length(ymax)==1
  n9<-xmin<xmax
  n10<-ymin<ymax
  if(!(n1&n2&n3&n4&n5&n6&n7&n8)){
     checkparameter$bbox<-FALSE
     print("Spatial limits are not correct (1 numeric vector for xmin, xmax, ymin and ymax)")
  }else{
     checkparameter$bbox<-TRUE
  }
  if(!(n9&n10)){
      checkparameter$bbox<-FALSE
      print("Spatial limits are not ordered correctly (xmin<xmax AND ymin<ymax)")
  }
  
  if(apply(checkparameter,1,sum)==4){
     #check date on GMIS time range
     mindate<-strptime(paste(data_gmis$startdate[idvar],"15",sep="-"),"%Y-%m-%d")
     maxdate<-strptime(paste(data_gmis$enddate[idvar],"15",sep="-"),"%Y-%m-%d")
     askeddate<-strptime(paste(date,"15",sep="-"),"%Y-%m-%d")
     if((mindate<=askeddate)&(askeddate<=maxdate)){
     	checkparameter$date<-TRUE
	}else{
     	checkparameter$date<-FALSE
     	print(paste(name,"is not available on GMIS for the",date))
     }
     #check the bounding box
     bboxgmis<-unlist(strsplit(data_gmis$bbox[idvar]," "))
     xmingmis<-as.numeric(bboxgmis[1])
     xmaxgmis<-as.numeric(bboxgmis[3])
     ymingmis<-as.numeric(bboxgmis[2])
     ymaxgmis<-as.numeric(bboxgmis[4])
     n1<-xmingmis<=xmin
     n2<-xmax<=xmaxgmis
     n3<-ymingmis<=ymin
     n4<-ymax<=ymaxgmis
     if(n1&n2&n3&n4){
     	checkparameter$bbox<-TRUE
     }else{
     	checkparameter$bbox<-FALSE
     	print(paste("The selected area is not strickly inside the spatial extent of", name,"in GMIS"))
     }
  }

  if(apply(checkparameter,1,sum)==4){
  	#define WCS connection"
        bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  	con<-paste("http://gmis.jrc.ec.europa.eu/webservices/",resolution,"/wcs-t?TIME=",date,"&service=wcs&version=1.0.0&request=getcoverage&coverage=",name,"&crs=EPSG:4326&BBOX=",bbox,"&format=image/tiff&interpolation=nearest",sep="")
  	#download the image file"
  	nomfich<-paste(name,date,"img.tiff",sep="_")
	nomfich<-tempfile(nomfich)
  	download(con,nomfich,quiet=TRUE,mode="wb")
  	#return the corresponding raster"
  	img<-raster(nomfich)
  	img[img==0]<-NA
  	#log inverse backtransform if chl or k490 data
  	if(length(grep("log",data_gmis$unit[idvar],ignore.case=TRUE))>0){
   		img<-10^img
  	}
        names(img)<-paste(name,date)
  	return(img)
 }
}
