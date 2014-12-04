#' This function downloads a variable from EMIS for a month on a spatial area
#' using the wcs-t service
#'
#' @param name A character vector of the shortname of the variable
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "2km"
#' @param date A character vector of the month ("YYYY-MM")
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A rasterlayer object of the map of the variable for the given month 
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'	#extraction of the MODIS chlorophyll a concentration for Septembre 2009 on the gulf of Syrte (Lybia)
#' 	img<-getemisdata("EMIS_A_CHLA","4km","2005-09",15,20.5,30,32.5)
#'	plot(img)
#'   } 
#'
getemisdata<-function(name="EMIS_A_CHLA",resolution="4km",date="2005-09",xmin=15,xmax=20.5,ymin=30,ymax=32.5){
  checkparameter<-data.frame(name=FALSE,resolution=FALSE,date=FALSE,bbox=FALSE)
  #check the spatial resolution
  if(resolution%in%c("2km","4km")){
	checkparameter$resolution<-TRUE
   	if(resolution=="4km"){data_emis<-data_emis_4km}
   	if(resolution=="2km"){data_emis<-data_emis_2km}
   	#check variable name
   	idvar<-grep(name,data_emis$shortname)
   	if(length(idvar)==0){
    		checkparameter$name<-FALSE
    		print("Variable name do not exist on EMIS")
        }else{
    		checkparameter$name<-TRUE
        }
  }else{
   	print("Spatial resolution should be 2km or 4km")
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
     #check date on EMIS time range
     mindate<-strptime(paste(data_emis$startdate[idvar],"15",sep="-"),"%Y-%m-%d")
     maxdate<-strptime(paste(data_emis$enddate[idvar],"15",sep="-"),"%Y-%m-%d")
     askeddate<-strptime(paste(date,"15",sep="-"),"%Y-%m-%d")
     if((mindate<=askeddate)&(askeddate<=maxdate)){
     	checkparameter$date<-TRUE
	}else{
     	checkparameter$date<-FALSE
     	print(paste(name,"is not available on EMIS for the",date))
     }
     #check the bounding box
     bboxemis<-unlist(strsplit(data_emis$bbox[idvar]," "))
     xminemis<-as.numeric(bboxemis[1])
     xmaxemis<-as.numeric(bboxemis[3])
     yminemis<-as.numeric(bboxemis[2])
     ymaxemis<-as.numeric(bboxemis[4])
     n1<-xminemis<=xmin
     n2<-xmax<=xmaxemis
     n3<-yminemis<=ymin
     n4<-ymax<=ymaxemis
     if(n1&n2&n3&n4){
     	checkparameter$bbox<-TRUE
     }else{
     	checkparameter$bbox<-FALSE
     	print(paste("The selected area is not strickly inside the spatial extent of", name,"in emis"))
     }
  }

  if(apply(checkparameter,1,sum)==4){
  	#define WCS connection"
        bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  	con<-paste("http://emis.jrc.ec.europa.eu/webservices/",resolution,"/wcs-t?TIME=",date,"&service=wcs&version=1.0.0&request=getcoverage&coverage=",name,"&crs=EPSG:4326&BBOX=",bbox,"&format=image/tiff&interpolation=nearest",sep="")
  	#download the image file"
  	nomfich<-paste(name,date,"img.tiff",sep="_")
  	download.file(con,nomfich,quiet=TRUE,mode="wb")
  	#return the corresponding raster"
  	img<-raster(nomfich)
  	remove(nomfich)
  	img[img==0]<-NA
  	#log inverse backtransform if chl or k490 data
  	if(length(grep("log",data_emis$unit[idvar],ignore.case=TRUE))>0){
   		img<-10^img
  	}
        names(img)<-paste(name,date)
  	return(img)
 }
}
