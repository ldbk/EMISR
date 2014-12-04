#' This function downloads a time series of a variable from EMIS for a period of time month on a spatial area
#' using the wcs-t service
#'
#' @param name A character vector of the shortname of the variable
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "2km"
#' @param startdate A character vector of the month who begins the series("YYYY-MM")
#' @param enddate A character vector of the month who ends the series("YYYY-MM")
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A rasterstack object of the variable for the given period
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'	#extraction of the MODIS chlorophyll a concentration monthly maps for 2005 and 2006 on the gulf of Syrte (Lybia)
#' 	img<-getemisdataseries("EMIS_A_CHLA","4km","2005-01","2006-12",15,20.5,30,32.5)
#'	plot(img)
#'   } 
#'
getemisdataseries<-function(name="EMIS_A_CHLA",resolution="4km",startdate="2005-09",enddate="2005-10",xmin=15,xmax=20.5,ymin=30,ymax=32.5){

 checkparameter<-data.frame(name=FALSE,resolution=FALSE,startdate=FALSE,enddate=FALSE,bbox=FALSE)
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

  #check time format for startdate
  n0<-nchar(startdate)==7
  n1<-substr(startdate,1,1)%in%1:2
  n2<-substr(startdate,2,2)%in%c(9,0)
  n3<-substr(startdate,3,3)%in%0:9
  n4<-substr(startdate,4,4)%in%0:9
  n5<-substr(startdate,5,5)=="-"
  n6<-substr(startdate,6,6)%in%c(0,1)
  n7<-substr(startdate,7,7)%in%0:9
  if(!(n0&n1&n2&n3&n4&n5&n6&n7)){
     checkparameter$startdate<-FALSE
     print("Start date format is not correct (YYYY-MM)")
  }else{
     checkparameter$startdate<-TRUE
  }
  #check time format for enddate
  n0<-nchar(enddate)==7
  n1<-substr(enddate,1,1)%in%1:2
  n2<-substr(enddate,2,2)%in%c(9,0)
  n3<-substr(enddate,3,3)%in%0:9
  n4<-substr(enddate,4,4)%in%0:9
  n5<-substr(enddate,5,5)=="-"
  n6<-substr(enddate,6,6)%in%c(0,1)
  n7<-substr(enddate,7,7)%in%0:9
  if(!(n0&n1&n2&n3&n4&n5&n6&n7)){
     checkparameter$enddate<-FALSE
     print("End date format is not correct (YYYY-MM)")
  }else{
     checkparameter$enddate<-TRUE
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
  
  if(apply(checkparameter,1,sum)==5){
     #check date on EMIS time range
     mindate<-strptime(paste(data_emis$startdate[idvar],"15",sep="-"),"%Y-%m-%d")
     maxdate<-strptime(paste(data_emis$enddate[idvar],"15",sep="-"),"%Y-%m-%d")
     askedstartdate<-strptime(paste(startdate,"15",sep="-"),"%Y-%m-%d")
     if((mindate<=askedstartdate)&(askedstartdate<=maxdate)){
     	checkparameter$startdate<-TRUE
	}else{
     	checkparameter$startdate<-FALSE
     	print(paste(name,"is not available on EMIS for the",date))
     }
     askedenddate<-strptime(paste(enddate,"15",sep="-"),"%Y-%m-%d")
     if((mindate<=askedenddate)&(askedenddate<=maxdate)){
     	checkparameter$enddate<-TRUE
	}else{
     	checkparameter$enddate<-FALSE
     	print(paste(name,"is not available on EMIS for the",date))
     }
     if((askedenddate>=askedstartdate)){
     	checkparameter$enddate<-TRUE
	}else{
     	checkparameter$enddate<-FALSE
     	print(paste(startdate,"is older than", enddate))
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

  if(apply(checkparameter,1,sum)==5){
 	# define timeindex"
   	startyear<-as.numeric(substr(startdate,1,4))
   	startmonth<-as.numeric(substr(startdate,6,7))
   	endyear<-as.numeric(substr(enddate,1,4))
   	endmonth<-as.numeric(substr(enddate,6,7))
        if(startyear<endyear){  
 	 timeindex1<-paste(startyear,sprintf("%02d",startmonth:12),sep="-")
 	 timelist<-expand.grid(1:12,(startyear+1):(endyear-1))
	 timeindex2<-paste(timelist[,2],sprintf("%02d",timelist[,1]),sep="-")
 	 timeindex3<-paste(endyear,sprintf("%02d",1:endmonth),sep="-")
  	 if((endyear-startyear)==1){
 	 	timeindex<-c(timeindex1,timeindex3)
         }else{
 	 	timeindex<-c(timeindex1,timeindex2,timeindex3)
         }
        }else{
 	 timeindex<-paste(startyear,sprintf("%02d",startmonth:endmonth),sep="-")
        }
 	# loop on timeindex and create a brick raster object"
         pb<-txtProgressBar(min=0,max=length(timeindex))
	 for(i in 1:length(timeindex)){
  		if(i<=1){
   		imgs<-getemisdata(name=name,resolution=resolution,date=timeindex[i],xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)
   		names(imgs)<-timeindex[i]
  		}else{
   		img<-getemisdata(name=name,resolution=resolution,date=timeindex[i],xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)
   		names(img)<-timeindex[i]
   		imgs<-stack(imgs,img)
  		}	
         	setTxtProgressBar(pb, i)
 	}
        close(pb)
  	return(imgs)
 }

}
