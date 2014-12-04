#' This function downloads a time series of a variable from EMIS for a period of time month for a given MPA
#' using the wcs-t service. The data are extracted with a buffer of about 10 km around the MPA.
#'
#' @param name A character vector of the shortname of the variable
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "2km"
#' @param startdate A character vector of the month who begins the series("YYYY-MM")
#' @param enddate A character vector of the month who ends the series("YYYY-MM")
#' @param wdpaid An integer of the ID of the marine protected area corresponding to the wdpaid of the mpa 
#' 
#' @export
#' @return A rasterstack object of the variable for the given period on the MPA
#' @keywords EMIS
#' @keywords wcs-t
#' @examples
#'   \dontrun{
#'      #extraction of the MODIS sea surface temperature at 2 km in 2009 and 2012 on the Pantelleria mpa (Italy)
#'      img<-mpaextract("EMIS_T_SST","2km","2009-01","2012-12",555540558)
#'      plot(img)
#'   } 
#'
mpaextract<-function(name="EMIS_A_CHLA",resolution="4km",startdate="2005-09",enddate="2005-10",wdpaid=555540558){
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
        #compute xmin, xmax, ymin, ymax
        xmin<-extent(mpa)@xmin-.1
        xmax<-extent(mpa)@xmax+.1
        ymin<-extent(mpa)@ymin-.1
        ymax<-extent(mpa)@ymax+.1
        #get the requested data
        imgs<-getemisdataseries(name=name,resolution=resolution,startdate=startdate,enddate=enddate,xmin,xmax,ymin,ymax)
        return(imgs)
 }
}

