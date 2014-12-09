# read_emis_wcst_data
#' This function downloads the list and the caracteristics of the EMIS data
#'
#' This function parse the xml file related to the wcs-t service of EMIS
#' and generate a dataframe with the description of the data
#'  
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "2km"
#' @param emis_wcst_url A character vector of the EMIS wcs-t url
#' 
#' @export
#' @return A dataframe with 8 columns 
#'     \item{source:}{The data source}
#'     \item{resolution:}{Spatial resolution of the data (pixel size)}
#'     \item{name:}{Name of the parameters}
#'     \item{shortname:}{Shortname of the parameters used in the wcs-t service}
#'     \item{unit:}{unit}
#'     \item{bbox:}{Geographical bounding box (xmin,ymin,xmax,ymax)}
#'     \item{startdate:}{Starting date of the time series}
#'     \item{enddate:}{End date of the time series}
#' @keywords EMIS, wcs-t
#' @examples
#'   \dontrun{
#' data_emis_4km<-read_emis_wcst_data(resolution="4km",emis_wcst_url="http://emis.jrc.ec.europa.eu/webservices/4km/wcs-t")
#' data_emis_2km<-read_emis_wcst_data(resolution="2km",emis_wcst_url="http://emis.jrc.ec.europa.eu/webservices/2km/wcs-t")
#'   } 
#'
read_emis_wcst_data<-function(resolution="4km",emis_wcst_url=paste("http://emis.jrc.ec.europa.eu/webservices/",resolution,"/wcs-t",sep="")){
 if(url.exists(emis_wcst_url)){
 doc<-xmlTreeParse(emis_wcst_url, isURL=TRUE,encoding="latin1")
 r<-xmlRoot(doc)
 nbvar<-xmlSize(r[[3]])
 carac<-data.frame(source=rep(unlist(r[[1]][[2]])[3],nbvar),resolution,name=NA,shortname=NA,unit=NA,bbox=NA,startdate=NA,enddate=NA)
 for(i in 1:nbvar){
   carac$unit[i]<-unlist(r[[3]][[i]][[1]])[3]
   carac$shortname[i]<-unlist(r[[3]][[i]][[2]])[3]
   carac$name[i]<-unlist(r[[3]][[i]][[3]])[3]
   carac$bbox[i]<-gsub("
",",",paste(unlist(r[[3]][[i]][[4]])[5],unlist(r[[3]][[i]][[4]])[9],collapse="
"))

   tps<-unlist(r[[3]][[i]][[4]])
   tps<-as.vector(tps[names(tps)=="children.timePosition.children.text.value"])
   tps<-strptime(paste(tps,"15",sep="-"),"%Y-%m-%d")
   carac$startdate[i]<- format(min(tps,na.rm=T),"%Y-%m")
   carac$enddate[i]<- format(max(tps,na.rm=T),"%Y-%m")
  }
  }else{
   print("no internet connection or EMIS server down")
   carac<-data.frame(source=NA,name=NA,shortname=NA,unit=NA,bbox=NA,time=NA)
 }
 return(carac)
 }

#' EMIS 4km data description
#'
#' A dataframe containing the description of the data available in EMIS 
#' at 4 km spatial resolution.
#' The variables are as follows:
#'
#' \itemize{
#'     \item{source:}{The data source}
#'     \item{resolution:}{Spatial resolution of the data (pixel size)}
#'     \item{name:}{Name of the parameters}
#'     \item{shortname:}{Shortname of the parameters used in the wcs-t service}
#'     \item{unit:}{unit}
#'     \item{bbox:}{Geographical bounding box (xmin,ymin,xmax,ymax)}
#'     \item{startdate:}{Starting date of the time series}
#'     \item{enddate:}{End date of the time series}
#' }
#'
#' @format A data frame with 35 rows and 8 variables
#' @source \url{http://emis.jrc.ec.europa.eu/webservices/4km/wcs-t}
#' @name data_emis_4km
NULL


#' EMIS 2km data description
#'
#' A dataframe containing the description of the data available in EMIS 
#' at 2 km spatial resolution.
#' The variables are as follows:
#'
#' \itemize{
#'     \item{source:}{The data source}
#'     \item{resolution:}{Spatial resolution of the data (pixel size)}
#'     \item{name:}{Name of the parameters}
#'     \item{shortname:}{Shortname of the parameters used in the wcs-t service}
#'     \item{unit:}{unit}
#'     \item{bbox:}{Geographical bounding box (xmin,ymin,xmax,ymax)}
#'     \item{startdate:}{Starting date of the time series}
#'     \item{enddate:}{End date of the time series}
#' }
#'
#' @format A data frame with 18 rows and 8 variables
#' @source \url{http://emis.jrc.ec.europa.eu/webservices/2km/wcs-t}
#' @name data_emis_2km
NULL
