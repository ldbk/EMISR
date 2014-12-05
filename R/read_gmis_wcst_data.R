# read_gmis_wcst_data
#' This function downloads the list and the caracteristics of the GMIS data
#'
#' This function parse the xml file related to the wcs-t service of GMIS
#' and generate a dataframe with the description of the data
#'  
#' @param resolution A character vector giving the spatial resolution of the data: "4km" or "9km"
#' @param gmis_wcst_url A character vector of the GMIS wcs-t url
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
#' @keywords GMIS, wcs-t
#' @examples
#'   \dontrun{
#' data_gmis_4km<-read_gmis_wcst_data(resolution="4km",gmis_wcst_url="http://gmis.jrc.ec.europa.eu/webservices/4km/wcs-t")
#' data_gmis_9km<-read_gmis_wcst_data(resolution="9km",gmis_wcst_url="http://gmis.jrc.ec.europa.eu/webservices/9km/wcs-t")
#'   } 
#'
read_gmis_wcst_data<-function(resolution="4km",gmis_wcst_url=paste("http://gmis.jrc.ec.europa.eu/webservices/",resolution,"/wcs-t",sep="")){
 if(url.exists(gmis_wcst_url)){
 doc<-xmlTreeParse(gmis_wcst_url, isURL=TRUE,encoding="latin1")
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
   carac$startdate<- format(min(tps),"%Y-%m")
   carac$enddate<- format(max(tps),"%Y-%m")
  }
  }else{
   print("no internet connection or GMIS server down")
   carac<-data.frame(source=NA,name=NA,shortname=NA,unit=NA,bbox=NA,time=NA)
 }
 return(carac)
 }

#' GMIS 4km data description
#'
#' A dataframe containing the description of the data available in GMIS 
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
#' @format A data frame with 30 rows and 8 variables
#' @source \url{http://gmis.jrc.ec.europa.eu/webservices/4km/wcs-t}
#' @name data_gmis_4km
NULL


#' GMIS 9km data description
#'
#' A dataframe containing the description of the data available in GMIS 
#' at 9 km spatial resolution.
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
#' @format A data frame with 30 rows and 8 variables
#' @source \url{http://emis.jrc.ec.europa.eu/webservices/9km/wcs-t}
#' @name data_gmis_9km
NULL
