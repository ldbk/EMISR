#' Sea surface temperature of the Pantelleria Marine Protected Area
#'
#' The SST was downloaded using:
#' \code{pantelleria_sst<-mpaextract("EMIS_T_SST","2km","2009-01","2012-12",555540558)}
#'
#' @format A RasterStack with 48 layers
#' @source \url{http://emis.jrc.ec.europa.eu/emis_6_0.php}
#' @name pantelleria_sst
NULL

#' Chlorophyll a surface concentration of the Pantelleria Marine Protected Area
#'
#' The chl was downloaded using:
#' \code{pantelleria_chl<-mpaextract("EMIS_A_CHLA","2km","2009-01","2012-12",555540558)}
#'
#' @format A RasterStack with 48 layers
#' @source \url{http://emis.jrc.ec.europa.eu/emis_6_0.php}
#' @name pantelleria_chl
NULL

#' Pantelleria Marine Protecte Area
#'
#' The polygon was extracted from the mpa2 file using the wdpid 555540558
#'
#' @format A SpatialPolygonsDataFrame
#' @source \url{http://www.protectedplanet.net}
#' @name pantelleria_mpa
NULL
