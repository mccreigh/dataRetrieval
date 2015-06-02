#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov} service calls.
#'
#' @param service string. Possible values are "iv" (for instantaneous), "dv" (for daily values), "gwlevels" 
#' (for groundwater levels), "site" (for site service), and "qw" (water-quality). Note: "qw" calls go to: 
#' \url{http://nwis.waterdata.usgs.gov/usa/nwis/qwdata} for data requests, and use different call requests schemes. 
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency \tab character \tab The NWIS code for the agency reporting the data\cr
#' site \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (for unit value data), \cr 
#' \tab character \tab or raw character string \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
#' X_D_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' statisticInfo \tab data.frame \tab A data frame containing information on the requested statistics on the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' 
#' @seealso \code{\link{renameNWISColumns}},  \code{\link{importWaterML1}}, \code{\link{importRDB1}}
#' @export
#' @examples
#' \dontrun{
#' # Examples not run for time considerations
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010")
#' dataTempUnit <- readNWISdata(sites="03086500", service="iv", parameterCd="00010")
#' #Empty:
#' multiSite <- readNWISdata(sites=c("04025000","04072150"), service="iv", parameterCd="00010")
#' #Not empty:
#' multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", parameterCd="00060")
#' bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
#' 
#' startDate <- as.Date("2013-10-01")
#' endDate <- as.Date("2014-09-30")
#' waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
#'                   service="dv", startDate=startDate, endDate=endDate)
#' siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",
#'                   hasDataTypeCd="iv", service="site")
#' qwData <- readNWISdata(bBox=c(-82.5,41.52,-81,41),startDate=as.Date("2000-01-01"),
#'                   drain_area_va_min=50, qw_count_nu=50,qw_attributes="expanded",
#'                   qw_sample_wide="wide",list_of_search_criteria=c("lat_long_bounding_box",
#'                   "drain_area_va","obs_count_nu"),service="qw")
#' }
readNWISdata <- function(service="dv", ...){
  urlCall    <- readNWISdataUrlCall(service=service, ...)
  importList <- readNWISdataImport(urlCall, service=service)
  readNWISdataRetval(importList, service)
}
 
#' @export
readNWISdataUrlCall <- function(service, ...) {
  matchReturn <- list(...)
  
  match.arg(service, c("dv","iv","gwlevels","site", "uv","qw","qwdata"))
  
  if(service == "uv"){
    service <- "iv"
  } else if (service == "qw"){
    service <- "qwdata"
  }
  
  if(length(service) > 1){
    stop("Only one service call allowed.")
  }
  
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=",",sep=""))))
  
  names(values)[names(values) == "startDate"] <- "startDT"
  names(values)[names(values) == "endDate"] <- "endDT"
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  format <- "waterml,1.1"
  #format <- "waterml,2.0" ## jlm this should probably be a func arg.
  baseURL <- "http://waterservices.usgs.gov/nwis/"
  
  if(service == "iv"){
    baseURL <- "http://nwis.waterservices.usgs.gov/nwis/"
  } else if (service == "qwdata"){
    baseURL <- "http://nwis.waterdata.usgs.gov/nwis/"

    format <- "rdb"
    
    names(values)[names(values) == "startDT"] <- "begin_date"
    names(values)[names(values) == "endDT"] <- "end_date"
    
    values["rdb_inventory_output"] <- "file"
    values["TZoutput"] <- "0"
    values["date_format"] <- "YYYY-MM-DD"
    values["qw_sample_wide"] <- "wide"
    
    if("bBox" %in% names(values)){
      values["nw_longitude_va"] <- as.character(matchReturn$bBox[1])
      values["nw_latitude_va"] <- as.character(matchReturn$bBox[2])
      values["se_longitude_va"] <- as.character(matchReturn$bBox[3])
      values["se_latitude_va"] <- as.character(matchReturn$bBox[4])
      values["coordinate_format"] <- "decimal_degrees"
      values <- values[-which("bBox" %in% names(values))] 
    }
    
  }
  
  if(service == "site"){
    format <- "rdb"
  }
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  baseURL <- paste0(baseURL,service,"/?format=",format,"&")
  urlCall <- paste0(baseURL,urlCall)
  urlCall
}
  
  
#' @export
readNWISdataImport <- function(urlCall, service) {
  if(service == "site"){
    importList <- importRDB1(urlCall)
  } else if(service != "qwdata") {    
    importList <- ImportWaterMlJlm(urlCall)
    if("dv" == service){
      retval$dateTime <- as.POSIXct(retval$dateTime)
    }
  } else {
    importList <- importRDB1(urlCall)    
  }
  importList
}

#' @export
readNWISdataRetval <- function(importList, service) {
  ##should the following be de-couple from the above?
  if(service == "site"){
    retval <- ParseRDB1Jlm(importList, asDateTime = FALSE, qw = FALSE)
  } else if(service != "qwdata") {
    retval <- ParseWaterML(importList, asDateTime = ("iv" == service), 
                           filterV=NULL, filterT=NULL)
  } else {
    possibleError <- tryCatch(
      {retval <- ParseRDB1Jlm(importList, asDateTime = TRUE, qw = TRUE)},
      error = function(e) {stop(e, "with url:", importList$obs_url)} )
  }
  retval
}

    