#' Function to return data from the NWISWeb WaterML1.1 service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url character containing the url for the retrieval
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = TRUE), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
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
#' @seealso \code{\link{renameNWISColumns}}
#' @export
#' @import XML
#' @import RCurl
#' @import reshape2
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' \dontrun{
#' data <- importWaterML1(obs_url,TRUE)
#' 
#' groundWaterSite <- "431049071324301"
#' startGW <- "2013-10-01"
#' endGW <- "2014-06-30"
#' groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
#'           startGW,endGW, service="gwlevels")
#' groundWater <- importWaterML1(groundwaterExampleURL)
#' 
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2013-11-03","2013-11-03",'uv')
#' unitData <- importWaterML1(unitDataURL,TRUE)
#' 
#' # Two sites, two pcodes, one site has two data descriptors:
#' siteNumber <- c('01480015',"04085427")
#' obs_url <- constructNWISURL(siteNumber,c("00060","00010"),startDate,endDate,'dv')
#' data <- importWaterML1(obs_url)
#' data$dateTime <- as.Date(data$dateTime)
#' data <- renameNWISColumns(data)
#' names(attributes(data))
#' attr(data, "url")
#' attr(data, "disclaimer")
#' 
#' inactiveSite <- "05212700"
#' inactiveSite <- constructNWISURL(inactiveSite, "00060", "2014-01-01", "2014-01-10",'dv')
#' inactiveSite <- importWaterML1(inactiveSite)
#' 
#' inactiveAndAcitive <- c("07334200","05212700")
#' inactiveAndAcitive <- constructNWISURL(inactiveAndAcitive, "00060", "2014-01-01", "2014-01-10",'dv')
#' inactiveAndAcitive <- importWaterML1(inactiveAndAcitive)
#' 
#' }
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "WaterML1Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' imporFile <- importWaterML1(fullPath,TRUE)
#'
zz<-NULL

#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' @examples
#' # jlm test case
#' thenGlobal <- thenFirstGlobal<-lubridate::now()
#' #timeTrack <- function(stamp) {
#'   nowNow <- lubridate::now()
#'   cat(paste0('\n', stamp,': ', 
#'              '\n cumulative: ', format(nowNow-thenFirstGlobal, unit='seconds'), 
#'              '\n section:    ', format(nowNow-thenGlobal,      unit='seconds'),'\n'))
#'   thenGlobal <<- lubridate::now()
#' }
#' # case with no aggregation by length
#' system.time(ret <- dataRetrieval::readNWISdata(service='iv', huc='06', siteStatus='active', 
#'                            parameterCd=c("00060","00065")))
#'                            
#' # case with? agg by length                            
#' system.time(ret <- dataRetrieval::readNWISdata(service='iv', huc='10', siteStatus='active', 
#'                            parameterCd=c("00060","00065")))                            
#'                            
#' # case with? agg by length                            
#' system.time(dataRetrieval::readNWISdata(service='iv', huc='10', siteStatus='active', 
#'                            parameterCd=c("00060","00065")))                            
#'                            
#' No modifications                          
#' Start of importWaterML1: 
#' cumulative: 0.944746 secs
#' section:    0.944746 secs
#' got rawData importWaterML1: 
#' cumulative: 6.835334 secs
#' section:    5.890093 secs
#' returnedDoc importWaterML1: 
#' cumulative: 6.922531 secs
#' section:    0.08674502 secs
#' before mega loop importWaterML1: 
#' cumulative: 6.93187 secs
#' section:    0.008822918 secs
#' after mega loop importWaterML1: ***
#' cumulative: 1.228706 mins
#' section:    1.113166 mins
#' before dcast importWaterML1: 
#' cumulative: 1.229821 mins
#' section:    0.06639314 secs
#' Aggregation function missing: defaulting to length
#' 
#' after dcast importWaterML1: **
#' cumulative: 1.988311 mins
#' section:    45.50783 secs
#' before subsetting NA importWaterML1: 
#' cumulative: 1.98832 mins
#' section:    0.0001008511 secs
#' after subsetting NA importWaterML1: 
#' cumulative: 2.012457 mins
#' section:    1.447874 secs
#' end importWaterML1: 
#' cumulative: 2.016126 mins
#' section:    0.2195818 secs   user  system elapsed 
#' 112.168   2.025 120.023                             
#' 
#' @export
ImportWaterMlJlm <- function(obs_url) {
 
  #timeTrack('Start of importWaterML1')
    
  if(file.exists(obs_url)){
    rawData <- obs_url
  } else {
    rawData <- getWebServiceData(obs_url)
  }
  #timeTrack('got rawData importWaterML1')
    
  list( rawData=rawData, obs_url=obs_url )
}

#' @export
ParseWaterML <- function(import, asDateTime=FALSE, tz="", 
                         filterVar=c("X_00065_00011", "X_00060_00011") ) {
  
  rawData <- import$rawData
  obs_url <- import$obs_url
  
  returnedDoc <- xmlTreeParse(rawData, getDTD = FALSE, useInternalNodes = TRUE)
  #timeTrack('returnedDoc importWaterML1')
  
  doc <- xmlRoot(returnedDoc)
  
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  queryInfo <- xmlToList(xmlRoot(xmlDoc(doc[["queryInfo"]])))
  names(queryInfo) <- make.unique(names(queryInfo))
  
  noteIndex <- grep("note",names(queryInfo))
  noteTitles <- as.character(lapply(queryInfo[noteIndex], function(x) x$.attrs))
  notes <- as.character(lapply(queryInfo[noteIndex], function(x) x$text))
  names(notes) <- noteTitles
  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }

  if(0 == length(timeSeries)){
    df <- data.frame()
    attr(df, "queryInfo") <- queryInfo
    attr(df, "url") <- obs_url
    return(df)
  }
  
  attList <- list()
  dataColumns <- c()
  qualColumns <- c()
  mergedDF <- NULL
  
  #stop()
  
  #timeTrack('before mega loop importWaterML1')
  for (i in 1:length(timeSeries)){
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)  
      
    uniqueName <- as.character(xpathApply(chunk, "@name", namespaces = chunkNS))
    site <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode", namespaces = chunkNS, xmlValue))
    agency <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode/@agencyCode", namespaces = chunkNS))
    pCode <-as.character(xpathApply(chunk, "ns1:variable/ns1:variableCode", namespaces = chunkNS, xmlValue))
    statCd <- as.character(xpathApply(chunk, "ns1:variable/ns1:options/ns1:option[@name='Statistic']/@optionCode", namespaces = chunkNS))
    statName <- as.character(xpathApply(chunk, "ns1:variable/ns1:options/ns1:option[@name='Statistic']", namespaces = chunkNS, xmlValue))
    noValue <- as.numeric(xpathApply(chunk, "ns1:variable/ns1:noDataValue", namespaces = chunkNS, xmlValue))
    
    extraSiteData <-  xmlToList(xmlRoot(xmlDoc(chunk[["sourceInfo"]])))
    extraVariableData <-  xmlToList(xmlRoot(xmlDoc(chunk[["variable"]])))
    
    valuesIndex <- as.numeric(which("values" == names(chunk)))

        
    zoneAbbrievs <- c(as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:defaultTimeZone/@zoneAbbreviation", namespaces = chunkNS)),
                      as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:daylightSavingsTimeZone/@zoneAbbreviation", namespaces = chunkNS)))
    names(zoneAbbrievs) <- c(as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:defaultTimeZone/@zoneOffset", namespaces = chunkNS)),
                      as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:daylightSavingsTimeZone/@zoneOffset", namespaces = chunkNS)))
    

    #stop()
    for (j in valuesIndex){
      subChunk <- xmlRoot(xmlDoc(chunk[[j]]))
      
      methodID <- as.character(xpathSApply(subChunk, "ns1:method/@methodID", namespaces = chunkNS))
      
      methodID <- zeroPad(methodID,2)
      
      value <- as.numeric(xpathSApply(subChunk, "ns1:value",namespaces = chunkNS, xmlValue))        
      
      if(length(value)!=0){
      
        value[value == noValue] <- NA
            
        attNames <- xpathSApply(subChunk, "ns1:value/@*",namespaces = chunkNS)
        attributeNames <- unique(names(attNames))
  
        ## really, x? how about attributeValues or attVals
        x <- lapply(attributeNames, function(x) xpathSApply(subChunk, paste0("ns1:value/@",x),namespaces = chunkNS))
        
        
        methodDescription <- as.character(xpathApply(subChunk, "ns1:method/ns1:methodDescription", namespaces = chunkNS, xmlValue))
        
        ### jlm can filter on this.
        ### if value name is not in some predefined set, next
        valueName <- paste("X",pCode,statCd,sep="_")        
        if(length(methodDescription) > 0 && methodDescription != ""){
          valueName <- paste("X",methodDescription,pCode,statCd,sep="_") 
        }
        if(length(filterVar)) if(!(valueName %in% filterVar)) next
        
        assign(valueName,value)
        
        ## "atomic" df
        df <- data.frame(agency_cd = rep(agency,length(value)),
                         site_no = rep(site,length(value)),
                         stringsAsFactors=FALSE)
        
        ## put attributes into the df
        if(length(attributeNames) > 0){
          for(k in 1:length(attributeNames)){
            attVal <- as.character(x[[k]])
            if(length(attVal) == nrow(df)){
              df$temp <- as.character(x[[k]])              
            } else {
              attrList <- xpathApply(subChunk, "ns1:value", namespaces = chunkNS, xmlAttrs)
              df$temp <- sapply(1:nrow(df),function(x) as.character(attrList[[x]][attributeNames[k]]))
              df$temp[is.na(df$temp)] <- ""
            }
            names(df)[which(names(df) %in% "temp")] <- attributeNames[k]
          }
        }
        
        ## put the value into the df
        df <- cbind(df, get(valueName))
        names(df)[length(df)] <- valueName
        
        ## qualifiers are what I think of as codes
        ## note tracking these columns by name
        if("qualifiers" %in% names(df)){
          qualName <- paste(valueName,"cd",sep="_")
          names(df)[which(names(df) == "qualifiers")] <- qualName
          qualColumns <- c(qualColumns, qualName)
        }
        
        ## track the columns with data by bame
        dataColumns <- c(dataColumns, valueName)
      
        if("dateTime" %in% attributeNames){
          
          datetime <- xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS)
          
          numChar <- nchar(datetime)

          if(asDateTime){
            
            # Common options:
            # YYYY numChar=4
            # YYYY-MM-DD numChar=10
            # YYYY-MM-DDTHH:MM numChar=16
            # YYYY-MM-DDTHH:MM:SS numChar=19
            # YYYY-MM-DDTHH:MM:SSZ numChar=20
            # YYYY-MM-DDTHH:MM:SS.000 numChar=23
            # YYYY-MM-DDTHH:MM:SS.000-XX:00 numChar=29
                        
            if(abs(max(numChar) - min(numChar)) != 0){
              warning("Mixed date types, not converted to POSIXct")
            } else {
              numChar <- numChar[1]
              if(numChar == 4){
                datetime <- as.POSIXct(datetime, "%Y", tz = "UTC")
              } else if(numChar == 10){
                datetime <- as.POSIXct(datetime, "%Y-%m-%d", tz = "UTC")
              } else if(numChar == 16){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M", tz = "UTC")
              } else if(numChar == 19){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
              } else if(numChar == 20){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
              }  else if(numChar == 23){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
              } else if(numChar == 24){
                datetime <- substr(datetime,1,23)
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
                df$tz_cd <- rep(zoneAbbrievs[1], nrow(df))
              } else if(numChar == 29){
                tzOffset <- as.character(substr(datetime,24,numChar))
                
                tzHours <- as.numeric(substr(tzOffset,1,3))
  
                datetime <- substr(datetime,1,23)
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
                datetime <- datetime - tzHours*60*60
                df$tz_cd <- as.character(zoneAbbrievs[tzOffset]) 
              }
              
              if(!("tz_cd" %in% names(df))){
                df$tz_cd <- zoneAbbrievs[1]
                tzHours <- as.numeric(substr(names(zoneAbbrievs[1]),1,3))
                datetime <- datetime - tzHours*60*60
              }
            }
            
            if(tz != ""){
              attr(datetime, "tzone") <- tz
              df$tz_cd <- rep(tz, nrow(df))
            } else {
              attr(datetime, "tzone") <- "UTC"
              df$tz_cd <- rep("UTC", nrow(df))
            }
            
            
          } else {
            
            datetime <- as.character(datetime)
            if(any(numChar) == 29){
              tzOffset <- as.character(substr(datetime,24,numChar))
              df$tz_cd <- as.character(zoneAbbrievs[tzOffset]) 
              df$tz_cd[is.na(df$tz_cd)] <- zoneAbbrievs[1]
            } else {
              df$tz_cd <- zoneAbbrievs[1]
            }
            
          }
          
          df$dateTime <- datetime     
        }
        
        colNames <- names(df)
        
        if( exists("qualName")){
          columnsOrdered <- c("agency_cd","site_no","dateTime","tz_cd",attributeNames[attributeNames != "dateTime"],qualName,valueName)
        } else {
          columnsOrdered <- c("agency_cd","site_no","dateTime","tz_cd",attributeNames[attributeNames != "dateTime"],valueName)
        }
        
        columnsOrderd <- columnsOrdered[columnsOrdered %in% names(df)]
        
        
        
        df <- df[,columnsOrderd]
                        
        if (is.null(mergedDF)){
          mergedDF <- df          
        } else {
          similarNames <- intersect(names(mergedDF), names(df))
          mergedDF <- merge(mergedDF, df,by=similarNames,all=TRUE)
        }
        
      } else {
        if (1 == i & valuesIndex[1] == j){
          mergedDF <- NULL
        } 
      }

    }

    ######################
    names(extraSiteData) <- make.unique(names(extraSiteData))
    
    sitePropertyIndex <- grep("siteProperty",names(extraSiteData))
    
    siteInfo <- data.frame(station_nm=extraSiteData$siteName,
                           site_no=extraSiteData$siteCode$text,
                           agency_cd=extraSiteData$siteCode$.attrs[["agencyCode"]],
                           timeZoneOffset=extraSiteData$timeZoneInfo$defaultTimeZone[1],
                           timeZoneAbbreviation=extraSiteData$timeZoneInfo$defaultTimeZone[2],
                           dec_lat_va=as.numeric(extraSiteData$geoLocation$geogLocation$latitude),
                           dec_lon_va=as.numeric(extraSiteData$geoLocation$geogLocation$longitude),
                           srs=extraSiteData$geoLocation$geogLocation$.attrs[["srs"]],
                           stringsAsFactors=FALSE)
    
    properties <- as.character(lapply(extraSiteData[sitePropertyIndex], function(x) {
      if(".attrs" %in% names(x)){
        x$.attrs
      } else {
        NA
      }              
    }))
    
    propertyValues <- as.character(lapply(extraSiteData[sitePropertyIndex], function(x) {
      if("text" %in% names(x)){
        x$text
      } else {
        NA
      }              
    }))
    
    names(propertyValues) <- properties
    propertyValues <- propertyValues[propertyValues != "NA"]
    siteInfo <- cbind(siteInfo, t(propertyValues))            
    
    names(extraVariableData) <- make.unique(names(extraVariableData))
    
    variableInfo <- c(parameterCd=extraVariableData$variableCode$text,
                               parameter_nm=extraVariableData$variableName,
                               parameter_desc=extraVariableData$variableDescription,
                               valueType=extraVariableData$valueType,
                               param_units=extraVariableData$unit$unitCode)
    
    variableInfo <- data.frame(t(variableInfo), stringsAsFactors=FALSE)
    
    statInfo <- data.frame(statisticName=statName,
                           statisticCd=statCd,
                           stringsAsFactors=FALSE)

    if (1 == i){
      siteInformation <- siteInfo
      variableInformation <- variableInfo
      statInformation <- statInfo
      
    } else {
      similarSites <- intersect(names(siteInformation), names(siteInfo))
      siteInformation <- merge(siteInformation, siteInfo, by=similarSites, all=TRUE)
      
      similarVariables <- intersect(names(variableInformation),names(variableInfo))
      variableInformation <- merge(variableInformation, variableInfo, by=similarVariables, all=TRUE)
      
      similarStats <- intersect(names(statInformation), names(statInfo))
      statInformation <- merge(statInformation, statInfo, by=similarStats, all=TRUE)
    }

    ######################
    
    attList[[uniqueName]] <- list(extraSiteData, extraVariableData)

    
  }
  #timeTrack('after mega loop importWaterML1')
  
  if(!is.null(mergedDF)){
  
    dataColumns <- unique(dataColumns)
    qualColumns <- unique(qualColumns) 
    
    sortingColumns <- names(mergedDF)[!(names(mergedDF) %in% c(dataColumns,qualColumns))]
  
    meltedmergedDF  <- melt(mergedDF,id.vars=sortingColumns)
    meltedmergedDF  <- meltedmergedDF[!is.na(meltedmergedDF$value),] 
  
    castFormula <- as.formula(paste(paste(sortingColumns, collapse="+"),"variable",sep="~"))
    ## dcast is what causes the aggregation message 
    #timeTrack('before dcast importWaterML1')
    mergedDF2 <- dcast(meltedmergedDF, castFormula, drop=FALSE)
    #timeTrack('after dcast importWaterML1')
    dataColumns2 <- !(names(mergedDF2) %in% sortingColumns)
    
    #timeTrack('before subsetting NA importWaterML1')
    if(sum(dataColumns2) == 1){
      mergedDF <- mergedDF2[!is.na(mergedDF2[,dataColumns2]),]
    } else {
      mergedDF <- mergedDF2[rowSums(is.na(mergedDF2[,dataColumns2])) != sum(dataColumns2),]
    }
    #timeTrack('after subsetting NA importWaterML1')
    
    if(length(dataColumns) > 1){
      mergedDF[,dataColumns] <- lapply(mergedDF[,dataColumns], function(x) as.numeric(x))
    } else {
      mergedDF[,dataColumns] <- as.numeric(mergedDF[,dataColumns])
    }
    
    names(mergedDF) <- make.names(names(mergedDF))
  } else {
    mergedDF <- data.frame()
  }

  variableInformation$noDataValue <- rep(NA, nrow(variableInformation))
  
  row.names(mergedDF) <- NULL
  attr(mergedDF, "url") <- obs_url
  attr(mergedDF, "siteInfo") <- siteInformation
  attr(mergedDF, "variableInfo") <- variableInformation
  attr(mergedDF, "disclaimer") <- notes["disclaimer"]
  attr(mergedDF, "statisticInfo") <- statInformation
  # Do we want this?
  #   attr(mergedDF, "attributeList") <- attList
  #   attr(mergedDF, "queryInfo") <- queryInfo
  attr(mergedDF, "queryTime") <- Sys.time()
  #timeTrack('end importWaterML1')
  return (mergedDF)
}
