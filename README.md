`dataRetrieval`
=============
Linux: [![travis](https://travis-ci.org/USGS-R/dataRetrieval.svg?branch=master)](https://travis-ci.org/USGS-R/dataRetrieval)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/msanha92b500grr7?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/dataretrieval-787)


Retrieval functions for USGS and EPA hydrologic and water quality data.

A fairly large overhaul of the functions was done on November 31, 2014. Feedback from initial users was incorporated. For questions, bug reports, and suggestions please create an Issue here:

[dataRetrieval Issues page](https://github.com/USGS-R/dataRetrieval/issues)


## Function Overview

Web service retrieval functions:

|Function | Inputs | Description | 
| -------------| ------------------------|:-------------|
|`readNWISdata` | `...`, `service` | NWIS data using user-specified queries |
|`readNWISdv` | `Common 3`, `parameterCd`, `statCd` | NWIS daily data with `Common` query |
|`readNWISqw` | `Common 3`, `parameterCd`, `expanded` | NWIS water quality data with `Common` query |
|`readNWISuv` | `Common 3`, `parameterCd` | NWIS instantaneous data with `Common` query |
|`readNWISpCode` | `parameterCd` | NWIS parameter code information |
|`readNWISgwl` | `Common 3` | NWIS groundwater level data with `Common` query |
|`readNWISpeak` | `Common 3` | NWIS peak flow data with `Common` query |
|`readNWISmeas` | `Common 3` | NWIS surface-water measurement data with `Common` query |
|`readNWISrating` | `siteNumber`, `type` | NWIS rating table for an active USGS streamgage |
|`readNWISsite` | `siteNumber` | NWIS site information |
|`whatNWISsites` | `...` | NWIS site search using user-specified queries |
|`whatNWISdata` | `siteNumber`, `service` | NWIS data availability, including period of record and count |
|`readWQPdata` | `...` | WQP data using user-specified queries |
|`readWQPqw` | `Common 3` | WQP data with `Common 3` query and either parameter code or characteristic name|
|`whatWQPsites` | `...` | WQP site search using user-specified queries |

* `Common 3` = siteNumber, startDate, endDate

##Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:

[https://github.com/USGS-R/dataRetrieval/issues](https://github.com/USGS-R/dataRetrieval/issues)


##Subscribe
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.



##Package Installation
To install the dataRetrieval package, you must be using R 3.0 or greater and run the following command:
```R
	install.packages("dataRetrieval")
```

##Version updates
---------------
###dataRetrieval 2.1.0
* Improved error handling
* Moved parameterCdFile and pcodeToName data to sysdata.rda to assure functions using that data are available in other packages.
* Fixed WaterML2 qualifier bug.



###dataRetrieval 2.0.1
* Improved help files and vignette documentation
* Added siteInfo and variableInfo attributes to returned data frames
* Removed any obsolete functions


###dataRetrieval 2.0.0

* Changing naming convention. Migrated `EGRET` specific retrievals to `EGRET`:

Moving `EGRET` specific functions to `EGRET` (version 2.0.0 and greater):


|Information Source | Meta Data | Data |
| -------------| -------------| ------------- |:-------------|
|NWIS | `readNWISInfo` | `readNWISSample` |
|  |  | `readNWISDaily` |
| Water Quality Portal  | `readWQPInfo`| `readWQPSample` |
| User-supplied files | `readUserInfo` | `readUserDaily`|
| | | `readUserSample` |

* Added back WaterML2 parsing tool
* Added specific groundwater, rating, peak, and surfacewater measurement functions
* Attached metadata attributes to returned dataframes


###dataRetrieval 1.4.0
* Changed naming convention:



###dataRetrieval 1.3.3

* Updated getNWISSiteInfo to retrieve multiple site file datasets at once using a vector of siteNumbers as input argument.
* Updated error-handling for Web service calls. More information is returned when errors happen
* Added some basic processing to Water Quality Portal raw data retrievals. Date columns are returned as Date objects, value columns are numeric, and a column is created from the date/time/timezone columns that is POSIXct.
* Added very generalized NWIS and WQP retrieval functions (getNWISData, getNWISSites, getGeneralWQPData, and whatWQPsites) which allow the user to use any argument available on the Web service platform.


###dataRetrieval 1.3.2

* Deprecated getQWData, updated readWQPdata to take either parameter code or characteristic name.
* Changed the name of raw data retrievals to: readNWISqw, getNWISunitData, getNWISdvData, and getWQPqwData (from: readNWISqw, retrieveUnitNWISData, retrieveNWISData, getRawQWData)
* Added NA warning to getDVData function
* Updated mergeReport to allow for Sample data with different measurements taken on the same day



##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
