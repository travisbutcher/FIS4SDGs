library(jsonlite)
library(tidyr)
library(data.table)

setwd("C:/Users/L.GonzalezMorales/OneDrive - United Nations/Federeted information systems for SDGs/Worksho 4-5 April 2018/")



#-----------------------------------------------------------------------------
# List of countreis to be plotted on a map (with XY coordinates)
#------------------------------------------- ----------------------------------

countryListXY <- as.data.frame(read.table("CountryListXY.txt", 
                                          header = TRUE, 
                                          sep = "\t",
                                          quote = "",
                                          na.strings = "", 
                                          stringsAsFactors = FALSE,
                                          encoding = "UTF-8"))

countryListXY[countryListXY$geoAreaCode==248,"geoAreaName"] <- "Åland Islands"
countryListXY[countryListXY$geoAreaCode==384,"geoAreaName"] <- "Côte d'Ivoire"
countryListXY[countryListXY$geoAreaCode==531,"geoAreaName"] <- "Curaçao"
countryListXY[countryListXY$geoAreaCode==638,"geoAreaName"] <- "Réunion"
countryListXY[countryListXY$geoAreaCode==652,"geoAreaName"] <- "Saint Barthélemy"

#------------------------------------------------------------------------------
# List of al series available 
#------------------------------------------------------------------------------

SeriesList  <- as.data.table(fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Series/List?allreleases=false"))[,c("release","code","description")]

#------------------------------------------------------------------------------
# Pull data for each series
#------------------------------------------------------------------------------

for(i in 1:length(SeriesList$code))
{
    
    
    queryString <- paste("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=",SeriesList[i][[2]],"&pageSize=5000",sep="")
    
    #queryString <- paste("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=","AG_FPA_MILLET","&pageSize=5000",sep="")

    x <- fromJSON(queryString )

    
    # Create grid of key columns
    geoAreaCodes <- countryListXY$geoAreaCode
    
    slice <- unique(subset(x$data$dimensions, select=-c(Nature)))
    slice$sliceId <- 1:nrow(slice)
    
    years <- unique(x$data$timePeriodStart)
    
    grid <- expand.grid(geoAreaCode = geoAreaCodes, 
                        sliceId = slice$sliceId, 
                        years = years)
    
    # Extract data matrix:
    data <- x$data[,c("geoAreaCode",
                      "timePeriodStart",
                      "value",
                      "valueType",
                      "time_detail",
                      "source")]
    
    colnames(data)[colnames(data)=="timePeriodStart"] <- "years"
    
    data <- cbind(data, x$data$dimensions)
    
    # Create data cube by left-joining grid and data matrix:
    cube <- merge(merge(merge(grid,
                              countryListXY,
                              all.x = TRUE),
                        slice,
                        all.x = TRUE),
                  data,
                  all.x = TRUE)
    
    cube <- cube[order(cube$geoAreaCode, cube$sliceId, cube$years),]
    
    
    cube <- as.data.table(cube)
    
    names(cube)
    
    # Create data cube for average of most recent 5-year period
    
    latest.year <- max(cube$years)
    
    cube.last5years.mean <- merge(unique(subset(cube, select=-c(years, value, valueType, time_detail, source, Nature))),
                                  cube[years %in% (latest.year-4):latest.year,
                                       list(years = paste(latest.year-4, latest.year, sep="-"),
                                            value = mean(as.numeric(value))),
                                       by = c("geoAreaCode", "sliceId")],
                                  all.x = TRUE)
    
    
    cube.last5years.mean <- cube.last5years.mean[order(cube.last5years.mean$geoAreaCode, 
                                                       cube.last5years.mean$sliceId, 
                                                       cube.last5years.mean$years),]
    
    
    
    # Create data cube for latest year available
    # https://stackoverflow.com/questions/24558328/how-to-select-the-row-with-the-maximum-value-in-each-group
    
    cube.latest <- cube[cube[, .I[years == max(years)], by =  c("geoAreaCode", "sliceId")]$V1]
    

    # PIVOT: x <- x  %>% spread(year,value)
    

    write.table(cube, 
                file = paste(SeriesList[i][[2]],"_cube.csv", sep=""), 
                 append = FALSE,
                 quote = FALSE, 
                 sep = "\t",
                 eol = "\n", 
                 na = "", 
                 dec = ".", 
                 row.names = FALSE,
                 col.names = TRUE, 
                 fileEncoding = "UTF-8")
    
    write.table(cube.last5years.mean, 
                file = paste(SeriesList[i][[2]],"_cube_last5.csv", sep=""), 
                append = FALSE,
                quote = FALSE, 
                sep = "\t",
                eol = "\n", 
                na = "", 
                dec = ".", 
                row.names = FALSE,
                col.names = TRUE, 
                fileEncoding = "UTF-8")
    
    write.table(cube.latest, 
                file = paste(SeriesList[i][[2]],"_cube_latest.csv", sep=""), 
                append = FALSE,
                quote = FALSE, 
                sep = "\t",
                eol = "\n", 
                na = "", 
                dec = ".", 
                row.names = FALSE,
                col.names = TRUE, 
                fileEncoding = "UTF-8")
    


}
