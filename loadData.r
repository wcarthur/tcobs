# A collection of functions to read formatted data files,
# generally obtained from the Bureau of Meteorology
# This includes station files, daily, half-hourly data files.

readDailyObs = function(obsfile, units = "m/s"){
  # Read a file that contains daily maximum wind gust data.
  # These files are described in DC02D_Notes_999999997960863.txt
  # See /nas/gemd/climate_change/CHARS/B_Wind/data/raw/obs/daily
  #
  # NOTES:
  # Units of wind speed vary between versions of this data. Some cases
  # have units of km/h, others m/s. The `units` argument indicates the
  # units of the raw data, and the data are converted to m/s by default.
  # Only handles conversion between m/s and km/h.

  colclasses = c("character", "integer", "integer", "integer", "integer", "double",
                 "character", "double", "character", "character", "character", "character")
  input <- read.csv(obsfile, skip = 1, header = FALSE,
                    strip.white = T, colClasses = colclasses)
  ds_yr <- input$V3
  ds_m <- input$V4
  ds_d <- input$V5
  ds_tm <- as.character(input$V10)
  ds_tm[ds_tm == ""] = "0000"
  dt = strptime(paste(ds_yr, ds_m, ds_d, ds_tm, sep = " "),
                format = "%Y %m %d %H%M", tz = "GMT")
  station = input$V2
  if (units == "km/h") {
    gust = input$V6 / 3.6
  }else{
    gust = input$V6
  }

  direction = input$V8
  quality = input$V7
  data <- data.frame(station = station, datetime = dt,
                     gust = gust, direction = direction,
                     quality = quality)
  cbind(data)
}

readHalfHourlyData = function(obsfile, units="m/s"){
  # Read a file that contains half-hourly observations.
  # These files are described in HM01X_Notes_999999997960860.txt
  # See /nas/gemd/climate_change/CHARS/B_Wind/data/raw/obs/halfhourly
  #
  # NOTES:
  # Units of wind speed vary between versions of this data. Some cases
  # have units of km/h, others m/s. The `units` argument indicates the
  # units of the raw data, and the data are converted to m/s by default.
  # Only handles conversion between m/s and km/h.
  input <- read.csv(obsfile, skip = 1, header = FALSE, strip.white = T)
  ds_yr <- input$V8
  ds_m <- input$V9
  ds_d <- input$V10
  ds_hr <- input$V11
  ds_min <- input$V12
  #build the LST datetime string:
  dt <- ISOdatetime(ds_yr, ds_m, ds_d, ds_hr, ds_min, sec = 0, tz = "GMT")
  station = input$V2
  if (units == "km/h") {
    gust = input$V17 / 3.6
  }else{
    gust = input$V17
  }

  direction = input$V15
  quality = input$V18
  data = data.frame(station = station, datetime = dt,
                    gust = gust, direction = direction,
                    quality = quality )

  cbind(data)
}

readStationFile = function(stationfile, header = F, skip = 1){
  # Read a formatted station file.
  input = read.csv(stationfile, sep = ",", header = header, skip = skip)
  stnId = input$V2
  stnName = input$V4
  stnLat = input$V7
  stnLon = input$V8
  stnState = input$V10
  stnOpenDate = strptime(paste("01", input$V5, sep = "/"), format = "%d/%m/%Y")
  stnCloseDate = strptime(paste("01", input$V6, sep = "/"), format = "%d/%m/%Y")
  stnDataStart = strptime(paste(input$V14, "01", "01", sep = "-"), format = "%Y-%m-%d")
  stnDataEnd = strptime(paste(input$V15, "01", "01", sep = "-"), format = "%Y-%m-%d")

  stations = data.frame(stnId, stnName, stnLat, stnLon,
                        stnState, stnOpenDate, stnCloseDate,
                        stnDataStart, stnDataEnd)
  cbind(stations)

}