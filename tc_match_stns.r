#Title: Program 'tc_match_stns.r'
#Author: Sanabria A., augusto.sanabria@ga.gov.au
#CreationDate: 2013-09-19
#Description: Program to match BoM stations with TC tracks.
#The station characteristics are read from BoM station database
#TC tracks are also provided by BoM.
#This script compares station location with TC tracks, if the TC track comes
#closer than 'mx_d' km to the station and the station was in operation at the time,
#write the station name and location; TC distance to the station, date and TC radius
#(MN_RADIUS_SF_WND) into an external file ("stn_TC_dist.txt").

#Keywords: BoM recording stations, BoM database of TC tracks

#Reference:
#SeeAlso:
#Version: 1.0
#Modified by:
#ModifiedDate:
#Modification:

#Required:
#NIL
#Returns:
#void, produces external file.
#
#
#Read TC tracks:
#tc_fname <- "IDCKMSTM0S.csv"
#TC_BoM_datab <- read.csv(tc_fname,skip=3,head=T)
# Extract fields of interest:
#OZ_TC_datab <- with(TC_BoM_datab,data.frame(name=NAME,UTC_time=TM,lon=LON,lat=LAT,radius=MN_RADIUS_GF_WIND)  )
source('loadData.r')
tc_fname <- "Allstorms.ibtracs_wmo.v03r06.csv"
TC_datab <- read.csv(tc_fname, skip = 1, header = T)
OZ_TC_datab <- with(TC_datab,data.frame(name = Name, UTC_time = ISO_time,
                                        lon = Longitude, lat = Latitude,
                                        radius = Pressure))

#Read list of BoM stns and location:

datadir <- "N:/climate_change/CHARS/B_Wind/data/raw/obs/daily/2006/"
stnFile <- paste(datadir, "DC02D_StnDet_99999999720437.txt", sep = "")
stations <- readStationFile(stnFile, header = F, skip = 1)

#Write all stations which are closer than 'mx_d' km to a TC track and are open at the time of the TC pass
#Write also stations not included in the two rules listed above with the reason (as a check)
t1 <- proc.time()
#mx_d <- 200.0
#mx_d <- 400.0
mx_d <- 600.0
keep_name <- c()
outdir <- "N:/climate_change/CHARS/B_Wind/data/derived/obs/tc/daily/2006/"
outputFile <- paste(outdir, "stn_TC_dist.allstns.txt", sep = "")
write.table("Database of BoM stations affected by TC", file = outputFile, append = FALSE)
write.table("Stn name BoM_ID State  Stn location TC_name  dist (km)   date passed  TC Radius (km)", file = outputFile, append = TRUE)

for (i in 1:length(stations$stnId)) {
  stnID <- stations$stnId[i]
  stn_name <- stations$stnName[i]
  stn_state <- stations$stnState[i]
  print(stn_name, max.levels = 0)
  stn_lat <- stations$stnLat[i]*(pi/180.0)
  stn_lon <- stations$stnLon[i]*(pi/180.0)

  stn_start <- stations$stnOpenDate[i]
  if (is.na(stations$stnCloseDate[i])) {
    stn_close = strptime("2100-01-01", format = "%Y-%m-%d", tz = "GMT")
  }else{
    stn_close = stations$stnCloseDate[i]
  }

  for (k in 1:length(OZ_TC_datab$name)) {
    lat2 <- OZ_TC_datab[k, 4]*(pi/180.0)
    lon2 <- OZ_TC_datab[k, 3]*(pi/180.0)
    if (is.na(lat2) | is.na(lon2) ) {
      print("**Warning: empty line (ignored)")
      print(paste("TC_name(previous) = ", OZ_TC_datab[k - 1, 1], sep = ""))
      next
    }

    dd <- 2*asin(sqrt((sin((stn_lat - lat2)/2)) ^ 2 +
                      cos(stn_lat)*cos(lat2)*(sin((stn_lon - lon2)/2)) ^ 2))
    dist_2stn <- 1.852*(180*60/pi)*dd
    #print(paste("dist = ",dist_2stn,sep="") )
    if (dist_2stn <= mx_d) {
      keep_name <- as.character(OZ_TC_datab[k, 1])
      TC_date = strptime(OZ_TC_datab[k, 2], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      if (TC_date >= stn_start & TC_date <= stn_close) {
        #Station is close enough, record station, distance and time of hit:
        write.table(paste(as.character(stn_name),
                          stnID, stn_state,
                          signif((180.0/pi)*stn_lat, 8),
                          signif((180.0/pi)*stn_lon, 8),
                          keep_name,round(dist_2stn, 1),
                          OZ_TC_datab[k, 2], OZ_TC_datab[k, 5], sep = ","),
                    file = outputFile, append = TRUE, row.names = FALSE,
                    col.names = FALSE, quote = FALSE)
      }else{
       #Station was NOT open at time of TC hit:
       #write.table(paste(as.character(stn_name)," (",signif((180.0/pi)*stn_lat,8),",",signif((180.0/pi)*stn_lon,8),")  ",keep_name,"  ",round(dist_2stn,1),"  ",OZ_TC_datab[k,2],"  No time match!",sep=""),
       #file=outf00,append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
      }
    }else{
      #Station is farther than 'mx_d' km, write it in to 'No_TC_hit' file:
      #write.table(paste(stn_name," (",signif((180.0/pi)*stn_lat,8),",",signif((180.0/pi)*stn_lon,8),")  ",keep_name,"  ",round(dist_2stn,1),"  ",OZ_TC_datab[k,2],"  Too far!",sep=""),
      #file=outf00,append=TRUE,row.names=FALSE,col.names=FALSE)
    }
  }
}
etime <- proc.time() - t1
print(paste("Proc. time = ", etime, sep = ""))
print("Normal program termination")

