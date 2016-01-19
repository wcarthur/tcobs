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
tc_fname <- "Allstorms.ibtracs_wmo.v03r06.csv"
TC_datab <- read.csv(tc_fname, skip = 1, header = T)
OZ_TC_datab <- with(TC_datab,data.frame(name = Name, UTC_time = ISO_time,
                                        lon = Longitude, lat = Latitude,
                                        radius = Pressure))
#Read list of BoM stns and location:
#stns_list <- "TC_region_stn_list.csv"
stns_list <- "BoM_stns_short.txt"
BoM_stn_list <- read.csv(stns_list, header = T)
OZ_TCregion_stns <- with(BoM_stn_list, data.frame(stn_ID = Site, stn = Sitename,
                                                  state = State, lat = Lat, lon = Lon,
                                                  start = Start, endt = End) )
#Write all stations which are closer than 'mx_d' km to a TC track and are open at the time of the TC pass
#Write also stations not included in the two rules listed above with the reason (as a check)
t1 <- proc.time()
#mx_d <- 200.0
#mx_d <- 400.0
mx_d <- 600.0
keep_name <- c()
outf <- "stn_TC_dist.ibtracs.txt"
#outf00 <- "No_TC_hit_stn.ibtracs.txt"
write.table("Database of BoM stations affected by TC", file = outf, append = FALSE)
#write.table("Database of BoM stations NOT affected by TC",file=outf00,append=FALSE)
write.table("Stn name BoM_ID State  Stn location TC_name  dist (km)   date passed  TC Radius (km)", file = outf, append = TRUE)
for (i in 1:length(OZ_TCregion_stns$stn)) {
  stn_ID <- OZ_TCregion_stns[i, 1]
  stn_name <- OZ_TCregion_stns[i, 2]
  stn_state <- OZ_TCregion_stns[i, 3]
  print(stn_name)
  stn_lat <- OZ_TCregion_stns[i, 4]*(pi/180.0)
  stn_lon <- OZ_TCregion_stns[i, 5]*(pi/180.0)
  stn_start <- as.integer(substring(OZ_TCregion_stns[i,6],4,7))
  if ((substring(OZ_TCregion_stns[i,7],4,7) == "    ") | (is.na(OZ_TCregion_stns[i,7]))) {
    stn_close = 5000
  }else{
    stn_close = as.integer(substring(OZ_TCregion_stns[i, 7], 4, 7))
  }
  #stn_close <- ifelse(as.integer(substring(OZ_TCregion_stns[i,7],3,7)) <= 0,5000,OZ_TCregion_stns[i,7] )
  for (k in 1:length(OZ_TC_datab$name)) {
    lat2 <- OZ_TC_datab[k, 4]*(pi/180.0)
    lon2 <- OZ_TC_datab[k, 3]*(pi/180.0)
    if (is.na(lat2) | is.na(lon2) ) {
      print("**Warning: empty line (ignored)")
      print(paste("TC_name(previous) = ", OZ_TC_datab[k - 1, 1], sep = ""))
      next
    }
    #print(OZ_TC_datab[k,1])
    dd <- 2*asin(sqrt((sin((stn_lat - lat2)/2)) ^ 2 +
                      cos(stn_lat)*cos(lat2)*(sin((stn_lon - lon2)/2)) ^ 2))
    dist_2stn <- 1.852*(180*60/pi)*dd
    #print(paste("dist = ",dist_2stn,sep="") )
    if (dist_2stn <= mx_d) {
      keep_name <- as.character(OZ_TC_datab[k, 1])
      TC_yy <- as.integer(substring(OZ_TC_datab[k, 2], 1, 4))
      TC_mm <- as.integer(substring(OZ_TC_datab[k, 2], 6, 7))
      if (TC_yy >= stn_start & TC_yy <= stn_close) {
        #Station is close enough, record station, distance and time of hit:
        write.table(paste(as.character(stn_name),
                          stn_ID, stn_state,
                          signif((180.0/pi)*stn_lat, 8),
                          signif((180.0/pi)*stn_lon, 8),
                          keep_name,round(dist_2stn, 1),
                          OZ_TC_datab[k, 2], OZ_TC_datab[k, 5], sep = ","),
                    file = outf, append = TRUE, row.names = FALSE,
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

