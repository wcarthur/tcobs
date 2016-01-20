#Title: Program 'tc_TC_wnds_stns.r'
#Author: Sanabria A., augusto.sanabria@ga.gov.au
#CreationDate: 2013-10-04
#Description: Program to extract cyclonic winds from BoM stations
#located in the cyclonic region of Australia.
#It reads a dataset of cyclonic tracks affecting a BoM recording station
#and extracts corresponding values from the station wind speeds.
#Dataset includes max wind speed of each cyclone coming closer than 600 km to
#the station.
#Output is written to external file using station ID as identifier.
#Keywords: BoM recording stations, BoM database of TC tracks, observed cyclonic winds

#Reference:
#SeeAlso:
#Version: 1.1
#Modified by: AS
#ModifiedDate: 2014-01-10
#Modification: The cyclonic wind dataset now includes only max wind speeds per cyclone.

#Required:
#sdir <<- "/home/asanabri/R/my_rsoftw/"
indir <- "C:/WorkSpace/tcobs/"
setwd(indir)
#source(paste(sdir,"get_TC_gusts.r",sep="")  )
#source(paste(sdir,"get_TC_gusts_per_cyclone.r",sep="")  )
source('get_TC_gusts_per_cyclone.r')
#Returns:
#void, produces external file.
#
t1 <- proc.time()
#Read database of stations affected by TCs:
outdir <- "N:/climate_change/CHARS/B_Wind/data/derived/obs/tc/ibtracs/"


#N.B. Change non-readable characters [ ] by " " (stn name should be read as a single string)

fname <- paste(indir, "stn_TC_dist.ibtracs.txt", sep = "")
bom_stns_wTC <- read.table(fname, sep = ",", skip = 1, header = F)

#Read BoM datasets (half-hour) located in Dir 'datadir'
datadir <- "N:/climate_change/CHARS/B_Wind/data/raw/obs/halfhourly/"


setwd(datadir)
all_bomd <- dir(pattern = "HM01X_Data")
#List stns not in bom data:
outf001 <- "stns_not_in_bomd.txt"
write.table("List of stations not in BoM-provided datasets",
            file = paste(outdir, outf001, sep = ""), append = FALSE)
write.table("stn_ID stn_name stn_ST stn_lat stn_lon",
            file = paste(outdir, outf001, sep = ""),
            append = TRUE)

#Transform BoM stn_ID in stn name into numeric:
bom_substr <- c()
for (i in 1:length(all_bomd)) {
  bom_substr[i] <- as.numeric(substring(all_bomd[i], 12, 17))
}
#Split the data into stations, extract wind gusts at the time of the TC track approaching the station and
#create a dataset of TC winds at that station:
grouped_data <- split(bom_stns_wTC, bom_stns_wTC$V2)
stn_char <- c()
cnt_stns <- 0
all_TC_gusts <- c()
for (i in 1:length(grouped_data)) {
  # extract wind gust from BoM dataset
  stn_id <- grouped_data[[i]][2]
  stn_ST <- unique(grouped_data[[i]][3])
  stn_name <- unique(grouped_data[[i]][1])
  cycl_name <- grouped_data[[i]][6]
  stn_lat <- grouped_data[[i]][4]
  stn_lon <- grouped_data[[i]][5]
  stn_char[i] <- paste(i, unique(stn_id),
                       stn_name$V1,stn_ST$V3,
                       " (",unique(stn_lat), unique(stn_lon),
                       ")", sep = ",")
  # open corresponding BoM dataset:
  bdatas <- which(as.numeric(unique(stn_id)) == as.numeric(bom_substr))
  # Test for errors (n.b. character(0) is numeric!!)
  ZZ <- try(ifelse(bdatas >= 1, bdatas, NA))
  # Write to external file list of stations not found in BoM-provided datasets:
  if (!is.numeric(ZZ)) {
    write.table(stn_char[i],
                file = paste(outdir, outf001, sep = ""),
                append = TRUE, row.names = FALSE,
                col.names = FALSE, quote = FALSE)
    next
  }
  # Write all stns found in BoM-provided datasets to R log file (to plot them in the OZ map):
  str1 <- paste(unique(stn_id), ", ", stn_name$V1, ",",
                unique(stn_lat), ",", unique(stn_lon),
                ",above, y" ,sep = "")
  print(str1)
  # TC_gusts <- get_TC_gusts(all_bomd[bdatas],unique(stn_ST),grouped_data[[i]][8],grouped_data[[i]][9])
  #print("Split data into cyclones in order to calc. max speed per cyclone")
  cycl_wsp <- cbind(grouped_data[[i]][8], grouped_data[[i]][9], grouped_data[[i]][6])
  cycl_mx_wsp <- split(cycl_wsp, cycl_wsp$V6, drop = TRUE)
  # loop tr' all cyclones:
  for (j in 1:length(cycl_mx_wsp)) {
    #print(paste("Extracting TC gusts for ",as.character(cycl_mx_wsp[[j]][3][1,1])))
    obs_gusts <- read.csv(all_bomd[bdatas], skip = 1, header = FALSE, strip.white = T)
    TC_gusts <- get_TC_gusts_per_cyclone(obs_gusts, unique(stn_ST),
                                         cycl_mx_wsp[[j]][1], cycl_mx_wsp[[j]][2],
                                         cycl_mx_wsp[[j]][3])

    if (all(is.na(TC_gusts))) next
    # Keep only max wind speed of this cyclone:
    #print("Filter maximum gusts")
    gusts = as.numeric(TC_gusts$V17)
    mx_gwsp <- which(gusts == max(gusts, na.rm = TRUE))
    if (length(mx_gwsp) > 1) {
      mx_gwsp = mx_gwsp[1]
    }
    #print("Appending maximum gust to list of all records")
    all_TC_gusts <- try(rbind(all_TC_gusts, TC_gusts[mx_gwsp,]))
    # all_TC_gusts <- try(rbind(all_TC_gusts,TC_gusts )  ) #write all wnd speeds not only max
  }
  # Write to external file winds in BoM-provided datasets which match date/time in cyclone track,
  # one dataset per station ID:
  cnt_stns <- cnt_stns + 1
  # write name with 6 digits (as in BoM station name convention):
  new_str <- formatC(unique(grouped_data[[i]]$V2), width = 6, flag = "0")
  out_main <- paste("bom_", new_str, ".csv",sep = "")
  print(paste("Writing data to: ",out_main))
  write.csv(all_TC_gusts,file = paste(outdir, out_main,sep = "") )
  all_TC_gusts <- c()
}

etime <- proc.time() - t1
print(paste("Stations in BoM-provided datasets hit by TC = ", cnt_stns, sep = "")  )
print(paste("Proc. time = ", etime, sep = ""))
#
print("Normal program termination")
