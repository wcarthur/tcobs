# function to extract gusts from a BoM dataset at a usr-provided time
# keeping track of the cyclone which orginated those gust winds.
# (called from program 'tc_TC_wnds_stns.r')
get_TC_gusts_per_cyclone = function(obs_gusts, ST, date, time, cyclone){
#bom_dset = name of BoM-provided wind dataset
#ST = state where dataset is located
#date = date at which TC hit the station
#time = corresponding time
#cyclone = Cyclone name
#open dataset:
#obs_gusts <- read.csv(bom_dset, skip = 1, header = FALSE)
#Read Local Standard Time (LST) in BoM dataset:

#build the LST datetime string:
ds_dattim1 <- obs_gusts$datetime #ISOdatetime(ds_yr, ds_m, ds_d, ds_hr, ds_min, sec = 0, tz = "GMT")
#Transform LST to UTC time (to match TC dataset):
if (ST == "WA") {
  ds_dattim <- ds_dattim1 - 8.*3600
}else if (ST == "NT") {
  ds_dattim <- ds_dattim1 - 9.5*3600
}else{
  ds_dattim <- ds_dattim1 - 10*3600
}

tcdt = strptime(date$V8, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

if (max(tcdt) < min(ds_dattim)) {
  #print(paste("No obs available for cyclone: ", as.character(cyclone[1,1]), ": ", max(tcdt)))
  return(NA)
}
obsidx = c()
for (i in 1:length(tcdt)) {
  dtm = tcdt[i] - 3600
  dtp = tcdt[i] + 5400
  idx = which((ds_dattim >= dtm) & (ds_dattim <= dtp))
  obsidx = append(obsidx, idx, after = length(obsidx))
}

#rint("Select only wind records at given times")
wnd_gusts <- obs_gusts[(obsidx),]
if (length(wnd_gusts$station) < 1) return(NA)
#Build up vector of name to match wnd_gusts:
NN <- cyclone[1,1]
cycl_name <- c()
cycl_name[1:length(wnd_gusts$station)] <- as.character(NN)
#Return cyclonic wind speeds (same format as BoM dataset) plus cyclone name in last column:
#print(paste("Return cyclonic wind speeds for ", as.character(NN)))

cbind(wnd_gusts, cycl_name)

}
