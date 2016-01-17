#function to extract gusts from a BoM dataset at a usr-provided time
#keeping track of the cyclone which orginated those gust winds.
#(called from program 'tc_TC_wnds_stns.r')
get_TC_gusts_per_cyclone = function(bom_dset,ST,date,time,cyclone){
#bom_dset = name of BoM-provided wind dataset
#ST = state where dataset is located
#date = date at which TC hit the station
#time = corresponding time
#cyclone = Cyclone name
#open dataset:
obs_gusts <- read.csv(bom_dset,skip=1,head=FALSE)
#Read Local Standard Time (LST) in BoM dataset:
ds_yr <- obs_gusts$V8
ds_m <- obs_gusts$V9
ds_d <- obs_gusts$V10
ds_hr <- obs_gusts$V11
ds_min <- obs_gusts$V12
#build the LST datetime string:
ds_dattim1 <- ISOdatetime(ds_yr,ds_m,ds_d,ds_hr,ds_min,sec=0,tz="GMT")
#Transform LST to UTC time (to match TC dataset):
if(ST == "WA"){ds_dattim <- ds_dattim1 - 8.*3600
}else if(ST == "NT"){ds_dattim <- ds_dattim1 - 9.5*3600
}else{ds_dattim <- ds_dattim1 - 10*3600
}
#Format it as '1992-10-28':
ds_dvec <-  format(ds_dattim,"%Y-%m-%d:%H:%M")
#Format usr_provided time (extracted from database of TC track):
usr_yr <- substring(date$V8,7,10)
usr_m <- substring(date$V8,4,5)
usr_d <- substring(date$V8,1,2)
usr_xx <- substring(time$V9,1,2)
usr_hr <- c()
usr_min <- c()
for(i in 1:length(usr_xx)){
   if(substring(usr_xx[i],2,2) == ":"){usr_hr[i] <- paste("0",substring(usr_xx[i],1,1),sep="")
                                 usr_min[i] <- substring(time$V9[i],3,4)
   }else{
       usr_hr[i] <- usr_xx[i]
       usr_min[i] <- substring(time$V9[i],4,5)
   }
}
#TC track comes in 3-hr intervals. For each track time select also times in BoM datasets +- 30min, +- 60min + 90min
usr_dattim1 <- ISOdatetime(usr_yr,usr_m,usr_d,usr_hr,usr_min,sec=0,tz="GMT")
#Append time to final vector:
usr_dattim <- usr_dattim1
usr_dattim <- append(usr_dattim,usr_dattim1+30*3600,after=length(usr_dattim)  )
usr_dattim <- append(usr_dattim,usr_dattim1-30*3600,after=length(usr_dattim)  )
usr_dattim <- append(usr_dattim,usr_dattim1+60*3600,after=length(usr_dattim)  )
usr_dattim <- append(usr_dattim,usr_dattim1-60*3600,after=length(usr_dattim)  )
usr_dattim <- append(usr_dattim,usr_dattim1+90*3600,after=length(usr_dattim)  )
#
usr_dvec <-  format(usr_dattim,"%Y-%m-%d:%H:%M")
##
#Select only wind records at given times:
wnd_gusts <- obs_gusts[which(ds_dvec %in% usr_dvec),]
if(length(wnd_gusts$V1) < 1)return(NA)
#Build up vector of name to match wnd_gusts:
NN <- cyclone[1,1]
cycl_name <- c()
cycl_name[1:length(wnd_gusts$V1)] <- as.character(NN)
#Return cyclonic wind speeds (same format as BoM dataset) plus cyclone name in last column:
cbind(wnd_gusts,cycl_name)


#
}
