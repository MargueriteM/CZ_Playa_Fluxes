# Red Lake Data Analysis I
# script author: Victoria Martinez and Marguerite Mauritz
# 05 August 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(zoo)

# get data from summaries folder
flux.files <- list.files(path="C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_DL_Collect/SmartFlux/summaries",full.names=TRUE)

# read the column number for each summary file
read_column_number <- function(colname){
  ret <- ncol(fread(colname, sep="\t", dec=".", header=TRUE, skip=0)[1,])
  obj_name <- tools::file_path_sans_ext(basename(colname))
  out <- data.frame(file=obj_name, colnumber=ret)
  out
}

# split into 2 because reading all at once is too big
data1 <- ldply(flux.files[1:100], read_column_number)

data2 <- ldply(flux.files[151:167], read_column_number)

# issue with files between 167-174 (zero KB files)
data3 <- ldply(flux.files[174:314], read_column_number)

data4 <- ldply(flux.files[314:500], read_column_number)

data5 <- ldply(flux.files[500:727], read_column_number)

data6 <- ldply(flux.files[727:854], read_column_number)

# read the flux files as csv and combine into single dataframe

data <- rbind(data1, data2, data3, data4, data5, data6)

# general column number is 211, select files
flux.files.read <- data %>%
  filter(colnumber==211) %>%
  mutate(file.path = paste("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_DL_Collect/SmartFlux/summaries/",
                           file,".txt",sep=''))

# get column names and units from complete summary files
flux.units <- fread(flux.files.read$file.path[1], sep="\t", dec=".", header=TRUE, skip=0)[1,]

# get data from complete summary files
flux.data <- do.call("rbind", lapply(flux.files.read$file.path, header = FALSE, fread, sep="\t", dec=".",skip = 2, fill=TRUE, na.strings="NaN", col.names=colnames(flux.units)))


# MM edit 9 April: 
# Steps to add:
# initial graphs of CO2 flux, LE, H, Co2 signal strength with color by qc code
# case_when to make the CO2 flux, LE, H ~NA for qc code <2

#eg: (creating test object just to make sure code works... when it does, can replace test with flux.data)
test <- flux.data %>%
  mutate(co2_flux = case_when(qc_co2_flux>2 ~ NA, 
                              .default = co2_flux),
         LE = case_when(qc_LE>2 ~NA,
                        .default=LE),
         H = case_when(qc_H>2 ~NA,
                        .default=H))

# case_when for unlikely ranges of CO2, LE, H ~ NA 
# case_when for when P_Rain_1_1_1>1 for CO2, LE, H ~ NA 



# APPLY ROLLING MEANS APPROACH TO FILTER co2, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux.data[,':=' (co2F_rollmean3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              co2F_rollmean5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              co2F_rollmean7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux.data[, ':=' (co2F_rollmean3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=daytime]


threshold <- 3

flux.filter <-flux.data%>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")))

# graph the 3 and 5 day SD ribbons around measured flux_add***
ggplot(flux.filter)+
  geom_line(aes(date_time, ))+
  #geom_line(aes(date_time, co2F_rollmean), colour="green")+
  geom_ribbon(aes(x=date_time, ymin=co2F_rollmean3-threshold*co2F_rollsd3, ymax=co2F_rollmean3+threshold*co2F_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=date_time, ymin=co2F_rollmean7-threshold*co2F_rollsd7, ymax=co2F_rollmean7+threshold*co2F_rollsd7), colour="blue",alpha=0.3)+
  facet_grid(daytime~.)

# graph the 3 day SD ribbons by day/night
ggplot(flux.filter)+
  geom_line(aes(date_time, co2_flux))+
  #geom_line(aes(date_time, co2F_rollmean), colour="green")+
  geom_ribbon(aes(x=date_time, ymin=co2F_rollmean3_daynight-threshold*co2F_rollsd3_daynight,
                  ymax=co2F_rollmean3_daynight+threshold*co2F_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

# mark any co2_flux >3* co2Frollsd3 (3 day moving SD) for removal
# For Jan 2020 added 2 weeks of data prior from 2019 so the running mean can carry over. 
flux_add[,filter_fc_roll := 0L]
flux_add[filter_fc==1, filter_fc_roll := 1L]

flux_add[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]


# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux.filter[,filter_co2_roll_daynight := 0L]
flux.filter[, filter_co2_roll_daynight := 1L]
flux_add[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
           FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]

# view the marked fluxes in ~25 day chunks---
ggplot(flux.filter, aes(date_time, co2_flux, colour=factor(filter_fc_roll)))+
  geom_point()

# view the marked fluxes in ~25 day chunks for day/night---
ggplot(flux_add[filter_fc_roll_daynight!=1&DOY_START>=1&DOY_START<=31,], aes(DOY_START, FC, colour=factor(filter_fc_roll_daynight)))+
  geom_point()

# graph with the fluxes from the 3SD 3day day/night filter removed---
ggplot(flux_add[filter_fc_roll_daynight==0,], aes(DOY_START, FC))+
  geom_line()

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
# HERE THIS DOESN'T REALLY APPLY, I HAVE DONE LESS MANUAL FILTERING AND AM RELYING ON THE RUNNING MEAN SMOOTH
# this comparison was for previous years. It's nice to have the code handy, if needed
ggplot()+
  geom_line(data=flux_add[filter_fc==0], aes(DOY_START, FC, colour="manual"), colour="blue")+
  geom_line(data=flux_add[filter_fc_roll_daynight==0], aes(DOY_START, FC, colour="SD day/night"), colour="green")


# SD filter for LE
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux_add[filter_LE !=1, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux_add[filter_LE !=1, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(flux_add[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux_add[,filter_le_roll := 0L]
flux_add[filter_LE==1, filter_le_roll := 1L]
flux_add[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux_add[,filter_le_roll_daynight := 0L]
flux_add[filter_LE==1, filter_le_roll_daynight := 1L]
flux_add[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
           LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]


# view the marked flux_addes in ~25 day chunks
ggplot(flux_add[filter_le_roll!=1&DOY_START>=100&DOY_START<=300,], aes(DOY_START, LE, colour=factor(filter_le_roll)))+
  geom_point()

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_le_roll_daynight!=1&DOY_START>=100&DOY_START<=300,], aes(DOY_START, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()

# graph with the flux_addes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_le_roll_daynight==0,], aes(DOY_START, LE))+
  geom_line()

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
# HERE THIS DOESN'T REALLY APPLY, I HAVE DONE LESS MANUAL FILTERING AND AM RELYING ON THE RUNNING MEAN SMOOTH
# this comparison was for previous years. It's nice to have the code handy, if needed
ggplot()+
  geom_line(data=flux_add[filter_LE==0], aes(DOY_START, LE, colour="manual"), colour="blue")+
  geom_line(data=flux_add[filter_le_roll_daynight==0], aes(DOY_START, LE, colour="SD day/night"), colour="green")

# SD filter for H
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux_add[filter_H !=1, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                             H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                             H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux_add[filter_H !=1, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(flux_add[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)


# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux_add[,filter_h_roll := 0L]
flux_add[filter_H==1, filter_h_roll := 1L]
flux_add[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]

# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux_add[,filter_h_roll_daynight := 0L]
flux_add[filter_H==1, filter_h_roll_daynight := 1L]
flux_add[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
           H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]


# view the marked flux_addes in ~25 day chunks
ggplot(flux_add[filter_h_roll!=1&DOY_START>=1&DOY_START<=50,], aes(DOY_START, H, colour=factor(filter_h_roll)))+
  geom_point()

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_h_roll_daynight!=1&DOY_START>=1&DOY_START<=100,], aes(DOY_START, H, colour=factor(filter_h_roll_daynight)))+
  geom_point()

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_h_roll_daynight==0,], aes(DOY_START, H))+
  geom_line()

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
# HERE THIS DOESN'T REALLY APPLY, I HAVE DONE LESS MANUAL FILTERING AND AM RELYING ON THE RUNNING MEAN SMOOTH
# this comparison was for previous years. It's nice to have the code handy, if needed
ggplot()+
  geom_line(data=flux_add[filter_H==0], aes(DOY_START, H, colour="manual"), colour="blue")+
  geom_line(data=flux_add[filter_h_roll_daynight==0], aes(DOY_START, H, colour="SD day/night"), colour="green")



