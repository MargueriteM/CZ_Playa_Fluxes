# Red Lake Data Analysis I
# script author: Victoria Martinez and Marguerite Mauritz
# 05 August 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(zoo)
library(plyr)

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

data6 <- ldply(flux.files[727:861], read_column_number)

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

flux.data <-flux.data%>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")))

# MM edit 9 April: 
# Steps to add:

# initial graphs of CO2 flux, LE, H, Co2 signal strength with color by qc code
ggplot(flux.data, aes(date_time, co2_flux, color = factor(qc_co2_flux)))+
  geom_point()

ggplot(flux.data, aes(date_time, LE, color = factor(qc_LE)))+
  geom_point()

ggplot(flux.data, aes(date_time, H, color = factor(qc_H)))+
  geom_point()

ggplot(flux.data, aes(date_time, co2_signal_strength_7500_mean))+
  geom_point()

# case_when to make the CO2 flux, LE, H ~NA for qc code <2

#eg: (creating test object just to make sure code works... when it does, can replace test with flux.data)
# co2 flux by qc code
flux.filter <- flux.data %>%
  mutate(co2_flux = case_when(qc_co2_flux>1 ~ NA, 
                              .default = co2_flux),
         LE = case_when(qc_LE>1 ~NA,
                        .default=LE),
         H = case_when(qc_H>1 ~NA,
                        .default=H))


# case_when for unlikely ranges of CO2, LE, H ~ NA 
flux.filter <- flux.filter %>%
  mutate(co2_flux = case_when(co2_flux<(-30)| co2_flux>30 ~ NA, 
                              .default = co2_flux),
         LE = case_when(LE<(-50)|LE>1000 ~NA,
                        .default=LE),
         H = case_when(H<(-120)|H>560 ~NA,
                       .default=H))


# case_when for when P_Rain_1_1_1>1 for CO2, LE, H ~ NA 
flux.filter <- flux.filter %>%
  mutate(co2_flux = case_when(P_RAIN_1_1_1>0 ~ NA, 
                              .default = co2_flux),
         LE = case_when(P_RAIN_1_1_1>0 ~NA,
                        .default=LE),
         H = case_when(P_RAIN_1_1_1>0 ~NA,
                       .default=H))
         

ggplot(flux.filter, aes(date_time, co2_flux, color = factor(qc_co2_flux)))+
  geom_point()

ggplot(flux.filter, aes(date_time, LE, color = factor(qc_LE)))+
  geom_point()

ggplot(flux.filter, aes(date_time, H, color = factor(qc_H)))+
  geom_point()

# APPLY ROLLING MEANS APPROACH TO FILTER co2, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside range
flux.filter[,':=' (co2F_rollmean3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              co2F_rollmean5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              co2F_rollmean7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux.filter[, ':=' (co2F_rollmean3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              co2F_rollsd3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=daytime]


threshold <- 3

# graph the 3 and 7 day SD ribbons around measured flux.data***
ggplot(flux.filter)+
  geom_line(aes(date_time, co2_flux))+
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


#calculate mean and standard dev for first 3 days fr daytime/nighttime
flux.filter[1:(((24/0.5)*3-1)*2.5), ':=' (co2F_rollmean3_daynight = mean(co2_flux, na.rm=TRUE),
                                    co2F_rollsd3_daynight = sd(co2_flux, na.rm=TRUE)),
            by=daytime]


# mark any co2_flux >3* co2Frollsd3 (3 day moving SD) for removal
# For Jan 2020 added 2 weeks of data prior from 2019 so the running mean can carry over. 
flux.filter[,filter_co2F_roll := 0L]
flux.filter[co2_flux>co2F_rollmean3+threshold*co2F_rollsd3|co2_flux<co2F_rollmean3-threshold*co2F_rollsd3, filter_co2F_roll := 2L]


# by Day/Night mark any co2_flux >3*co2Frollsd3 (3 day moving SD) for removal
flux.filter[,filter_co2F_roll_daynight := 0L]
flux.filter[co2_flux>co2F_rollmean3_daynight+threshold*co2F_rollsd3_daynight|
           co2_flux<co2F_rollmean3_daynight-threshold*co2F_rollsd3_daynight, filter_co2F_roll_daynight := 2L]


# view the marked fluxes for deletion---
ggplot(flux.filter, aes(date_time, co2_flux, colour=factor(filter_co2F_roll)))+
  geom_point()

# view the marked fluxes for day/night---
ggplot(flux.filter, aes(date_time, co2_flux, colour=factor(filter_co2F_roll_daynight)))+
  geom_point()

# graph with the fluxes from the 3SD 3day day/night filter removed---
ggplot(flux.filter[filter_co2F_roll_daynight==0,], aes(date_time, co2_flux, colour=factor(filter_co2F_roll_daynight)))+
  geom_point()

# SD filter for LE
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux.filter[, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux.filter[, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=daytime]

#calculate mean and standard dev for first 3 days fr daytime/nighttime
flux.filter[1:(((24/0.5)*3-1)*2.5), ':=' (LE_rollmean3_daynight = mean(LE, na.rm=TRUE),
                                    LE_rollsd3_daynight = sd(LE, na.rm=TRUE)),
            by=daytime]


threshold <- 3

# graph the 3 and 7 day SD ribbons around measured flux.data
ggplot(flux.filter)+
  geom_line(aes(date_time, LE))+
  geom_ribbon(aes(x=date_time, ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=date_time, ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux.data by day/night
ggplot(flux.filter)+
  geom_line(aes(date_time, LE))+
  geom_ribbon(aes(x=date_time, ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux.filter[,filter_le_roll := 0L]
flux.filter[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux.filter[,filter_le_roll_daynight := 0L]
flux.filter[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
           LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]


# view the marked fluxes for deletion
ggplot(flux.filter, aes(date_time, LE, colour=factor(filter_le_roll)))+
  geom_point()

# view the marked fluxes for day/night
ggplot(flux.filter, aes(date_time, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()

# graph with the flux.dataes from the 3SD 3day day/night filter removed
ggplot(flux.filter[filter_le_roll_daynight==0,], aes(date_time, LE, color = factor(filter_le_roll_daynight)))+
  geom_point()

# SD filter for H
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux.filter[, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                             H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                             H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux.filter[, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                             H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=daytime]


threshold <- 3

# graph the 3 and 7 day SD ribbons around measured flux.data
ggplot(flux.filter)+
  geom_line(aes(date_time, H))+
  geom_ribbon(aes(x=date_time, ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=date_time, ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux.data by day/night
ggplot(flux.filter)+
  geom_line(aes(date_time, H))+
  geom_ribbon(aes(x=date_time, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

#calculate mean and standard dev for first 3 days fr daytime/nighttime
flux.filter[1:(((24/0.5)*3-1)*2.5), ':=' (H_rollmean3_daynight = mean(H, na.rm=TRUE),
                                    H_rollsd3_daynight = sd(H, na.rm=TRUE)),
            by=daytime]

# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux.filter[,filter_h_roll := 0L]
flux.filter[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]

# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux.filter[,filter_h_roll_daynight := 0L]
flux.filter[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
           H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]


# view the marked fluxes for deletion
ggplot(flux.filter, aes(date_time, H, colour=factor(filter_h_roll)))+
  geom_point()

# view the marked fluxes for day/night
ggplot(flux.filter, aes(date_time, H, colour=factor(filter_h_roll_daynight)))+
  geom_point()

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux.filter[filter_h_roll_daynight==0,], aes(date_time, H, color = factor(filter_h_roll_daynight)))+
  geom_point()

#filter co2 flux, Le, and H using the daytime/nighttime 3 day rolling mean

#########
########### Filtering DONE ###############
########### 

# Prior to saving, filter data and remove unnecessary columns
# c("date_time_orig","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END","time_diff","SW_IN_POT_AF") 
# date_time_orig, time_diff and SW_IN_POT_AF probably won't be in files after 2019 because they were used for the timestamp corrections needed prior to 2019
flux_filter_sd <- copy(flux.filter)
flux_filter_sd[filter_co2F_roll_daynight!=0, co2_flux := NA]
flux_filter_sd[filter_h_roll_daynight!=0, H := NA]
flux_filter_sd[filter_le_roll_daynight!=0, LE := NA]

# check the time period is right: 
ggplot(flux_filter_sd,aes(date_time, co2_flux))+geom_point()
summary(flux_filter_sd$date_time)


# save to OneDrive
setwd("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SmartFlux/")

write.table(flux_filter_sd,
            file="RedLake_Flux_20211112_20240411_SmartFlux_Output_filtered_sd.csv",sep=",", dec=".",
            row.names=FALSE)


