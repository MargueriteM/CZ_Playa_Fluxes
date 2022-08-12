
###################################
# Format data from SmartFlux      #
# for upload to Ameriflux         #
#     M. Mauritz                  #
#    7 July 2022                  #
###################################

# Use data directly from SmartFlux system with default Basic EddyPro settings
# Site page: https://ameriflux.lbl.gov/sites/siteinfo/US-Jo3#overview
# Data-upload Instructions: https://ameriflux.lbl.gov/half-hourly-hourly-data-upload-format/

# # load flux results data from Smartflux system
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)
library(plyr)
library(cowplot)

# get windrose function from github
source(paste0("https://raw.githubusercontent.com/MargueriteM/R_functions/master/plot.windrose.R"))

# working directory (don't need to set, all file reads have full path)
# setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/JER_RedLakePlaya/Data/SmartFlux/results/2021/09")
#

# get data from summaries folder
flux.files2 <- list.files(path="~/Desktop/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SmartFlux/summaries",
                          full.names=TRUE,
                          pattern=".txt")

# read the column number for each summary file
read_column_number <- function(colname){
  ret <- ncol(fread(colname, sep="\t", dec=".", header=TRUE, skip=0)[1,])
  obj_name <- tools::file_path_sans_ext(basename(colname))
  out <- data.frame(file=obj_name, colnumber=ret)
  out
}

# split into 2 because reading all at once is too big
data1 <- ldply(flux.files2[1:150], read_column_number)

data2 <- ldply(flux.files2[151:167], read_column_number)

# issue with files between 167-174 (zero KB files)
data3 <- ldply(flux.files2[174:241], read_column_number)

data4 <- ldply(flux.files2[242:280], read_column_number)


data <- rbind(data1, data2, data3, data4)

# read the flux files as csv and combine into single dataframe

# general column number is 211, select files
flux.files.read <- data %>%
  filter(colnumber==211) %>%
  mutate(file.path = paste("~/Desktop/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SmartFlux/summaries/",
                           file,".txt",sep=''))
# get column names and units from complete summary files
flux.units2 <- fread(flux.files.read$file.path[1], sep="\t", dec=".", header=TRUE, skip=0)[1,]

# get data from complete summary files
flux.data2 <- do.call("rbind", lapply(flux.files.read$file.path, header = FALSE, fread, sep="\t", dec=".",
                                      skip = 2, fill=TRUE, na.strings="NaN", col.names=colnames(flux.units2)))

# format date_time variable
# create timestamp start/end column
# datafile name contains start time, "time" column contains end time of integration period
flux.data2 <- flux.data2 %>%
  mutate(TIMESTAMP_END=ymd_hms(paste(date,time,sep=" ")),
         TIMESTAMP_START=TIMESTAMP_END-minutes(30))

# check column names
colnames(flux.data2)
 
# make sure timestamp start/end are first two columns and remove columns "date", "time" to avoid confusion
# format all columns to be in the same order: 
names_all <- colnames(flux.data2[,!c("TIMESTAMP_START","TIMESTAMP_END","date","time"),with=FALSE])
names_output <- c("TIMESTAMP_START","TIMESTAMP_END",names_all)

setcolorder(flux.data2,names_output)

#
# make some quick graphs
# flux unfiltered
flux.data2 %>%
  #filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2 & `u*`>0.2 & co2_signal_strength_7500_mean>85) %>%
  ggplot(., aes(TIMESTAMP_START, co2_flux))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("Half-hourly NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")

# filtered
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2 & `u*`>0.2 & co2_signal_strength_7500_mean>85 & P_RAIN_1_1_1==0) %>%
  ggplot(., aes(TIMESTAMP_START, co2_flux))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("Half-hourly NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")

# 


# co2 mol fraction
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2 & `u*`>0.2 & co2_signal_strength_7500_mean>85) %>%
  ggplot(., aes(TIMESTAMP_START, co2_mole_fraction))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("C" *O[2]*" Mole Fraction (μmol mo"*l^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")

#LE graphs
flux.data2 %>%
  filter(qc_LE<2 & `u*`>0.2 & co2_signal_strength_7500_mean>85 & P_RAIN_1_1_1==0) %>%
  ggplot(., aes(TIMESTAMP_START, (LE/2454000)*1800))+
  geom_point(aes(colour = factor(qc_LE)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("Half-hourly ET (mm " *m^-2* " se" *c^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")


# rain in mm
ggplot(flux.data2, aes(TIMESTAMP_START, P_RAIN_1_1_1*1000))+
  geom_line()+
  labs(y="Rainfall (mm)",
       x = "Month")+
  theme_bw()

# Air Temperature in C
ggplot(flux.data2, aes(TIMESTAMP_START, TA_1_1_1-273.15))+
  geom_point(size=0.1,color="grey")+
  geom_line(size=0.2)+
  labs(y=expression("Air Temperature ("~degree~"C)"), x="Month")+
  theme_bw()

# windrose
wind.dat <- flux.data2%>%
  select(TIMESTAMP_START,WS_1_1_1,WD_1_1_1, wind_speed, wind_dir)%>%
  drop_na()

wind.2d <- plot.windrose(wind.dat,
                         wind.dat$WS_1_1_1,
                         wind.dat$WD_1_1_1)+
  theme_bw()+
  labs(title="2-D Anemometer")

# 
wind.3d <- plot.windrose(wind.dat,
                         wind.dat$wind_speed,
                         wind.dat$wind_dir)+
  theme_bw()+
  labs(title="Sonic Anemometer")

# graph 2-D and Sonic Wind Rose side-by-side
plot_grid(wind.2d,wind.3d,labels=c("A","B"))

# graph correlation between sonic and 2D wind speed
ggplot(wind.dat, aes(wind_speed, WS_1_1_1))+
  geom_point() +
  geom_abline(yintercept=0, slope=1)

# graph correlation between sonic and 2D wind direction
ggplot(wind.dat, aes(wind_dir, WD_1_1_1))+
  geom_point() +
  geom_abline(yintercept=0, slope=1)

# graph single variables
ggplot(flux.data2, aes(TIMESTAMP_START, v_rot))+geom_point()
ggplot(flux.data2, aes(TIMESTAMP_START, w_rot))+geom_point()


ggplot(flux.data2, aes(TIMESTAMP_START, WD_1_1_1))+geom_point()

# soil temperature profile
tsoil1 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_1_1_1-273.15))+geom_point() + ylim(-5,40) + theme(axis.title.x=element_blank())
tsoil2 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_1_2_1-273.15))+geom_point() + ylim(-5,40) + theme(axis.title.x=element_blank())
tsoil3 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_1_3_1-273.15))+geom_point() + ylim(-5,40) + theme(axis.title.x=element_blank())
tsoil4 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_1_4_1-273.15))+geom_point() + ylim(-5,40) + theme(axis.title.x=element_blank())
tsoil5 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_1_5_1-273.15))+geom_point() + ylim(-5,40) + theme(axis.title.x=element_blank())

plot_grid(tsoil1, tsoil2, tsoil3, tsoil4, tsoil5, nrow=5)

# TCAV probes with SHF
soil6 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_2_1_1-273.15))+geom_point() + ylim(-10,45) + theme(axis.title.x=element_blank())
shf1 <- ggplot(flux.data2, aes(TIMESTAMP_START, SHF_1_1_1))+geom_point() + ylim(-20,35) + theme(axis.title.x=element_blank())

plot_grid(soil6,shf1, nrow=2)

# TCAV probes with SHF
soil7 <- ggplot(flux.data2, aes(TIMESTAMP_START, TS_3_1_1-273.15))+geom_point() + ylim(-10,45) + theme(axis.title.x=element_blank())
shf2 <- ggplot(flux.data2, aes(TIMESTAMP_START, SHF_2_1_1))+geom_point() + ylim(-20,35) + theme(axis.title.x=element_blank())

plot_grid(soil7,shf2, nrow=2)

# final SHf
shf3 <- ggplot(flux.data2, aes(TIMESTAMP_START, SHF_3_1_1))+geom_point() + ylim(-20,35) + theme(axis.title.x=element_blank())

shf3

# soil temp and SHF all together
plot_grid(soil6, soil7, shf1, shf2, nrow=2, ncol=2)

# air temperature and surface temperature
tair <- ggplot(flux.data2, aes(TIMESTAMP_START, TA_1_1_1-273.15))+geom_point() + ylim(-25,65)

tsurf <- ggplot(flux.data2, aes(TIMESTAMP_START, TC_1_1_1-273.15))+geom_point() + ylim(-25,65)

plot_grid(tair, tsurf, nrow=2)

# sonic temp
tsonic <- ggplot(flux.data2, aes(TIMESTAMP_START, sonic_temperature-273.15))+geom_line()+ ylim(-25,65)

plot_grid(tair, tsurf, tsonic, nrow=3)


# voltage
ggplot(flux.data2, aes(TIMESTAMP_START, VIN_1_1_1))+geom_line()

# signal strength
ggplot(flux.data2, aes(TIMESTAMP_START, co2_signal_strength_7500_mean))+geom_line()

#  radiation components
lwin <- ggplot(flux.data2, aes(TIMESTAMP_START, LWIN_1_1_1))+geom_point() + ylim(0,750) + theme(axis.title.x=element_blank())
lwout <- ggplot(flux.data2, aes(TIMESTAMP_START, LWOUT_1_1_1))+geom_point() + ylim(0,750) + theme(axis.title.x=element_blank())
swin <- ggplot(flux.data2, aes(TIMESTAMP_START, SWIN_1_1_1))+geom_point() + ylim(-5,1600) + theme(axis.title.x=element_blank())
swout <- ggplot(flux.data2, aes(TIMESTAMP_START, SWOUT_1_1_1))+geom_point() + ylim(-5,1600) + theme(axis.title.x=element_blank())
ppfd <- ggplot(flux.data2, aes(TIMESTAMP_START, PPFD_1_1_1))+geom_point() + ylim(-5,2500) + theme(axis.title.x=element_blank())

plot_grid(lwin, lwout, swin, swout, ppfd, nrow=5)

# global and net radiation, albedo
rg <- ggplot(flux.data2, aes(TIMESTAMP_START, RG_1_1_1))+geom_point() + theme(axis.title.x=element_blank())
rn <- ggplot(flux.data2, aes(TIMESTAMP_START, RN_1_1_1))+geom_point() + theme(axis.title.x=element_blank())
alb <- ggplot(flux.data2, aes(TIMESTAMP_START, ALB_1_1_1))+geom_point() + theme(axis.title.x=element_blank())

plot_grid(rg,rn,alb, nrow=3)

# U*
ggplot(flux.data2, aes(TIMESTAMP_START, `u*`))+geom_point()

# rain, rh, lws??
rain <- ggplot(flux.data2, aes(TIMESTAMP_START, P_RAIN_1_1_1))+geom_point() + theme(axis.title.x=element_blank())
rh <- ggplot(flux.data2, aes(TIMESTAMP_START, RH_1_1_1))+geom_point() + theme(axis.title.x=element_blank())
# not transfering to ameriflux format
#lws <- ggplot(flux.data2, aes(TIMESTAMP_START, LWS_1_1_1))+geom_point() + theme(axis.title.x=element_blank())

plot_grid(rain, rh, nrow=2)
#plot_grid(rain, rh, lws, nrow=3)

# rain and SWC
vwc1 <- ggplot(flux.data2, aes(TIMESTAMP_START, SWC_1_1_1))+geom_point() + theme(axis.title.x=element_blank())
vwc2 <- ggplot(flux.data2, aes(TIMESTAMP_START, SWC_1_2_1))+geom_point()  + theme(axis.title.x=element_blank())
vwc3 <- ggplot(flux.data2, aes(TIMESTAMP_START, SWC_1_3_1))+geom_point()  + theme(axis.title.x=element_blank())
vwc4 <- ggplot(flux.data2, aes(TIMESTAMP_START, SWC_1_4_1))+geom_point() + theme(axis.title.x=element_blank())
vwc5 <- ggplot(flux.data2, aes(TIMESTAMP_START, SWC_1_5_1))+geom_point() + theme(axis.title.x=element_blank())

plot_grid(vwc1, vwc2, vwc3, vwc4, vwc5, rain, nrow=6)


# footprint
ggplot(flux.data2, aes(TIMESTAMP_START, `x_90%`))+geom_point()

ggplot(flux.data2, aes(`u*`, `x_90%`))+geom_point()

# filter to keep only data under turbulent conditions and remove NAs
flux.data2 %>%
  filter(`u*`>0.2) %>%
  ggplot(., aes(TIMESTAMP_START, `x_90%`))+
  geom_line()

# use windrose to plot footprints
footprint.data <- flux.data2 %>%
  select(TIMESTAMP_START,WD_1_1_1,`u*`,`x_90%`,`x_70%`,`x_50%`,`x_30%`,`x_10%`)%>%
  filter(`u*`>0.2) %>%
  drop_na

# use windrose function to make a footprint graph: ... it's a hack.
plot.windrose(footprint.data,footprint.data$`x_90%`,footprint.data$WD_1_1_1,spdmax=1000,spdres=100)+theme_bw()

# histogram of footprint distance
ggplot(footprint.data)+
  geom_histogram(aes(`x_10%`),fill="blue")+
  geom_histogram(aes(`x_30%`),fill="red")+
  geom_histogram(aes(`x_50%`),fill="green")+
  geom_histogram(aes(`x_70%`),fill="purple")+
  geom_histogram(aes(`x_90%`),colour="grey")+
  labs(x="Distance Contribution (m)")+
  theme_bw()




# save to upload to ameriflux, save to server: 
# <SITE_ID>_<RESOLUTION>_<TS-START>_<TS-END>_<OPTIONAL>.csv
# setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/Ameriflux_USJo1")

# save file in Ameriflux format
# write.table(flux.biomet[,!c("date_time"),with=FALSE],
#            file = paste("US-Jo1_HH_",min(flux.biomet$TIMESTAMP_START),"_",max(flux.biomet$TIMESTAMP_END),
#                         ".csv",sep=""),
#            sep=',', dec='.', row.names=FALSE, na="-9999", quote=FALSE)



