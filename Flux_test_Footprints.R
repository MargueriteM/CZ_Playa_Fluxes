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
# setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/JER_RedLakePlaya/Data/Data_DL_Collect/SmartFlux/results/2021/09")
#

# get data from summaries folder

flux.files2 <- list.files(path="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Playa/Data/Data_DL_Collect/SmartFlux/summaries",full.names=TRUE)


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


data3 <- ldply(flux.files2[174:328], read_column_number)

# next
data4 <- ldply(flux.files2[329:460], read_column_number)

# next
data5 <- ldply(flux.files2[461:574], read_column_number)

data6 <- ldply(flux.files2[575:679], read_column_number)

data7 <- ldply(flux.files2[680:706], read_column_number)

data8 <- ldply(flux.files2[707:825], read_column_number)

data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8)


# read the flux files as csv and combine into single dataframe

# general column number is 211, select files
flux.files.read <- data %>%
  filter(colnumber==211) %>%

  mutate(file.path = paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Playa/Data/Data_DL_Collect/SmartFlux/summaries/",
                           file,".txt",sep=''))
# get column names and units from complete summary files
flux.units2 <- fread(flux.files.read[1,"file.path"], sep="\t", dec=".", header=TRUE, skip=0)[1,]

# get data from complete summary files
flux.data2 <- do.call("rbind", lapply(flux.files.read$file.path, header = FALSE, fread, sep="\t", dec=".",
                                      skip = 2, fill=TRUE, na.strings="NaN", col.names=colnames(flux.units2)))

# format date_time variable
flux.data2 <- flux.data2 %>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")))

# view end of data
tail(flux.data2)

# graph flux and u*
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2) %>%
  ggplot(., aes(`u*`, co2_flux))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)
  

# make some quick graphs
datefilter <- min(as.Date(flux.data2$date_time)) #as.Date("2023-01-01")
# flux
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) &
           qc_co2_flux<2 & `u*`>0.2 &
           co2_signal_strength_7500_mean>85&
           as.Date(date_time)>datefilter) %>%
  ggplot(., aes(date_time, co2_flux))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("Half-hourly NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x = "Month")+
  facet_grid(.~year(date), scales="free_x")+
  theme_bw()+
  theme(legend.position="bottom")

# co2 mol fraction
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2 & 
           `u*`>0.2 &
           co2_signal_strength_7500_mean>85 &
           as.Date(date_time)>datefilter) %>%
  ggplot(., aes(date_time, co2_mole_fraction))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("C" *O[2]*" Mole Fraction (μmol mo"*l^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")

# constrain the flux further, mutliply to g and plot by year

# calculate half-hourly gC
flux.g <- flux.data2 %>%
  filter((co2_flux>-10 & co2_flux<5) & qc_co2_flux<2 & `u*`>0.2 & co2_signal_strength_7500_mean>85) %>%
  #group_by(date) %>%
  mutate(co2_g = co2_flux*1800*1*10^-6*12.01, na.rm=TRUE)

 
  ggplot(flux.g, aes(date_time, co2_g))+
  geom_point (size=0.25)+
    geom_line(size=0.1)+
  labs(y=expression("Half-hourly NEE (g C" *m^-2* "30mi" *n^-1*")"),
       x = "Month")+
    geom_hline(yintercept=0)+
    facet_grid(.~year(date), scales="free_x")+
  theme_bw()
  
  # rain in mm by day
  ggplot(flux.data2, aes(yday(date_time), P_RAIN_1_1_1*1000))+
    geom_line()+
    labs(y="Rainfall (mm)",
         x = "Month")+
    theme_bw()+
    facet_grid(.~year(date))

#LE graphs
flux.data2 %>%
  filter(qc_LE<2 & `u*`>0.2 &
           as.Date(date_time)>datefilter) %>%
  ggplot(., aes(date_time, (LE/2454000)*1800))+
  geom_point(aes(colour = factor(qc_LE)), size=0.25)+
  geom_line(size=0.1)+
  labs(y=expression("Half-hourly ET (mm" *m^-2* "se" *c^-1*")"),
       x = "Month")+
  theme_bw()+
  theme(legend.position="bottom")



flux.data2 %>%
  filter(qc_co2_flux<2 & `u*`>0.2&
           as.Date(date_time)>datefilter) %>%
ggplot(., aes(date_time, w_rot))+geom_line()

flux.data2 %>%
  filter(qc_co2_flux<2 & `u*`>0.2&
           as.Date(date_time)>datefilter) %>%
  ggplot(., aes(date_time, w_rot))+geom_line()

# rain in mm
ggplot(flux.data2, aes(date_time, P_RAIN_1_1_1*1000))+
  geom_line()+
  labs(y="Rainfall (mm)",
       x = "Month")+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")

# Air Temperature in C
ggplot(flux.data2, aes(date_time, TA_1_1_1-273.15))+
  geom_point(size=0.1,color="grey")+
  geom_line(size=0.2)+
  labs(y=expression("Air Temperature ("~degree~"C)"), x="Month")+
  theme_bw()

# Soil Temperature profiles in C
ggplot(flux.data2, aes(x=date_time))+
  geom_line(size=0.2, aes(y=TS_1_1_1-273.15))+
  geom_line(size=0.2, aes(y=TS_1_2_1-273.15),color="lightblue")+
  geom_line(size=0.4, aes(y=TS_1_3_1-273.15),color="grey")+
  geom_line(size=0.4, aes(y=TS_1_4_1-273.15),color="lightgrey")+
  geom_line(size=0.4, aes(y=TS_1_5_1-273.15),color="lightgreen")+
  labs(y=expression("Soil Temperature ("~degree~"C)"), x="Month")+
  theme_bw()

# soil integrated surface temperature in C
ggplot(flux.data2, aes(x=date_time))+
  geom_line(size=0.2, aes(y=TS_2_1_1-273.15))+
  geom_line(size=0.2, aes(y=TS_3_1_1-273.15),color="lightblue")+
  labs(y=expression("Soil Temperature ("~degree~"C)"), x="Month")+
  theme_bw()

# windrose
wind.dat <- flux.data2%>%
  filter(as.Date(date_time)>datefilter)%>%
select(date_time,WS_1_1_1,WD_1_1_1, wind_speed, wind_dir)%>%
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

# graph single variables
ggplot(flux.data2, aes(date_time, v_rot))+geom_point()
ggplot(flux.data2, aes(date_time, w_rot))+geom_point()


ggplot(flux.data2, aes(date_time, WD_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, TS_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, TA_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, TC_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, VIN_1_1_1))+geom_line()

ggplot(flux.data2, aes(date_time, sonic_temperature))+geom_line()

ggplot(flux.data2, aes(date_time, co2_signal_strength_7500_mean))+geom_line()

ggplot(flux.data2, aes(date_time, co2_mole_fraction))+geom_line()


ggplot(flux.data2, aes(TA_1_1_1, TS_1_1_1))+geom_line()

ggplot(flux.data2, aes(TA_1_1_1, TC_1_1_1))+geom_line()


ggplot(flux.data2, aes(date_time, `x_90%`))+geom_point()

ggplot(flux.data2, aes(date_time, `u*`))+geom_point()

ggplot(flux.data2, aes(`u*`, `x_90%`))+geom_point()

# filter to keep only data under turbulent conditions and remove NAs
flux.data2 %>%
  filter(`u*`>0.2) %>%
ggplot(., aes(date_time, `x_90%`))+
  geom_line()

# use windrose to plot footprints
footprint.data <- flux.data2 %>%
  select(date_time,WD_1_1_1,`u*`,`x_90%`,`x_70%`,`x_50%`,`x_30%`,`x_10%`)%>%
  filter(`u*`>0.2) %>%
  drop_na

# use windrose function to make a footprint graph: ... it's a hack.
plot.windrose(footprint.data,footprint.data$`x_90%`,footprint.data$WD_1_1_1,spdmax=1000,spdres=100)+
  theme_bw()+
  labs(title="Footprint direction & distance")

# histogram of footprint distance
ggplot(footprint.data)+
  geom_histogram(aes(`x_10%`),fill="blue")+
  geom_histogram(aes(`x_30%`),fill="red")+
  geom_histogram(aes(`x_50%`),fill="green")+
  geom_histogram(aes(`x_70%`),fill="purple")+
  geom_histogram(aes(`x_90%`),colour="grey")+
  labs(x="Distance Contribution (m)")+
  theme_bw()
  
