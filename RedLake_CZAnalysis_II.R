# Red Lake Data Analysis I
# script author: Victoria Martinez and Marguerite Mauritz
# 23 April 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(zoo)
library(readxl)
library(cowplot)
library(reshape2)
library(bigleaf)


setwd("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SmartFlux/")

flux <- read.csv("RedLake_Flux_20211112_20240411_SmartFlux_Output_filtered_sd.csv", header = TRUE)

flux <-flux%>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")),
         date = ymd(date),
         year = year(date),
         month = month(date),
         hour = hour(date_time),
         TA_C = TA_1_1_1-272.15,
         ET = LE.to.ET(LE,TA_C),
         ET_30min = ET*60*30)

#create daily summary
flux.day <- flux%>%
  group_by(date)%>%
  summarise(NEE.day = mean(co2_flux, na.rm = TRUE),
            H.day = mean(H, na.rm = TRUE),
            ET.day = sum(ET_30min, na.rm = TRUE),
            T.day = mean((TA_1_1_1-272.15), na.rm = TRUE), #convert Kelvin to Celsius
            P.day = sum((P_RAIN_1_1_1*1000), na.rm = TRUE),
            S10.day = mean(SWC_1_1_1, na.rm = TRUE),
            S20.day = mean(SWC_1_2_1, na.rm = TRUE),
            S31.day = mean(SWC_1_3_1, na.rm = TRUE),
            S49.day = mean(SWC_1_4_1, na.rm = TRUE),
            S93.day = mean(SWC_1_5_1, na.rm = TRUE))%>%
  mutate(year = year(date),
         DOY = yday(date))
                           
#create hourly summary by month
flux.hour <- flux%>%
  group_by(year, month, hour)%>%
  summarise(NEE.hour = mean(co2_flux, na.rm = TRUE),
            H.hour = mean(H, na.rm = TRUE),
            T.hour = mean((TA_1_1_1-272.15), na.rm = TRUE)) #convert Kelvin to Celsius

#create monthly summary
flux.month <- flux%>%
  group_by(year, month)%>%
  summarise(T.month = round(mean(TA_C, na.rm = TRUE),2),
            P.month = sum(P_RAIN_1_1_1*1000, na.rm = TRUE))
           
#graph net ecosystem exchange per day  (-values are carbon absorption/+ values are carbon flowing into atmosphere)--  
#units in umol/m^2/sec
ggplot(flux.day, aes(date, NEE.day))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
facet_grid(.~year, scales = "free_x")

#ET in mm per day
ggplot(flux.day, aes(date, ET.day))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year, scales = "free_x")

#graph total precipitation per day in mm
ggplot(flux.day, aes(date, P.day))+
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year, scales = "free_x")

#graph avg temperature per day
ggplot(flux.day, aes(date, T.day))+
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year, scales = "free_x")

#precip ad temp in long format and compared throughout years
P.T.day <- flux.day %>%
  select(date, T.day, P.day, year)%>%
  pivot_longer(!c(date, year), names_to="variable",values_to="value")

ggplot(P.T.day, aes(x = date, y = value, color = factor(variable)))+
  geom_line()+
  facet_grid(.~year, scales = "free_x")

#soil 
#long format?
ggplot(flux.day, aes(x = date))+
  geom_line(aes(y = S10.day), color = "#BCD6E6")+
  geom_line(aes(y = S20.day), color = "#9EBBCC")+
  geom_line(aes(y = S31.day), color = "#8FA9B9")+
  geom_line(aes(y = S49.day), color = "#85A5BE")+
  geom_line(aes(y = S93.day), color = "#395663")+
  labs(y = "Soil Moisture (m^3/m^3)")


#hourly data per month for NEE--
ggplot(flux.hour, aes(hour, NEE.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)

#hourly data per month for H
ggplot(flux.hour, aes(hour, H.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)

#hourly data per month for LE
ggplot(flux.hour, aes(hour, LE.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)

#hourly data per month for Temperature
ggplot(flux.hour, aes(hour, T.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)

#Monthly Temp and Precip 
plot.T <- ggplot(flux.month, aes(month, T.month, color = factor(year)))+
  geom_point()+
  geom_line()+
  labs(color="Year")

ggplot(flux.month, aes(month, P.month, color = factor(year)))+
  geom_point()+
  geom_line()

plot.P <- ggplot(flux.month, aes(month, P.month, fill = factor(year)))+
  geom_col(position="dodge", width = 0.5)+
  labs(fill="Year")+
  theme_bw()

#graph monthly Temp and Precip together
plot_grid(plot.T, plot.P, nrow = 2, align = "v")

#gapfill data with reddyproc
#graph by year,month,hour as well
