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
            S93.day = mean(SWC_1_5_1, na.rm = TRUE),
            TS10.day = mean(TS_1_1_1, na.rm = TRUE),
            TS20.day = mean(TS_1_2_1, na.rm = TRUE),
            TS31.day = mean(TS_1_3_1, na.rm = TRUE),
            TS49.day = mean(TS_1_4_1, na.rm = TRUE),
            TS93.day = mean(TS_1_5_1, na.rm = TRUE))%>%
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

months <- list("1" = "January",
               "2" = "February",
               "3" = "March",
               "4" = "April",
               "5" = "May",
               "6" = "June",
               "7" = "July",
               "8" = "August",
               "9" = "September",
               "10" = "October",
               "11" = "November",
               "12" = "December")

months_labeller <- function(variable,value){
  return(months[value])
}
           
#graph net ecosystem exchange per day  (-values are carbon absorption/+ values are carbon flowing into atmosphere)--  
#units in umol/m^2/sec
plot.NEE.day <- 
  ggplot(flux.day, aes(DOY, NEE.day, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
facet_grid(.~year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","Dec"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     #guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Daily Avg NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x="Month", col="Year")

#ET in mm per day
plot.ET.day <- 
  ggplot(flux.day, aes(DOY, ET.day, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","Dec"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     #guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Daily Sum ET (mm da" *y^-1*")"),
       x="Month", col="Year")

#graph total precipitation per day in mm
ggplot(flux.day, aes(DOY, P.day, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","Dec"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     #guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Daily Avg Precip (mm)"),
       x="Month", col="Year")

#graph avg temperature per day
ggplot(flux.day, aes(DOY, T.day, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_grid(.~year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","Dec"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     #guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Daily Avg Temp (°C)"),
       x="Month", col="Year")

#precip ad temp in long format and compared throughout years
P.T.day <- flux.day %>%
  select(date, T.day, P.day, year, DOY)%>%
  pivot_longer(!c(date, DOY, year), names_to="variable",values_to="value")

plot.P.T.day <- 
  ggplot(P.T.day, aes(x = DOY, y = value, color = factor(variable)))+
  geom_line()+
  facet_grid(.~year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","Dec"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     #guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Air Temp (°C)"),
       x="Month", col="Variable")
#+scale_y_continuous(sec.axis = sec_axis(~.,name= "Precip (mm)"))

#graph daily co2 flux, ET, and precip+temp variables in one figure
plot_grid(plot.NEE.day, plot.ET.day, plot.P.T.day, nrow=3, align="v")

#soil Heat Plates by depth
plot.VWC.day <-
  ggplot(flux.day, aes(x = date))+
  geom_line(aes(y = S10.day), color = "#BCD6E6")+
  geom_line(aes(y = S20.day), color = "#9EBBCC")+
  geom_line(aes(y = S31.day), color = "#8FA9B9")+
  geom_line(aes(y = S49.day), color = "#85A5BE")+
  geom_line(aes(y = S93.day), color = "#395663")+
  labs(y = "Soil Moisture (m^3/m^3)")

#plot soil T by depth
plot.TS.day <-
  ggplot(flux.day, aes(x = date))+
  geom_line(aes(y = TS10.day), color = "#BCD6E6")+
  geom_line(aes(y = TS20.day), color = "#9EBBCC")+
  geom_line(aes(y = TS31.day), color = "#8FA9B9")+
  geom_line(aes(y = TS49.day), color = "#85A5BE")+
  geom_line(aes(y = TS93.day), color = "#395663")+
  labs(y = "Soil Temp (°C)")


#Soil VWC, soil T, and precip+temp in one figure


###LEGEND MESSES UP THE SCALE EVEN WITHOUT THE SECONDARY AXIS


plot_grid(plot.VWC.day, plot.TS.day, plot.P.T.day, nrow=3, align="v")

#hourly data per month for NEE--
ggplot(flux.hour, aes(hour, NEE.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month, labeller=months_labeller)+
  labs(col="Year")

#hourly data per month for H
ggplot(flux.hour, aes(hour, H.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month, labeller=months_labeller)

#hourly data per month for LE
ggplot(flux.hour, aes(hour, LE.hour, color = factor(year)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month, labeller=months_labeller)

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
