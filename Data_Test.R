# Check test files from Playa Tower

# load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# get windrose function
source("~/Desktop/R/R_programs/Functions/plot.windrose.R")

# set working directory to One Drive folder with data (folder belongs to Marguerite Mauritz)
setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/JER_RedLakePlaya/Data")

# read column names and import data
biomet.head <- colnames(read.table("CR3000 Red Lake Remote Connect_Biomet.dat", sep=",", dec=".", skip=1, header=TRUE))
biomet <- read.table("CR3000 Red Lake Remote Connect_Biomet.dat", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=biomet.head, na.strings=c("NAN"))

# see all column names
print(biomet.head)

# [1] "TIMESTAMP"         "RECORD"            "VIN_39_18_1_1_1"   "PPFD_21_7_1_1_1"   "WS_33_16_1_1_1"   
# [6] "WD_35_20_1_1_1"    "SWIN_10_6_1_1_1"   "SWOUT_11_6_1_1_1"  "LWIN_14_6_1_1_1"   "LWOUT_15_6_1_1_1" 
# [11] "RN_5_6_1_1_1"      "RN_4_6_1_1_1"      "ALB_26_99_1_1_1"   "TC_16_2_1_1_1"     "TA_1_2_1_1_1"     
# [16] "RH_3_19_1_1_1"     "LWS_33_16_1_1_1"   "PA_2_4_1_1_1"      "MWS_34_16_1_1_1"   "P_RAIN_19_8_1_1_1"
# [21] "TS_38_2_6_1_1"     "TS_38_2_7_1_1"     "SHF_37_6_1_1_1"    "SHF_37_6_2_1_1"    "SHF_37_6_3_1_1"   
# [26] "SWC_36_12_1_1_1"   "SWC_36_12_1_2_1"   "SWC_36_12_1_3_1"   "SWC_36_12_1_4_1"   "SWC_36_12_1_5_1"  
# [31] "TS_38_2_1_1_1"     "TS_38_2_1_2_1"     "TS_38_2_1_3_1"     "TS_38_2_1_4_1"     "TS_38_2_1_5_1"   

# change data to long format
biomet.long <- biomet %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP))

# plot battery voltage
biomet.long %>%
  filter(str_detect(variable,"^VIN")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot atmospheric pressure
biomet.long %>%
  filter(str_detect(variable,"^PA")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot all SWC variables
biomet.long %>%
  filter(str_detect(variable,"^SWC")) %>%
ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot all soil temperature and air temperature
biomet.long %>%
  filter(str_detect(variable,"^TS|^TA")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot all wind variables
biomet.long %>%
  filter(str_detect(variable,"^WD|^WS|^MWS")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot wind variables in windrose
plot.windrose(biomet,biomet$WS_16_33_1_1_1,biomet$WD_20_35_1_1_1)


# plot PAR and air temperature
biomet.long %>%
  filter(str_detect(variable,"^PPFD|^TA")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot radiation components
#"SWIN_10_6_1_1_1"   "SWOUT_11_6_1_1_1"  "LWIN_14_6_1_1_1"   "LWOUT_15_6_1_1_1" 
# [11] "RN_5_6_1_1_1"      "RN_4_6_1_1_1"      "ALB_26_99_1_1_1" 
biomet.long %>%
  filter(str_detect(variable,"^SWIN|^SWOUT|^LWIN|^LWOUT|^RN|^RG|^ALB|^PPFD")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# plot soil heat flux
biomet.long %>%
  filter(str_detect(variable,"^SHF")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# precipitation, relative humidity, leaf wetness
biomet.long %>%
  filter(str_detect(variable,"^P_RAIN|^RH|^LWS")) %>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_point()+
  geom_line()+
  facet_grid(variable~., scales="free_y")
