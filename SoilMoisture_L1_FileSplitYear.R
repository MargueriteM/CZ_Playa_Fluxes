

#BIOMET DATA FILE FROM DATALOGGER DOWNLOAD FOLDER SEPERATED BY YEAR AND MOVED INTO NEW DATA_L1 FOLDER


library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)

#see all files in biomet folder
list.files(path="C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SoilMoisture_CS650")

# set working directory to One Drive folder with data (folder belongs to Marguerite Mauritz)
setwd("C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SoilMoisture_CS650")

# read column names and import data
sm.head1 <- colnames(read.table("CR3000 Red Lake Remote Connect_Soil_CS650.dat", sep=",", dec=".", skip=1, header=TRUE))
sm.units1 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650.dat", sep=",", dec=".", skip=2, header=FALSE,col.names=sm.head1)[1,]
sm1 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650.dat", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=sm.head1, na.strings=c("NAN"))

sm.head2 <- colnames(read.table("CR3000 Red Lake Remote Connect_Soil_CS650_2.dat.backup", sep=",", dec=".", skip=1, header=TRUE))
sm.units2 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650_2.dat.backup", sep=",", dec=".", skip=2, header=FALSE,col.names=sm.head2)[1,]
sm2 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650_2.dat.backup", sep=",", dec=".", skip=4, header=FALSE,
                  col.names=sm.head2, na.strings=c("NAN"))

sm.head3 <- colnames(read.table("CR3000 Red Lake Remote Connect_Soil_CS650_3.dat", sep=",", dec=".", skip=1, header=TRUE))
sm.units3 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650_3.dat", sep=",", dec=".", skip=2, header=FALSE,col.names=sm.head3)[1,]
sm3 <- read.table("CR3000 Red Lake Remote Connect_Soil_CS650_3.dat", sep=",", dec=".", skip=4, header=FALSE,
                  col.names=sm.head3, na.strings=c("NAN"))

sm <- rbind(sm1,sm2, sm3)

#convert timestamp to date object
sm <- sm %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date=as.Date(TIMESTAMP),
         year=year(date))

#split file into individual years & save by each year
sm2021 <- sm %>%
  filter(year==2021)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

#add units
sm2021 <- rbind(sm.units1, sm2021)

sm2022 <- sm %>%
  filter(year==2022)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

sm2022 <- rbind(sm.units2, sm2022)

sm2023 <- sm %>%
  filter(year==2023)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

sm2023 <- rbind(sm.units3, sm2023)

#save to Data_L1
setwd("C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_L1/SoilMoisture_CS650")
write.csv(sm2021, file="SoilMoisture_L1_2021.csv", row.names=FALSE)
write.csv(sm2022, file="SoilMoisture_L1_2022.csv", row.names=FALSE)
write.csv(sm2023, file="SoilMoisture_L1_2023.csv", row.names=FALSE)


#Variable Checks Code into "SystemFiles" wihin Data folder
