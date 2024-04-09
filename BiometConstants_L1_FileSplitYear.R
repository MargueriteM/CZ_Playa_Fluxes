

#BIOMET DATA FILE FROM DATALOGGER DOWNLOAD FOLDER SEPERATED BY YEAR AND MOVED INTO NEW DATA_L1 FOLDER


library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)

#see all files in biomet folder
list.files(path="C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Biomet")

# set working directory to One Drive folder with data (folder belongs to Marguerite Mauritz)
setwd("C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Biomet")

# read column names and import data
biomet.head <- colnames(read.table("CR3000 Red Lake Remote Connect_BiometConstants.dat.1.backup", sep=",", dec=".", skip=1, header=TRUE))
biomet.units <- read.table("CR3000 Red Lake Remote Connect_BiometConstants.dat.1.backup", sep=",", dec=".", skip=2, header=FALSE,col.names=biomet.head)[1,]
biometCon <- read.table("CR3000 Red Lake Remote Connect_BiometConstants.dat.1.backup", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=biomet.head, na.strings=c("NAN"))

#convert timestamp to date object
biometCon <- biometCon %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date=as.Date(TIMESTAMP),
         year=year(date))

#split file into individual years & save by each year
biometCon2021 <- biometCon %>%
  filter(year==2021)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

#add units
biometCon2021 <- rbind(biomet.units, biometCon2021)

biometCon2022 <- biometCon %>%
  filter(year==2022)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

biometCon2022 <- rbind(biomet.units, biometCon2022)

biometCon2023 <- biometCon %>%
  filter(year==2023)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

biometCon2023 <- rbind(biomet.units, biometCon2023)

#save to Data_L1
setwd("C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_L1/Biomet")
write.csv(biometCon2021, file="BiometConstants_L1_2021.csv", row.names=FALSE)
write.csv(biometCon2022, file="BiometConstants_L1_2022.csv", row.names=FALSE)
write.csv(biometCon2023, file="BiometConstants_L1_2023.csv", row.names=FALSE)


#Variable Checks Code into "SystemFiles" wihin Data folder
