

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
vc.head1 <- colnames(read.table("CR3000 Red Lake Serial_VariableChecks.dat", sep=",", dec=".", skip=1, header=TRUE))
vc.units1 <- read.table("CR3000 Red Lake Serial_VariableChecks.dat", sep=",", dec=".", skip=2, header=FALSE,col.names=vc.head1)[1,]
vc1 <- read.table("CR3000 Red Lake Serial_VariableChecks.dat", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=vc.head1, na.strings=c("NAN"))

vc.head2 <- colnames(read.table("CR3000 Red Lake Remote Connect_VariableChecks.dat.backup", sep=",", dec=".", skip=1, header=TRUE))
vc.units2 <- read.table("CR3000 Red Lake Remote Connect_VariableChecks.dat.backup", sep=",", dec=".", skip=2, header=FALSE,col.names=vc.head2)[1,]
vc2 <- read.table("CR3000 Red Lake Remote Connect_VariableChecks.dat.backup", sep=",", dec=".", skip=4, header=FALSE,
                  col.names=vc.head2, na.strings=c("NAN"))

vc <- rbind(vc1,vc2)

#convert timestamp to date object
vc <- vc %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date=as.Date(TIMESTAMP),
         year=year(date))

#split file into individual years & save by each year
vc2021 <- vc %>%
  filter(year==2021)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

#add units
vc2021 <- rbind(vc.units1, vc2021)

vc2022 <- vc %>%
  filter(year==2022)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

vc2022 <- rbind(vc.units2, vc2022)

vc2023 <- vc %>%
  filter(year==2023)%>%
  select(!c(year, date))%>%
  mutate(TIMESTAMP=as.character(TIMESTAMP))

vc2023 <- rbind(vc.units1, vc2023)

#save to Data_L1
setwd("C:/Users/uvmar/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_L1/Biomet")
write.csv(vc2021, file="VariableChecks_L1_2021.csv", row.names=FALSE)
write.csv(vc2022, file="VariableChecks_L1_2022.csv", row.names=FALSE)
write.csv(vc2023, file="VariableChecks_L1_2023.csv", row.names=FALSE)


#Variable Checks Code into "SystemFiles" wihin Data folder
