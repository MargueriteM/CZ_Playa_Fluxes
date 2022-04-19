# # load flux results data from Smartflux system
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)
library(plyr)

# get windrose function from github
source(paste0("https://raw.githubusercontent.com/MargueriteM/R_functions/master/plot.windrose.R"))

# working directory (don't need to set, all file reads have full path)
# setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/JER_RedLakePlaya/Data/SmartFlux/results/2021/09")
#

# get data from summaries folder
flux.files2 <- list.files(path="~/Desktop/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/SmartFlux/summaries",full.names=TRUE)

# read the column number for each summary file
read_column_number <- function(colname){
  ret <- ncol(fread(colname, sep="\t", dec=".", header=TRUE, skip=0)[1,])
  obj_name <- tools::file_path_sans_ext(basename(colname))
  out <- data.frame(file=obj_name, colnumber=ret)
  out
}

data <- ldply(flux.files2, read_column_number)


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
flux.data2 <- flux.data2 %>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")))

# make some quick graphs
flux.data2 %>%
  filter((co2_flux>-25 & co2_flux<25) & qc_co2_flux<2 & `u*`>0.2) %>%
  ggplot(., aes(date_time, co2_flux))+
  geom_point(aes(colour = factor(qc_co2_flux)), size=0.25)+
  geom_line(size=0.1)

flux.data2 %>%
  filter(qc_co2_flux<2 & `u*`>0.2) %>%
ggplot(., aes(date_time, w_rot))+geom_line()

ggplot(flux.data2, aes(date_time, v_rot))+geom_point()
ggplot(flux.data2, aes(date_time, w_rot))+geom_point()


ggplot(flux.data2, aes(date_time, WD_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, TS_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, TA_1_1_1))+geom_point()

ggplot(flux.data2, aes(date_time, VIN_1_1_1))+geom_line()

ggplot(flux.data2, aes(TA_1_1_1, TS_1_1_1))+geom_line()

ggplot(flux.data2, aes(TA_1_1_1, TC_1_1_1))+geom_line()


ggplot(flux.data2, aes(date_time, `x_90%`))+geom_point()

ggplot(flux.data2, aes(date_time, `u*`))+geom_point()

ggplot(flux.data2, aes(`u*`, `x_90%`))+geom_point()

# filter to keeep only data under turbulent conditions and remove NAs
flux.data2 %>%
  filter(`u*`>0.2) %>%
ggplot(., aes(date_time, `x_90%`))+
  geom_line()

# use windrose to plot 
footprint.data <- flux.data2 %>%
  select(date_time,WD_1_1_1,`u*`,`x_90%`)%>%
  filter(`u*`>0.2) %>%
  drop_na

# use windrose function to make a footprint graph: ... it's a hack.
plot.windrose(footprint.data,footprint.data$`x_90%`,footprint.data$WD_1_1_1,spdmax=1000,spdres=100)

# histogram of footprint distance
ggplot(footprint.data, aes(`x_90%`))+geom_histogram()

