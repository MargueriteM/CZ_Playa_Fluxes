# Red Lake Data Analysis I
# script author: Victoria Martinez and Marguerite Mauritz
# 05 August 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

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

data6 <- ldply(flux.files[727:854], read_column_number)

# read the flux files as csv and combine into single dataframe

data <- rbind(data1, data2, data3, data4, data5, data6)

# general column number is 211, select files
flux.files.read <- data1 %>%
  filter(colnumber==211) %>%
  mutate(file.path = paste("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Tower Data/JER_Playa/Data/Data_DL_Collect/SmartFlux/summaries/",
                           file,".txt",sep=''))

# get column names and units from complete summary files
flux.units <- fread(flux.files.read$file.path[1], sep="\t", dec=".", header=TRUE, skip=0)[1,]

# get data from complete summary files
flux.data <- do.call("rbind", lapply(flux.files.read$file.path, header = FALSE, fread, sep="\t", dec=".",skip = 2, fill=TRUE, na.strings="NaN", col.names=colnames(flux.units)))

# put flux.data in long format
flux.long <- flux.data %>%
  select(date, time, co2_flux, LE, H, co2_signal_strength_7500_mean, P_RAIN_1_1_1, PPFD_1_1_1, LWOUT_1_1_1, LWIN_1_1_1, RG_1_1_1, RH_1_1_1, RN_1_1_1, SHF_1_1_1, SHF_2_1_1, SHF_3_1_1, SWC_1_1_1, SWC_1_2_1, SWC_1_3_1, SWC_1_4_1, SWC_1_5_1, SWIN_1_1_1, SWOUT_1_1_1, TA_1_1_1, TC_1_1_1, TS_1_1_1, TS_1_2_1, TS_1_3_1, TS_1_4_1, TS_1_5_1, TS_2_1_1, TS_3_1_1, VIN_1_1_1, WD_1_1_1, ALB_1_1_1, air_pressure) %>%
  pivot_longer(!c(date, time), names_to="variable",values_to="value") %>%
  mutate(date_time=ymd_hms(paste(date,time,sep=" ")))


# graph data by flux and color by year 



ggplot(flux.long, aes(date_time, variable = "co2_flux"))+
  facet_grid(variable~value,scales="free_y")  

