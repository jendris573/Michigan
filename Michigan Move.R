# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggtext)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)

#read in Michigan climate data
climate <- read_excel("climate.xlsx")

#create column for Julian date
climate$julian_date <- yday(climate$DATE)

#create column for month
climate <- mutate(climate, month=month(climate$DATE))

#create column for year
climate <- mutate(climate, year=year(climate$DATE))

#omit NA in precipitation recordings 
precip <- climate[complete.cases(climate[,4]),] %>%
  group_by(NAME, julian_date) %>%
  summarise(mean_precip = mean(PRCP))

#omit NA in TMAX recordings 
snow <- climate[complete.cases(climate[,5]),] %>%
  group_by(NAME, julian_date) %>%
  summarise(mean_snow = mean(SNOW))

#omit NA in TMAX recordings 
tmax <- climate[complete.cases(climate[,6]),] %>%
  group_by(NAME, julian_date) %>%
  summarise(mean_TMAX = mean(TMAX))

#omit NA in TMIN recordings 
tmin <- climate[complete.cases(climate[,7]),] %>%
  group_by(NAME, julian_date) %>%
  summarise(mean_TMIN = mean(TMIN))

# # # # # # # # # # # # #
# Benton Harbor Plot ----
# # # # # # # # # # # # #

BH_means

BH_temps <-ggplot(data=mean_tmax, aes(x=julian_date, y= mean_TMAX, group=NAME, color=NAME)) +
  geom_line(y= mean_tmax$mean_TMAX, color= "red")+
  
  labs(y=expression("Average Temperarture (°F)"),
       x="Julian date")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggtitle("Average High Temperatures")

tmax_plot
