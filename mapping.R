######################
### Turtle Mapping ###
##################################################################
### Marine Turtle Conservation Lab at Florida State University ###
##################################################################


###load packages 
library(git2r)
library(ggpubr)
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(lubridate)
library(janitor)
library(reshape)
library(ggplot2)
library(magick)
library(rnaturalearth)
library(mapview)
library(sf)
library(dplyr)
library(sp)

###import data
capture_data <- read.csv("/Users/aidanperez/Documents/FP_proj_2022/Datasets/2022-02_Cm_captures.csv")
fp_score <- read.csv("/Users/aidanperez/Documents/FP_proj_2022/Datasets/2022-02_Cm_FP_Scores.csv")


###modify data 
#standardize spelling on FP Visibility
capture_data$Fibropapilloma.Visible<- gsub(pattern = " *",  #detect space(s)
                                           replacement = "", #replace w/ nothing
                                           x = capture_data$Fibropapilloma.Visible)
table(capture_data$Fibropapilloma.Visible, useNA = "ifany")  #confirm that issue is fixed

#remove "NA" values 
capture_data$Fibropapilloma.Visible <- filter(!is.na(Fibropapilloma.Visible))
capture_data <- capture_data[!(is.na(capture_data$Fibropapilloma.Visible) | capture_data$Fibropapilloma.Visible==""), ] ## fix this
table(capture_data$Fibropapilloma.Visible)

#isolate by region, species (Chelonia mydas), and recapture
table(capture_data$Site)
table(capture_data$Species)
table(capture_data$Tags)
CRCD <- capture_data %>% 
  filter(grepl('Bimini, Bahamas', Site)) %>% #call different site for different data set
  filter(grepl('Cm', Species))

#verify modifications worked 
table(CRCD$Site) #Should only contain region specific data
table(CRCD$Fibropapilloma.Visible) #should have no NA, consistent "No" spelling
table(CRCD$Species) #should only include Cm
table(CRCD$FP.Balazs.Score)

### Import Bahamas shapefile 
bahamas <- st_read ("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/data/BHS_adm1.shp")

### Plot of captures and visibility on North and South Bimini
plotNS1 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 1) +
  ggtitle("Plot of Captures and Visibility; North and South Bimini") +
  theme_bw() +
  xlim(-79.37,-79.21) +
  ylim(25.61,25.8) +
  coord_sf() 

plotNS1

### Plot of North Bimini with affected FP
plotN2 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 1) +
  ggtitle("Plot of Captures and Visibility; North Bimini") +
  theme_bw() +
  xlim(-79.3,-79.23) +
  ylim(25.72,25.78) +
  coord_sf() 

plotN2

  
 