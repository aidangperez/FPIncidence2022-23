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
  filter(grepl('Cm', Species))

#verify modifications worked 
table(CRCD$Site) #Should only contain region specific data
table(CRCD$Fibropapilloma.Visible) #should have no NA, consistent "No" spelling
table(CRCD$Species) #should only include Cm
table(CRCD$FP.Balazs.Score)
table(CRCD$Year)

#######################
### Bimini, Bahamas ###
#######################
### Import Bahamas shapefile 
bahamas <- st_read("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/high_res_biminishape/bimini_shape.shp")

### Plot of captures and visibility on North and South Bimini
plotBimNS1 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 1) +
  ggtitle("Plot of Captures and Visibility; North and South Bimini") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  scale_color_manual(values=c("#fce5cd", "#cc0000")) +
  xlim(-79.37,-79.21) +
  ylim(25.61,25.8) +
  coord_sf() 

plotBimNS1

### Plot of North Bimini with affected FP
plotBimN2 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 2) +
  ggtitle("Plot of Captures and Visibility; North Bimini") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  scale_color_manual(values=c("#fce5cd", "#cc0000")) +
  xlim(-79.3,-79.23) +
  ylim(25.72,25.78) +
  coord_sf() 

plotBimN2

### Plot of North Bimini with FP severity (Balazs score)
plotBimSevN2 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  scale_color_brewer(palette = "YlOrRd") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 2) +
  ggtitle("Plot of FP Severity (Balazs Score); North Bimini") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  xlim(-79.27,-79.24) +
  ylim(25.73,25.76) +
  coord_sf() 

plotBimSevN2

### Plot of South Bimini with affected FP
plotBimS <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 2) +
  ggtitle("Plot of Captures and Visibility; South Bimini") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  scale_color_manual(values=c("#fce5cd", "#cc0000")) +
  xlim(-79.32,-79.23) +
  ylim(25.61,25.72) +
  coord_sf() 

plotBimS

### Plot of South Bimini with FP Severity (Balazs score)
plotBimSevS2 <- ggplot()+
  geom_sf(data = bahamas, fill = "grey") +
  scale_color_brewer(palette = "YlOrRd") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 2) +
  ggtitle("Plot of FP Severity (Balazs Score); South Bimini") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  xlim(-79.29,-79.24) +
  ylim(25.66,25.72) +
  coord_sf() 

plotBimSevS2



########################################
### Crystal River, Fl, United States ###
########################################
### Import Florida shapefile 
florida <- st_read("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/fl_shapefile/Florida_Shoreline_(1_to_40%2C000_Scale).shp")

### Plot of captures and visibility in Crytal River
plotFLVis <- ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), size = 3) +
  ggtitle("Plot of Captures and Visibility; Florida") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  scale_color_manual(values=c("#fce5cd", "#cc0000")) +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  coord_sf()

plotFLVis

### Plot of Crystal River FP severity
plotFLSev <- ggplot()+
  geom_sf(data = florida, fill = "grey") +
  scale_color_brewer(palette = "YlOrRd") + 
  geom_point(data = CRCD, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 3) +
  ggtitle("Plot Severtiy; Florida") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#34c6eb")) +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  coord_sf()

plotFLSev

