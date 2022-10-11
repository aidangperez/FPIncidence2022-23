#######################################
### Recaptured Turtle Investigation ###
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
table(CRCD$Year)
table(CRCD$Season)
table(CRCD$SCL.Standard.cm)
table(CRCD$FP.Balazs.Score)
table(CRCD$Tags)
table(CRCD$Flipper.Tag.1)
table(CRCD$Flipper.Tag.2)

#filter such that only repeated tag values are kept, presuming these were recaptured (and remove too small entries)
Recaps <- CRCD %>% 
  group_by(Flipper.Tag.1) %>%
  filter(!grepl('TOO SMALL', Flipper.Tag.1)) %>%
  filter(n()>1) %>% 
  group_by(Flipper.Tag.2) %>%
  filter(!grepl('TOO SMALL', Flipper.Tag.2)) %>%
  filter(n()>1) 

table(Recaps$Flipper.Tag.1)
table(Recaps$Flipper.Tag.2)

#verify consistency with Flipper.Tag.2 column

