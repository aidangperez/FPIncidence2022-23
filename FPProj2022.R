#################################
### FP Incidence Project 2022 ###
#################################

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
capture_data <- capture_data[!(is.na(capture_data$Fibropapilloma.Visible) | capture_data$Fibropapilloma.Visible==""), ]
table(capture_data$Fibropapilloma.Visible)

#isolate by region and by species (Chelonia mydas)
table(capture_data$Site)
table(capture_data$Species)
CRCD <- capture_data %>% 
  filter(grepl('Crystal River, FL, USA', Site)) %>% #call different site for different data set
  filter(grepl('Cm', Species))


#verify modifications worked 
table(CRCD$Site) #Should only contain region specific data
table(CRCD$Fibropapilloma.Visible) #should have no NA, consistent "No" spelling
table(CRCD$Species) #should only include Cm
table(CRCD$Year)

##################
### Data Plots ###
##################

### stack bar graph of visibility 

stackbg <- ggplot(data = CRCD, aes(Year, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(breaks = scales :: pretty_breaks(n = 10)) + 
  ggtitle("Crystal River") +
  geom_bar()

stackbg #generates a stacked bar graph of "Yes" and "No" values, Y axis on counts

### stack bar graph of visibility, normalized proportionally

propsum<- CRCD %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

stackbg_prop <- ggplot(data = propsum, aes(Year, prop, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(label = scales::percent) + 
  ggtitle("Crystal River") +
  geom_col()

stackbg_prop #generates a stacked bar graph, normalized grouping of "Yes" and "No" values, Y axis on proportion percentages

###############
### Mapping ###
###############


##############
### Export ###
##############

### note to edit titles in graph, final folder destination, plot, and file title prior to export

ggsave("stackbg_prop_CRYSTAL.pdf", plot = stackbg_prop,
       path = "/Users/aidanperez/Documents/FP_proj_2022/Plots/Crystal River")


