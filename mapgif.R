#############################################################
#### Animated gif of FP Visibility Across Capture Regions ###
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
