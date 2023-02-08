#################################
### FP Incidence Project 2022 ###
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
capture_data <- capture_data[!(is.na(capture_data$Fibropapilloma.Visible) | capture_data$Fibropapilloma.Visible==""), ]
table(capture_data$Fibropapilloma.Visible)

#isolate by region and by species (Chelonia mydas)
table(capture_data$Site)
table(capture_data$Species)
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

### summary data function script 
#summary data
sum_data <- function(y, upper_limit = max(11)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'x̄ =', round(mean(y), 1), '\n')))
}


##################
### Data Plots ###
##################

### stack bar graph of visibility by year 

stackbg_Y <- ggplot(data = CRCD, aes(Year, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(breaks = scales :: pretty_breaks(n = 10)) + 
  ggtitle("Crystal River") +
  geom_bar()

stackbg_Y #generates a stacked bar graph of "Yes" and "No" values by year, Y axis on counts

### stack bar graph of visibility, by season

stackbg_S <- ggplot(data = CRCD, aes(Season, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_y_continuous(breaks = scales :: pretty_breaks(n = 10)) + 
  ggtitle("Crystal River") +
  geom_bar()

stackbg_S #generates a stacked bar graph of "Yes" and "No" values by season, Y axis on counts 

### histogram of visibility, by SCL

histo_SCL <- ggplot(data = CRCD, aes(SCL.Standard.cm, fill = Fibropapilloma.Visible)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks = scales :: pretty_breaks(n = 10)) +
  theme_linedraw() + 
  ggtitle("Crystal River")

histo_SCL #generates a stacked histogram of "Yes" and "No" values by SCL, Y axis on counts

### stack bar graph of visibility by year, normalized proportionally

propsumYEAR<- CRCD %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

stackbg_Y_prop <- ggplot(data = propsumYEAR, aes(Year, prop, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(label = scales::percent) + 
  ggtitle("Crystal River") +
  geom_col()

stackbg_Y_prop #generates a stacked bar graph, normalized grouping of "Yes" and "No" values, Y axis on proportion percentages, by year

#### stack bar graph of visibility by Season, normalized proportionally

propsumSEAS<- CRCD %>%
  group_by(Season, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Season) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

stackbg_S_prop <- ggplot(data = propsumSEAS, aes(Season, prop, fill = Fibropapilloma.Visible )) +
  theme_linedraw() +
  scale_y_continuous(labels = scales :: percent) + 
  ggtitle("Crystal River") +
  geom_col()

stackbg_S_prop #generates a stacked bar graph of "Yes" and "No" values, Y axis on proportion percentages, by season

### stack bar graph of visibility by SCL, normalized proportionally

histo_SCL_prop <- ggplot(data = CRCD, aes(SCL.Standard.cm, fill = Fibropapilloma.Visible)) +
  geom_histogram(bins = 10, position = 'fill') +
  scale_x_continuous(breaks = scales :: pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales :: percent) +
  theme_linedraw() + 
  ggtitle("Crystal River")

histo_SCL_prop #generates a stacked histogram of "yes" and "No" values, Y axis on proportion percentages, by SCL

### plot of FP Balazs scores by year (with linear regression)

balazs_plot_year <- ggplot(CRCD, aes(Year,  FP.Balazs.Score)) +
  theme_linedraw() +
  geom_jitter(shape = 5,
              size = 3, stroke = 1,
              width = .2, height = .2) +
  scale_y_continuous(breaks = scales :: pretty_breaks(n=12)) +
  geom_smooth(method = "lm", se = T, col = "#ed3737", alpha = .3) +
  ggtitle("Crystal River") +
  stat_summary(fun.data = sum_data, geom = "text")

balazs_plot_year #generates a graph of plots detailing FP balazs score metric compared to capture years, with a standard linear regression imposed

### plot of FP Balazs scores by season 

balazs_plot_season <- ggplot(CRCD, aes(Season,  FP.Balazs.Score)) +
  theme_linedraw() +
  geom_jitter(shape = 5,
              size = 3, stroke = 1,
              width = .2, height = .2) +
  scale_y_continuous(breaks = scales :: pretty_breaks(n=12)) +
  geom_smooth(method = "lm", se = T, col = "#ed3737", alpha = .3) +
  ggtitle("Crystal River") +
  stat_summary(fun.data = sum_data, geom = "text")

balazs_plot_season #generates a graph of plots detailing FP balazs score metric compared to capture seasons

### plot of FP Balazs scores by SCL (with linear regression)

balazs_plot_SCL <- ggplot(CRCD, aes(SCL.Standard.cm,  FP.Balazs.Score)) +
  theme_linedraw() +
  geom_jitter(shape = 5,
              size = 3, stroke = 1,
              width = .2, height = .2) +
  scale_y_continuous(breaks = scales :: pretty_breaks(n=12)) +
  scale_x_continuous(breaks = scales :: pretty_breaks(n = 20)) +
  xlim(25, 60) +
  geom_smooth(method = "lm", se = T, col = "#ed3737", alpha = .3) +
  ggtitle("Crystal River")

balazs_plot_SCL #generates a graph of plots detailing FP balazs score metric compared to SCL

################################################################
### North and South Bimini Visibility Qualitative Comparison ###
################################################################

# plot a count comparing # of turtles in north and south bimini distinctively (use some kind of y coordinate filter)
# plot just north bimini visibilty
# plot just north bimini severity 
# plot just south bimini visibilty
# plot just south bimini severity 


##############
### Export ###
##############

### note to edit titles in graph, final folder destination, plot, and file title prior to export

ggsave("balazs_plot_SCLCR.pdf", plot = balazs_plot_SCL,
       path = "/Users/aidanperez/Documents/FP_proj_2022/Plots/Crystal River")

table(CRCD$Fibropapilloma.Visible)


