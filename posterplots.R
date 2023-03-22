####################
### Poster Plots ###
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

#isolate by species (Chelonia mydas)
table(capture_data$Site)
table(capture_data$Species)
CRCD <- capture_data %>% 
  filter(grepl('Cm', Species))

table(CRCD$Species)

#isolate two data frames by site
table(CRCD$Site)
CRCDBIM <- CRCD %>% 
  filter(grepl('Bimini, Bahamas', Site))
CRCDCR <- CRCD %>% 
  filter(grepl('Crystal River, FL, USA', Site))
CRCDBFH <- CRCD %>%
  filter(grepl('Bonefish Hole', Location))

#load shape files
florida <- st_read("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/fl_shapefile/Florida_Shoreline_(1_to_40%2C000_Scale).shp")
bimini <- st_read("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/high_res_biminishape/bimini_shape.shp")
states <- ne_states(country = "United States of America", returnclass = "sf")
bahamas <- st_read("/Users/aidanperez/Documents/FP_proj_2022/Datasets/map/bhs_adm0_shape/BHS_adm0.shp")

############
### Maps ###
############

### Crystal River ###
#Crystal River Map Inset
submapCR <- ggplot() +
  theme_void() + 
  geom_sf(data = states) +
  xlim(-84.5, -82) +
  ylim (28, 30.2) +
  annotate("rect", fill = "#09b7e3", alpha = 0.8,
           xmin = -82.82, xmax = -82.57, ymin = 28.65, ymax = 28.85) +
  coord_sf() + 
  theme(plot.background = element_rect(colour = "black", size = 2),
        plot.margin = margin(2, 2, 2, 2))

#Crystal River Large
maplgCR <- ggplot() +
  geom_sf(data = florida, fill = "grey") +
  scale_color_brewer(palette = "PuOr" ) + 
  geom_point(data = CRCDCR, aes (x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), 
             shape = 21,
             stroke = 2,
             size = 2,
             col = "black",
             show.legend = FALSE) + 
  geom_point(data = CRCDCR, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)),
             size = 2,
             alpha = .85,
             show.legend = FALSE) +
  theme_linedraw() + 
  theme(#axis.title = element_text(size = 14),
        #axis.text.x = element_text(size = 10),
        #axis.text.y = element_text(size = 10),
        title = element_text(face = "bold"),
        #legend.title = element_text(size = 12),
        #legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "#000000"),
        legend.background = element_rect(fill = "#b8b8b8"),
        legend.key = element_rect(fill = "#b8b8b8"),
        panel.background = element_rect(fill = "#34c6eb")
  ) + 
  guides(color = guide_legend(title = "Fibropapilloma Visiblity")) + 
  labs(x = "Longitude", 
       y = "Latitude",
       title = "FP Visiblity in Crystal River 2016-2022") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  coord_sf()

maplgCR

### Bimini 
maplgBIM <- ggplot() +
  geom_sf(data = bimini, fill = "grey") +
  scale_color_brewer(palette = "PuOr" ) + 
  geom_point(data = CRCDBIM, aes (x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)), 
             shape = 21,
             stroke = 2,
             size = 2,
             col = "black",
             show.legend = FALSE) + 
  geom_point(data = CRCDBIM, aes(x = GPS_X, y = GPS_Y, color = factor(Fibropapilloma.Visible)),
             size = 2,
             alpha = .85,
             show.legend = FALSE) +
  theme_linedraw() + 
  theme(#axis.title = element_text(size = 14),
    #axis.text.x = element_text(size = 10),
    #axis.text.y = element_text(size = 10),
    title = element_text(face = "bold"),
    #legend.title = element_text(size = 12),
    #legend.text = element_text(size = 10),
    legend.box.background = element_rect(color = "#000000"),
    legend.background = element_rect(fill = "#b8b8b8"),
    legend.key = element_rect(fill = "#b8b8b8"),
    panel.background = element_rect(fill = "#34c6eb")
  ) + 
  guides(color = guide_legend(title = "Fibropapilloma Visiblity")) + 
  labs(x = "Longitude", 
       y = "Latitude",
       title = "FP Visiblity in Bimini 2016-2018") +
  xlim(-79.34,-79.21) +
  ylim(25.65,25.77) +
  coord_sf()

maplgBIM

#############
### Plots ###
#############

# Plotting counts prop crystal river 

fp_visibility <- CRCDCR %>%
  group_by(Year,Fibropapilloma.Visible) %>%
  summarise(fp_visibility = length((Fibropapilloma.Visible)))

fp_year_summ<- CRCDCR %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

blues <- c("#d1d1d1", "#09b7e3")


stack_propCR <- ggplot(fp_year_summ, aes(Year, prop, fill = Fibropapilloma.Visible)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual("FP Visible", values = blues) +
  theme_linedraw() + 
  theme(#axis.title.x = element_text(size = 18, face = "bold"),
       # axis.title = element_text(size = 12),
        axis.title.y = element_text(face = "bold"),
       # axis.text.x = element_text(size = 12),
        #axis.text.y = element_text(size = 12),
        title = element_text( face = "bold"),
        legend.title = element_text(size = 14),
        legend.key.size = (unit(1, 'in')),
        legend.text = element_text(size = 14)
  ) + 
  scale_x_continuous( breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(label = scales::percent ) + 
  labs(x = "",
       y = "Proportion of Turtles",
       title = "FP Visiblity in Crystal River 2016-2022")

stack_propCR

# Plotting counts prop Bimini

fp_visibility2 <- CRCDBIM %>%
  group_by(Year,Fibropapilloma.Visible) %>%
  summarise(fp_visibility = length((Fibropapilloma.Visible)))

fp_year_summ2<- CRCDBIM %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

blues <- c("#d1d1d1", "#09b7e3")


stack_propBIM <- ggplot(fp_year_summ2, aes(Year, prop, fill = Fibropapilloma.Visible)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual("FP Visible", values = blues) +
  theme_linedraw() + 
  theme(#axis.title.x = element_text(size = 18, face = "bold"),
    # axis.title = element_text(size = 12),
    axis.title.y = element_text(face = "bold"),
    # axis.text.x = element_text(size = 12),
    #axis.text.y = element_text(size = 12),
    title = element_text( face = "bold"),
    legend.title = element_text(size = 14),
    legend.key.size = (unit(1, 'in')),
    legend.text = element_text(size = 14)
  ) + 
  scale_x_continuous( breaks = c(2016, 2017, 2018)) +
  scale_y_continuous(label = scales::percent ) + 
  labs(x = "",
       y = "Proportion of Turtles",
       title = "FP Visiblity in Bimini Overall 2016-2018" )

stack_propBIM

# plotting counts of Bonefish 
fp_visibility2 <- CRCDBFH %>%
  group_by(Year,Fibropapilloma.Visible) %>%
  summarise(fp_visibility = length((Fibropapilloma.Visible)))

fp_year_summ2<- CRCDBFH %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)

blues <- c("#d1d1d1", "#09b7e3")


stack_propBFH <- ggplot(fp_year_summ2, aes(Year, prop, fill = Fibropapilloma.Visible)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual("FP Visible", values = blues) +
  theme_linedraw() + 
  theme(#axis.title.x = element_text(size = 18, face = "bold"),
    # axis.title = element_text(size = 12),
    axis.title.y = element_text(face = "bold"),
    # axis.text.x = element_text(size = 12),
    #axis.text.y = element_text(size = 12),
    title = element_text( face = "bold"),
    legend.title = element_text(size = 14),
    legend.key.size = (unit(1, 'in')),
    legend.text = element_text(size = 14)
  ) + 
  scale_x_continuous( breaks = c(2016, 2017, 2018)) +
  scale_y_continuous(label = scales::percent ) + 
  labs(x = "",
       y = "Proportion of Turtles",
       title = "FP Visiblity in Bonefish Hole 2016-2018" )

stack_propBFH


# SCL Plot CR
histo_SCLCR <- ggplot(data = CRCDCR, aes(SCL.Standard.cm, fill = Fibropapilloma.Visible)) +
  geom_histogram(bins = 10, show.legend = FALSE) +
  scale_fill_manual("FP Visible", values = blues) +
  scale_x_continuous(breaks = scales :: pretty_breaks(n = 10)) +
  theme_linedraw() + 
  theme(title = element_text(face = "bold")) + 
  labs(x = "Standard Carapice Length",
       y = "Count of Turtles",
       title = "FP Visibility by SCL in Crystal River")

histo_SCLCR

#SCL Plot Bimini
histo_SCLBIM <- ggplot(data = CRCDBIM, aes(SCL.Standard.cm, fill = Fibropapilloma.Visible)) +
  geom_histogram(bins = 10, show.legend = FALSE) +
  scale_fill_manual("FP Visible", values = blues) +
  scale_x_continuous(breaks = scales :: pretty_breaks(n = 10)) +
  theme_linedraw() + 
  theme(title = element_text(face = "bold")) + 
  labs(x = "Standard Carapice Length",
       y = "Count of Turtles",
       title = "FP Visibility by SCL in Bimini")

histo_SCLBIM





### export
stack_propCR
stack_propBIM
stack_propBFH
maplgBIM
maplgCR
histo_SCLBIM
histo_SCLCR



