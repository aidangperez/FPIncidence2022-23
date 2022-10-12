#######################################
## Necessary Packages for New System ##
#######################################

install.packages(c (
  "dplyr",
  "ggplot2",
  "ggthemes",
  "ggpubr",
  "gridExtra",
  "tidyverse",
  "lubridate",
  "janitor",
  "reshape",
  "sf",
  "mapview",
  "cowplot",
  "rcartocolor",
  "magick",
  "git2r"
))

devtools::install_github("ropenscilabs/rnaturalearth")
devtools::install_github("ropenscilabs/rnaturalearthdata")

install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", 
                 type = "source")
install.packages("rnaturalearth")
