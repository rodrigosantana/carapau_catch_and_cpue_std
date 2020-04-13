########################################################################
## Description: Time-series CPUE standardization for Carapau in
## Brazilian Southeast coast...
##
## Maintainer: DatenKraft Data Science
## Author: Rodrigo Sant'Ana
## Created: Mon Apr 13 17:48:37 2020 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Loading R packages...

######@> Package list...
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(patchwork)
library(extrafont)
library(tmap)
library(tmaptools)
library(rgeos)
library(rgdal)
library(sf)
library(viridis)
library(INLA)

########################################################################
######@> Setup R...

######@> Loading and registering new fonts...
font_import()
loadfonts(device = "pdf")

######@> DatenKraft theme...
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
my_theme <- function(base_size = 16, base_family = "Trebuchet MS") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_blank(),
              axis.line = element_line(arrow = seta),
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              strip.background = element_rect(fill = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

######@> Standardizing the number of decimal places...
options(scipen = 10)

########################################################################
######@> Loading datasets...

######@> Cerco SC...
db01 <- read.table("data/input_cerco_carapau_CPUE_SC.csv",
                   header = TRUE, sep = ";", dec = ",",
                   fileEncoding = "ISO-8859-1")



########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
