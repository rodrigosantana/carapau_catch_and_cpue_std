########################################################################
## Description: Catch analyses of Caranx crysos (Carapau)...
##
## Maintainer: WWF / DatenKraft data science
## Author: Rodrigo Sant'Ana
## Created: seg abr  6 13:38:31 2020 (-0300)
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

######@> Rebuilding dataset...
db01 <-
    read_excel("data/FINAL_OriginalBrazil_27April_2015_Brazil_1950_2010.xlsx",
               sheet = "Database")
db01 <- data.frame(db01)

######@> SP dataset...
db02 <- read.table("data/input_tendencias_pescarias_carapau_SP.csv",
                   header = TRUE, sep = ";", dec = ".",
                   fileEncoding = "ISO-8859-1")

######@> SC dataset - Industrial...
db03 <- read.table("data/relatorio30_1584886140_industrial.csv",
                   header = TRUE, sep = ";", dec = ",",
                   fileEncoding = "ISO-8859-1")

######@> SC dataset - Artesanal Type 01...
db04 <- read.table("data/relatorio30_1584886230_artesanal_tipo1.csv",
                   header = TRUE, sep = ";", dec = ",",
                   fileEncoding = "ISO-8859-1")

######@> SC dataset - Artesanal Type 02...
db05 <- read.table("data/relatorio30_1584886315_artesanal_tipo2.csv",
                   header = TRUE, sep = ";", dec = ",",
                   fileEncoding = "ISO-8859-1")

######@> FishBase distribution dataset...
db06 <- read.table("data/FishBase_Caranx_crysos_distribution_ver01.csv",
                   header = TRUE, sep = ",", dec = ".")

######@> FishBase environmental dataset...
db07 <- read.table("data/FishBase_Caranx_crysos_environmental_data.csv",
                   header = TRUE, sep = ",", dec = ".")

########################################################################
######@> Cleaning and tidying datasets...

######@> Filtering only Carapau data, selecting variables and renaming
######@> variables in db01...
db01 <- db01 %>%
    filter(PortugueseCommonName == "Carapau") %>%
    select(Year, Sector, PortugueseCommonName, CatchAmount..tonnes.) %>%
    rename(year = Year, sector = Sector, spp = PortugueseCommonName,
           catch = CatchAmount..tonnes.) %>%
    as.data.frame()

######@> Renaming variables in db02 and convert catches to tons...
names(db02) <- c("year", "month", "city", "gear", "spp", "catch",
                 "trips", "productive.units")
db02 <- db02 %>%
    mutate(catch = catch/1000) %>%
    as.data.frame()

######@> Renaming variables in db03, cleaning catch structure, convert
######@> catches to tons and creating a new variable called sector...
names(db03) <- c("year", "month", "gear", "spp", "catch", "trips")
db03 <- db03 %>%
    mutate(catch = gsub("\\.", "", catch)) %>%
    mutate(catch = as.numeric(gsub(",", ".", catch)) / 1000) %>%
    mutate(sector = "Industrial") %>%
    as.data.frame()

######@> Renaming variables in db04, cleaning catch structure, convert
######@> catches to tons and creating a new variable called sector...
names(db04) <- c("year", "month", "gear", "spp", "catch", "trips")
db04 <- db04 %>%
    mutate(catch = gsub("\\.", "", catch)) %>%
    mutate(catch = as.numeric(gsub(",", ".", catch)) / 1000) %>%
    mutate(trips = as.numeric(gsub("\\.", "", trips))) %>%
    mutate(sector = "Artisanal - Type 01") %>%
    as.data.frame()

######@> Renaming variables in db05, cleaning catch structure, convert
######@> catches to tons and creating a new variable called sector...
names(db05) <- c("year", "month", "gear", "spp", "catch", "trips")
db05 <- db05 %>%
    mutate(catch = gsub("\\.", "", catch)) %>%
    mutate(catch = as.numeric(gsub(",", ".", catch)) / 1000) %>%
    mutate(sector = "Artisanal - Type 02") %>%
    as.data.frame()

######@> Renaming variables in db06...
names(db06) <- c("genus", "species", "lat", "lon", "quad.code",
                 "probability")

######@> Renaming variables in db07...
names(db07) <- c("genus", "species", "lat", "lon", "quad.code", "depth",
                 "surface.temp", "bottom.temp", "surface.sal",
                 "bottom.sal", "primary.prod", "ice.concentration",
                 "bottom.oxygen", "dist.land")

########################################################################
######@> Exploratory Data Analysis...

######@> Spatial distribution of Carapau - FishBase data...

#####@> Loading a base map from R...
data(World)

####@> converting CRS...
World <- st_transform(World, crs = 4326)

#####@> Transforming db06 into a spatial object...
db06.sp <- st_as_sf(db06, coords = c("lon", "lat"))

####@> converting CRS...
st_crs(db06.sp) <- st_crs(World)

#####@> Probability distribution map - Total...
p00 <- tm_shape(World, xlim = c(-100, 40), ylim = c(-30, 50)) +
    tm_polygons() +
    tm_grid(x = seq(-100, 40, by = 20), y = seq(-30, 50, by = 10)) +
    tm_shape(db06.sp) +
    tm_dots(col = "probability", palette = "plasma", size = 0.05,
            title = "Probability", shape = 15, alpha = 0.5) +
    tm_xlab(text = "Longitude", size = 1) +
    tm_ylab(text = "Latitude", size = 1) +
    tm_layout(scale = 2, legend.position = c("left", "top"),
              bg.color = "white", legend.bg.color = "white",
              legend.frame = "black") +
    tm_scale_bar(width = 0.15, position = c("left", "bottom")) +
    tm_credits("Source: FishBase")
p00

#####@> Watching only to Southern area of the West Atlantic
#####@> distribution...
tmp <- st_crop(db06.sp, xmin = -50, xmax = -30, ymin = -28, ymax = -18)
p01 <- tm_shape(World, xlim = c(-50, -30), ylim = c(-28, -18)) +
    tm_polygons() +
    tm_grid(x = seq(-50, -30, by = 5), y = seq(-28, -18, by = 2)) +
    tm_shape(tmp) +
    tm_dots(col = "probability", palette = "plasma", size = 1,
            title = "Probability", shape = 16) +
    tm_xlab(text = "Longitude", size = 1) +
    tm_ylab(text = "Latitude", size = 1) +
    tm_layout(scale = 2, legend.position = c("left", "top"),
              bg.color = "white", legend.bg.color = "white",
              legend.frame = "black") +
    tm_scale_bar(width = 0.15, position = c("left", "bottom")) +
    tm_credits("Source: FishBase")
p01

######@> Working with Rebuilding fisheries catches dataset...

#####@> Watching total catches by year and sector...
p03 <- db01 %>%
    group_by(year, sector) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = year, y = catch)) +
    geom_line() +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Year", y = "Total catch (t)",
         caption = "Source: Rebuilding fish catches") +
    facet_wrap(~ sector, scales = "free_x") +
    scale_x_continuous(limits = c(1978, 2007)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-5, 1250)) +
    my_theme()
p03

######@> Working with SP catches dataset...

#####@> Watching total catches by year...
p04 <- db02 %>%
    group_by(year) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = year, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 22, bs = "cc")) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Year", y = "Total catch (t)",
         caption = "Source: PMAP-SP / Instituto de Pesca de São Paulo") +
    scale_x_continuous(limits = c(1998, 2019), breaks = seq(1998, 2020, 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
    my_theme()
p04

#####@> Watching total catches by month...
p05 <- db02 %>%
    group_by(month) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = month, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x)) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Month", y = "Total catch (t)",
         caption = "Source: PMAP-SP / Instituto de Pesca de São Paulo") +
    scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1300)) +
    my_theme()
p05

#####@> Watching total catches by season...
p06 <- db02 %>%
    mutate(season = ifelse(month <= 3, 1,
                    ifelse(month <= 6, 2,
                    ifelse(month <= 9, 3, 4)))) %>%
    group_by(season) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = season, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Season", y = "Total catch (t)",
         caption = "Source: PMAP-SP / Instituto de Pesca de São Paulo") +
    scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
    my_theme()
p06

######@> Working with SC catches dataset...

#####@> Unifying the SC datasets...
sc <- gtools::smartbind(db03, db04, db05)
row.names(sc) <- 1:nrow(sc)

#####@> Watching total catches by year...
p07 <- sc %>%
    group_by(year) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = year, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 18, bs = "cc")) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Year", y = "Total catch (t)",
         caption = "Source: PMAP-SC / Universidade do Vale do Itajaí") +
    scale_x_continuous(limits = c(2000, 2019), breaks = seq(2000, 2020, 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-30, 1000)) +
    my_theme()
p07

#####@> Watching total catches by month...
p08 <- sc %>%
    group_by(month) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = month, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x)) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Month", y = "Total catch (t)",
         caption = "Source: PMAP-SC / Universidade do Vale do Itajaí") +
    scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-200, 1300)) +
    my_theme()
p08

#####@> Watching total catches by season...
p09 <- sc %>%
    mutate(season = ifelse(month <= 3, 1,
                    ifelse(month <= 6, 2,
                    ifelse(month <= 9, 3, 4)))) %>%
    group_by(season) %>%
    summarise(catch = sum(catch, na.rm = TRUE)) %>%
    ggplot(data = ., aes(x = season, y = catch)) +
    geom_line() +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
    geom_point(pch = 21, fill = "white", colour = "black", size = 5) +
    labs(x = "Season", y = "Total catch (t)",
         caption = "Source: PMAP-SC / Universidade do Vale do Itajaí") +
    scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-300, 3000)) +
    my_theme()
p09

######@> Creating a mosaic plots to save...

#####@> Creating a new folder to save figs...
dir.create("Figs")

#####@> Maping distribution...
png("Figs/Carapau_spatial_distribution.png", units = "cm", res = 200,
    width = 35, height = 35)
print(p00)
dev.off()

png("Figs/Zoom_Carapau_spatial_distribution.png", units = "cm", res = 200,
    width = 35, height = 35)
print(p01)
dev.off()

#####@> Production data...
png("Figs/Catches_dataset_comparison.png", units = "cm", res = 200,
    width = 35, height = 30)
p03 /
    (p04 | p07) /
    (p05 | p08) /
    (p06 | p09)
dev.off()

########################################################################
######@> Concatenating datasets ...



########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-SA 4.0)
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
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
