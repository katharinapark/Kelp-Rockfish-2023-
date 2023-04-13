################################################################################
##  R script to look at PISCO data for kelp rockfish
##  Katharina
##  Combination of script from Emma Saas kelp rockfish and 
##  Melissa Monk's gopher rockfish assessment 
## 
################################################################################
#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

#do once: uncomment the next two lines, then comment out the next two lines
#install.packages("remotes")
#remotes::install_github("pfmc-assessments/nwfscSurvey")

#load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(nwfscSurvey)
library(here)

#set working directory
#by having an R project you can use the here() to set a working directory
#and not have to change it depending on who's using the script

#Set the working directory
dir <- file.path(here(),"pisco", "data")
setwd(dir)

load("species.transects.collapsed.RData")
pisco <- species.transects.collapsed

lon_range <- c(-121,-117)
lat_range <- c(33.2, 35)


ggplot() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  geom_point(data = pisco, aes(x = longitude, y = latitude, color = CA_MPA_Name_Short,
                               shape = site_status),show.legend=FALSE ,
             alpha = 1, size = 2) 

#### maps of count by year - needs work
ggplot() +
  geom_point(data = pisco, aes(x = longitude, y = latitude, size = number.fish), 
             alpha = .5) +
  scale_size_binned("Count", breaks = c(1, 5, 10, 25,  55)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  facet_wrap("campus", ncol = 4, nrow = 5)
