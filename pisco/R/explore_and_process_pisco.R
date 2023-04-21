################################################################################
##  R script to look at PISCO data for kelp rockfish
##  Katharina Park
##  Combination of script from Emma Saas kelp rockfish and 
##  Melissa Monk's gopher rockfish assessment 
## 
################################################################################
#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

#load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(here)
#set working directory
#by having an R project you can use the here() to set a working directory
#and not have to change it depending on who's using the script


#Set the species you want to work on
species <- "SMEL"
#Set the minimum size for the the species that you want to keep
min_size <- 15
#Set the parameters for the weight-length conversion
# From Lea et al. 1999 Fish Bulletin 177
#Based on total length in mm
#W = .00000631 * TL ^ 3.14
weight_length_a <- 0.00000631
weight_length_b <- 3.14

#Set the working directory
dir <- file.path(here(),"pisco", "data")
setwd(dir)

#read in all of the csv's from Avery Parsons-Field (UCSB)
#Includes biomass calculations, but exclude canopy and some additional transects
#Fish_biomass <- read.csv("MLPA_fish_biomass_density_transect_raw.csv")
luSite <- read.csv("luSite.csv")
#looks to be summary data
#Fish_site_means_target <- read.csv("MLPA_fish_site_means_with_targeted.csv")
#Raw data through 2021
Kelpforest_fish <- read.csv("MLPA_kelpforest_fish.csv")


#####





#Kelp rockfish summary ---------------------------------------------------------

kelprf <- Kelpforest_fish %>%
  filter(classcode == species)

kelprf %>%
  group_by(campus, level, zone) %>%
  summarise(total_fish = sum(count)) %>%
  pivot_wider(names_from = zone, values_from = total_fish)

#sum bottom and mid



#Transects ---------------------------------------------------------------------
#Look at the number of transects

Transects <- Kelpforest_fish %>%
  dplyr::select(campus, method, survey_year, year, month, day, site, zone, 
                level, transect, depth, vis, temp, surge, pctcnpy, 
                site_name_old) %>%
  unique()







#-------------------------------------------------------------------------------


#collapse the number of fish by transect
species.data <- FishLengths %>%
  dplyr::select(TransectID, count) %>% #later on convert to biomass here
  group_by(TransectID) %>%
  summarise(total_fish = sum(count)) 

#Merge to the transect data
species.transects <- left_join(Transects, species.data) %>%
  mutate(total_fish = ifelse(is.na(total_fish), 0, total_fish)) #convert NA to 0

pinniger#merge in the sitelocation data
species.transects <- left_join(species.transects, luSite)

#look at summary species data before collapsing transects so you don't have to 
#worry about effort here
#summary pivot tables
level_zone <-species.transects %>% filter(total_fish >0) %>% 
  group_by(level, zone) %>%
  tally() %>%
  pivot_wider(names_from = zone, values_from = n)
View(level_zone)

#not really any kelp rockfish in the canopy mid (CNMD) to remove that
#not many samples or fish in the keep - remove that 
#investigate the MID samples further - not many
species.transects <- species.transects %>%
  filter(!level == 'CNMD',
         !zone == "DEEP")

#look at just campus 
species.transects %>% group_by(campus) %>% summarise(all_fish = sum(total_fish))
#no fish in HSU samples

#year and campus
year_campus <- species.transects %>% filter(total_fish >0) %>% 
  group_by(year, campus) %>%
  tally() %>%
  pivot_wider(names_from = campus, values_from = n)
View(year_campus)
#not really any samples from Vantuna VRG so remove
#UCSB very few samples from 1999-2002
#UCSC ramped up from 1999-2001
#keep just UCSC and UCSB for now 
species.transects <- species.transects %>%
  filter(!campus %in% c("VRG", "HSU"))


#look at month and year
 year_month <- species.transects %>% filter(total_fish >0) %>% 
  mutate_at(vars(month), as.factor) %>%
  group_by(year, month) %>%
  tally() %>%
  pivot_wider(names_from = month, values_from = n)
View(year_month)
#tally by year month to remove months rarely seeing kelp rockfish
month_tally <- species.transects %>% filter(total_fish >0) %>% 
  mutate_at(vars(month), as.factor) %>%
  group_by(month) %>%
  tally()

#keep months between 7 and 12 
species.transects <- species.transects %>%
  filter(month >6)

#look at general summaries
summary(as.factor(species.transects$month))
summary(as.factor(species.transects$year))  #1 NA
summary(as.factor(species.transects$day))
summary(as.factor(species.transects$zone))
summary(as.factor(species.transects$site_side))
summary(as.factor(species.transects$vis)) #lots of NA's
summary(as.factor(species.transects$surge))  #lots of blanks
summary(as.factor(species.transects$depth)) #lots of NA's



#look at the sites by year
site_year <- species.transects %>%
  group_by(site_side, campus, year) %>%
  tally() %>%
  pivot_wider(names_from = year, values_from = n)
View(site_year)

#get the number of years a site was sampled
site_year_sampled <- species.transects %>%
  group_by(site_side) %>%
  summarise(n_years = n_distinct(year))

#in sample 23 years is the max - keep sites for now sampled in at least 10 years
site_year_sampled_keep <- site_year_sampled %>%
  filter(n_years>9)
species.transects <- species.transects %>%
  filter(site_side %in% site_year_sampled_keep$site_side)


#now collapse transects sampled at the same level, site_side and day
#or else keep them separate and somehow ID them as replicates
species.transects.collapsed <- species.transects %>%
  group_by(year, month, day, site_side, campus, zone, 
           level, CA_MPA_Name_Short, latitude, longitude,
           site_designation, site_status) %>%
  summarise(number.transects = n(),
            number.fish = sum(total_fish)) %>%
  mutate(cpue = number.fish/number.transects)
#assuming that each transect is equal - check if bottom and mid are the same volume
save(species.transects.collapsed, file = "species.transects.collapsed.RData")

#super noisy and messy
ggplot(species.transects.collapsed %>% 
         group_by(year, site_side, campus) %>% 
         summarise(average_cpue = mean(cpue))) +
  geom_line(aes(x = year, y = average_cpue, colour = site_side),
            show.legend = FALSE) +
  xlab("Year") + ylab("Average CPUE") +
  facet_wrap(~campus)

#plot average cpue by mpa and designation
ggplot(species.transects.collapsed %>% 
         group_by(year, CA_MPA_Name_Short, site_status, campus) %>% 
         summarise(average_cpue = mean(cpue))) +
  geom_line(aes(x = year, y = average_cpue, colour = CA_MPA_Name_Short),
            show.legend = FALSE) +
  xlab("Year") + ylab("Average CPUE") +
  facet_wrap(~campus+site_status)


#plot average cpue by mpa and designation
ggplot(species.transects.collapsed %>% 
         group_by(year, site_status) %>% 
         summarise(average_cpue = mean(cpue))) +
  geom_line(aes(x = year, y = average_cpue, colour = site_status)) +
  xlab("Year") + ylab("Average CPUE") 
