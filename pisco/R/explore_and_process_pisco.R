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
species <- "SATR"
#Set the minimum size for the the species that you want to keep
#min_size <- 15
#Set the parameters for the weight-length conversion
# From Lea et al. 1999 Fish Bulletin 177
#Based on total length in mm
#W = .00000631 * TL ^ 3.14
weight_length_a <- 0.00000631
weight_length_b <- 3.14


# Read data ----
#Set the working directory
dir <- file.path(here(),"pisco", "data")
setwd(dir)

#read in all of the csv's from Avery Parsons-Field (UCSB)
#Includes biomass calculations, but exclude canopy and some additional transects
#Fish_biomass <- read.csv("MLPA_fish_biomass_density_transect_raw.csv")
luSite <- read.csv("luSite.csv") %>%
  dplyr::select(-X)
#looks to be summary data
#Fish_site_means_target <- read.csv("MLPA_fish_site_means_with_targeted.csv")
#Raw data through 2021
Kelpforest_fish <- read.csv("MLPA_kelpforest_fish.csv")

#Summary tables ---------------------------------------------------------

#Take the entire kelp forest table
targetSpp <- Kelpforest_fish %>%
  filter(classcode == species)

# Look at how many of the fish by campus level and zone
total_fish <- targetSpp %>%
  group_by(campus, level, zone) %>%
  summarise(total_fish = sum(count)) %>%
  pivot_wider(names_from = zone, values_from = total_fish)
total_fish

#kelp rockfish are in the bottom, canopy and mid
#total of 39,389 kelp rockfish
#level    sm
#1 BOT   20998
#2 CAN   14114
#3 CNMD    159
#4 MID    4118
#Transects ---------------------------------------------------------------------
#Look at the number of transects

Transects <- Kelpforest_fish %>%
  dplyr::select(campus, method, survey_year, year, month, day, site, zone, 
                level, transect) %>%
  unique()

# Look at how many of the fish by campus level and zone
total_transects <- Transects %>%
  group_by(campus, level, zone) %>%
  tally() %>%
  pivot_wider(names_from = zone, values_from = n)
total_transects

# Filter 1 ----------------------------------------------------------------
#remove zone == DEEP and levels == CNMD and campus==HSU
fish_dat <- Kelpforest_fish %>%
  filter(!zone == 'DEEP',
         !campus == 'HSU',
         !level == 'CNMD')


#-------------------------------------------------------------------------------
Transects <- fish_dat %>%
  dplyr::select(campus, method, year, month, day, site, zone, 
                level, transect) %>%
  unique()
#Check to see if there's always a mid and a bottom
midBot_pairs <- Transects %>%
  filter(level %in% c('BOT', 'MID')) %>%
  dplyr::select(campus, year, month, day, site, zone, transect, level) %>%
  group_by(campus, year, month, day, site, zone, transect, level) %>%
  tally() %>%
  pivot_wider(names_from = level, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(pairT = BOT + MID)
  
#how many transects only have a bottom or mid
oneLevel <- midBot_pairs %>% filter(pairT == 1)
#look to see if these are evenly distributed
summary(as.factor(oneLevel$year))
summary(as.factor(oneLevel$campus))
summary(as.factor(oneLevel$site))

#how many transects have both
bothLevels <- midBot_pairs %>% filter(pairT == 2)


# Species data---------------------------------------------------------------------

#collapse the number of fish by transect
species.data <- fish_dat %>%
  filter(classcode == species) %>%
  group_by(campus, year, month, day, site, zone, level, transect) %>%
  summarise(total_fish = sum(count)) 

#Merge to the transect data
species.transects <- left_join(Transects, species.data) %>%
  mutate(total_fish = ifelse(is.na(total_fish), 0, total_fish)) #convert NA to 0

#add lusite info
species.transects <- left_join(species.transects, luSite)

#look at summary species data before collapsing transects
#worry about effort here
#summary pivot tables
level_zone <- species.transects %>% 
  filter(total_fish >0) %>% 
  group_by(level, zone) %>%
  tally() %>%
  pivot_wider(names_from = zone, values_from = n)
level_zone

#look at just campus 
species.transects %>% group_by(campus) %>% summarise(all_fish = sum(total_fish))
#no fish in HSU samples

#year and campus
year_campus <- species.transects %>% filter(total_fish >0) %>% 
  group_by(year, campus) %>%
  tally() %>%
  pivot_wider(names_from = campus, values_from = n)
year_campus


# Filter 2 ---------------------------------------------------------------------

#UCSB very few samples from 1999-2002
#UCSC ramped up from 1999-2001

species.transects <- species.transects %>%
  filter(year > 2001) %>%
  filter(!(campus == 'UCSB' & year < 2003)) %>%
  filter(!campus == 'VRG')

fish_dat <- fish_dat %>%
  filter(classcode == species) %>%
  filter(year > 2001) %>%
  filter(!(campus == 'UCSB' & year < 2003)) %>%
  filter(!campus == 'VRG')
#----------------------------------------------------------------

#look at month and year
 year_month <- species.transects %>% filter(total_fish >0) %>% 
  mutate_at(vars(month), as.factor) %>%
  group_by(year, month) %>%
  tally() %>%
  pivot_wider(names_from = month, values_from = n)
year_month
#tally by year month to remove months rarely seeing kelp rockfish
month_tally <- species.transects %>% filter(total_fish >0) %>% 
  mutate_at(vars(month), as.factor) %>%
  group_by(month) %>%
  tally()
month_tally

month_tally2 <- species.transects %>% 
  mutate_at(vars(month), as.factor) %>%
  group_by(month) %>%
  tally()
month_tally2

# Filter 3 ---------------------------------------------------------------------
#keep months between 7 and 11
species.transects <- species.transects %>%
  filter(month %in% (6:11))

fish_dat <- fish_dat %>%
  filter(month %in% (6:11))

#-------------------------------------------------------------------------------


#look at general summaries
summary(as.factor(species.transects$month))
summary(as.factor(species.transects$year))
summary(as.factor(species.transects$zone))
summary(as.factor(species.transects$site))
summary(as.factor(species.transects$vis)) #lots of NA's
summary(as.factor(species.transects$surge))  #lots of blanks
summary(as.factor(species.transects$depth)) #lots of NA's


#look at the sites to see where the target was caught at least once
site_Target <- species.transects %>%
  filter(total_fish >0) %>%
  group_by(site) %>%
  tally()
#View(site_Target)


#Filter 4 ----------------------------------------------------------------------
#keep only sites that observed the target at least once

species.transects <- species.transects %>%
  filter(site %in% site_Target$site)

fish_dat <- fish_dat %>%
  filter(site %in% site_Target$site)

# Filter 5----------------------------------------------------------------------

#get the number of years a site was sampled
site_year_sampled <- species.transects %>%
  group_by(site) %>%
  summarise(n_years = n_distinct(year))

#in sample 23 years is the max - keep sites for now sampled in at least 6 years
site_year_sampled_keep <- site_year_sampled %>%
  filter(n_years>5) # can change later

species.transects <- species.transects %>%
  filter(site %in% site_year_sampled_keep$site)

fish_dat <- fish_dat %>%
  filter(site %in% site_year_sampled_keep$site)

#-------------------------------------------------------------------------------
#Look at the length distribution by level 

#inner join lengths with transect info
targetLengths <- left_join(fish_dat, species.transects)

targetLengths <- targetLengths  %>%
  dplyr::select(fish_tl, count, campus, year, count, zone, level, site_status, 
                site)
#uncount for plotting
targetLengths <- uncount(targetLengths, count)

length_counts <- targetLengths %>%
  group_by(year, campus) %>%
  tally() %>%
  pivot_wider(names_from = campus, values_from = n)
write.csv(length_counts, "length_counts.csv")


ggplot(targetLengths, aes(x = fish_tl, colour = level, fill = level)) +
  geom_density(alpha = .5) +
  xlab("Fish total length") + ylab("Density") +
  scale_color_viridis_d() + scale_fill_viridis_d()
ggsave(file = file.path(getwd(), "length_by_level.png"), width = 7, height = 7)

#-------------------------------------------------------------------------------
#summary info of length by level
targetLengths %>%
  group_by(level) %>%
  summarise(x = quantile(fish_tl, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))
#canopy fish are definitely very small 4-5 cm on average


#plot length data
#remove VRG 2019 and canopy
ggplot(targetLengths %>% filter(!(campus=='VRG' & year == 2019)) %>% 
         filter(!level == 'CAN'),
       aes(x = fish_tl, colour = campus, fill = campus)) +
  geom_density(alpha = .5) +
  facet_wrap(~year) +
  xlab("Fish total length") + ylab("Density") +
  scale_color_viridis_d() + scale_fill_viridis_d()
ggsave(file = file.path(getwd(), "fish_length_by_year.png"), width = 7, height = 7)


ggplot(targetLengths, aes(x = fish_tl, colour = zone, fill = zone)) +
  geom_density(alpha = .5) +
  xlab("Fish total length") + ylab("Density") +
  scale_color_viridis_d() + scale_fill_viridis_d()
ggsave(file = file.path(getwd(), "length_by_zone.png"), width = 7, height = 7)
            









#frog
#melissa stopped here 4/24/23 7am

#-------------------------------------------------------------------------------
#now collapse transects sampled at the same level, site_side and day
#or else keep them separate and somehow ID them as replicates
species.transects.collapsed <- species.transects %>%
  group_by(year, month, day, site, campus, zone, 
           level, CA_MPA_Name_Short, latitude, longitude,
           site_designation, site_status) %>%
  summarise(number.transects = n(),
            number.fish = sum(total_fish)) %>%
  mutate(cpue = number.fish/number.transects)
#assuming that each transect is equal - check if bottom and mid are the same volume
save(species.transects.collapsed, file = "species.transects.collapsed.RData")

#super noisy and messy
ggplot(species.transects.collapsed %>% 
         group_by(year, site, campus) %>% 
         summarise(average_cpue = mean(cpue))) +
  geom_line(aes(x = year, y = average_cpue, colour = site),
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
