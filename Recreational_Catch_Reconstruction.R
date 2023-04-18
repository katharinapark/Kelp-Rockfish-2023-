################################################################
#
#
#
#
################################################################

library(ggplot2)
library(dplyr)

#df turns csv into data frame 

recdf <- read.csv(file = "Catch_Reconstruction_Recreational.csv")

#Get rid of X 

recdf$X <- NULL
recdf$X.1 <- NULL


# Read in commercial data 
commercialdf <- read.csv(file = "Catch_Reconstruction_Commercial.csv")
commercialdf$X <- NULL




#Create line chart of Rec PC Mode

recPC <- ggplot(data= recdf, aes(x=YEAR, y=CATCH.MT)) + 
  #Specifies that a line chart should be created
  geom_line() + 
  labs(title = "Kelp Rockfish Recreational Catch Reconstruction - PC Mode", x = "Year", y = "Catch in mt")
print(recPC)

#Create line chart of Rec PR Mode 

recPR <- ggplot(data= recdf, aes(x=YEAR.1, y=CATCH.MT.1)) +
  geom_line() +
  labs(title = "Kelp Rockfish Recreational Catch Reconstruction - PR Mode", x = "Year", y = "Catch in mt") 
print(recPR) 

#Combine both PC and PR modes into one chart to compare recreational landings 

ggplot(recdf, aes(x = YEAR, y = CATCH.MT, colour = as.factor(MODE))) +
  geom_line(linewidth = 2) +
  labs(title = "Kelp Rockfish Recreational Catch Reconstruction CA", x = "YEAR", y = "Catch in MT (Metric Tons)",
       color = "Mode") +
  scale_colour_viridis_d()
#  scale_color_manual(values = c("CATCH.MT" = "blue", "CATCH.MT.1" = "red"),
#                     labels = c("PC", "PR")) +
  theme_minimal()
 



#Rename variable 

colnames(commercialdf)[colnames(commercialdf) == "LANDED_WEIGHT_MTONS"] <- "Catch_mt"

#Create line chart of commercial data 

commercial <- ggplot(data= commercialdf, aes(x=LANDING_YEAR, y=Catch_mt)) + 
  geom_line() + 
  labs(title = "Kelp Rockfish Commercial Catch Reconstruction CA", x = "Year", y = "Catch in mt") +
theme_minimal ()
print(commercial)



#Create line chart displaying both recreational and commercial catches


recandcommercial <- ggplot()

# Add the recreational data lines
recandcommercial <- recandcommercial + geom_line(data = recdf, aes(x = YEAR, y = CATCH.MT, color = "Recreational_PC_mode"))
recandcommercial <- recandcommercial + geom_line(data = recdf, aes(x = YEAR, y = CATCH.MT.1, color = "Recreational_PR_mode"))

# Add the commercial data line
recandcommercial <- recandcommercial + geom_line(data = commercialdf, aes(x = LANDING_YEAR, y = Catch_mt, color = "All_commercial_modes"))

# Set color mappings
recandcommercial <- recandcommercial + scale_color_manual(values = c("Recreational_PC_mode" = "red", "Recreational_PR_mode" = "blue", "All_commercial_modes" = "green"))

# Add labels and title
recandcommercial <- recandcommercial + xlab("Year") + ylab("Catch in mt (metric tons)") + ggtitle("Recreational and Commercial Kelp Rockfish Catch Estimates by Year and Mode")

# Print the plot
print(recandcommercial)

####
commercialdf <- commercialdf %>%
  rename(YEAR = LANDING_YEAR,
         CATCH.MT = Catch_mt) %>%
  mutate(MODE = "COM")




# Combine catches ####
#combine recreational and commercial catches
catches <- rbind(recdf[, c(1,3,2)], commercialdf[,1:3])


ggplot(catches, aes(x = YEAR, y = CATCH.MT, colour = as.factor(MODE))) +
  geom_line(linewidth = .8) +
  labs(title = "Kelp Rockfish Catch Reconstruction CA", x = "YEAR", y = "Catch in MT (Metric Tons)",
       color = "Mode") +
  scale_colour_viridis_d()





#Attempt 2 
geom_line(data = recdf, aes(x = YEAR, y = CATCH.MT, color = "Recreational_PC_mode")) +
  geom_line(data = recdf, aes(x = YEAR, y = CATCH.MT.1, color = " Recreational_PR_mode")) +
  geom_line(data = commercialdf, aes(x = LANDING_YEAR, y = Catch_mt(metric_tons) , color = "All_commercial_modes")) +
  scale_color_manual(values = c(" Recreational_PC_mode" = "red", "Recreational_PR_mode" = "blue", "All_commercial_modes" = "green")) +
  xlab("Year") + 
  ylab("Catch in mt (metric tons)") + 
  ggtitle("Recreational and Commercial Kelp Rockfish Catch Estimates by Year and Mode")