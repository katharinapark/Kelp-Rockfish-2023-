#summing total landings in weight by year and mode
attach(Catch_streams_Recreational_2005_2022_)
landings_sum <- aggregate(SUM_RETAINED_MT ~ RECFIN_YEAR + RECFIN_MODE_NAME, data = Catch_streams_Recreational_2005_2022_, sum)
print(landings_sum) 
#package for improving the quality and aesthetics of your graphics
library(ggplot2)
#create plot of landings sum by mode and year 
ggplot(landings_sum, aes(x = RECFIN_YEAR, y = SUM_RETAINED_MT, color = RECFIN_MODE_NAME)) + 
  geom_point() +
  labs(x= "Year", y = "Weight (metric tons)", title = "Total recreational landings in mt by year and mode") +
  scale_color_manual(values = c("red", "blue", "green", "purple"))

