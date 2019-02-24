
FebUS <- na.open201402
FebUS <-  FebUS[FebUS$Country_PL== "United States",]
FebUS <-  FebUS[FebUS$State_PL != "Hawaii",]

library(ggplot2)
library(maps)
library(ggmap)
#load us map data
all_states <- map_data("state")

#plot all states with ggplot

map1 <- ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),fill="light grey",color="white") +theme(axis.text = element_blank(),panel.background = element_blank())
map1

#point with NPS_Type
map.Feb1 <- map1 + geom_point(data=FebUS, aes(x=FebUS$Property.Longitude_PL, y=FebUS$Property.Latitude_PL,color=FebUS$NPS_Type),size=4)
map.Feb1

#point with Overall_Sat_H
map.Feb2 <- map1 + geom_point(data=FebUS, aes(x=FebUS$Property.Longitude_PL, y=FebUS$Property.Latitude_PL,color=FebUS$Overall_Sat_H),size=4)
map.Feb2

Contact GitHub API Training Shop Blog About
© 2017 GitHub, Inc. Terms Privacy Security Status Help