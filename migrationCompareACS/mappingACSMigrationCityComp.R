library(ggmap)
library(ggplot2)
library(maps)
### Testing out map visualization for city migration comparison ###
### Testing out map visualization for city migration comparison ###
### Testing out map visualization for city migration comparison ###
states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="white" )
p<- p +  
  geom_point(data=MigrationCityComp, 
             mapping=aes(x=Longitude, y=Latitude, size=WmeanDistance,color="red"))
p

map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

ggmap(map) +   geom_point(data=MigrationCityComp, 
                          mapping=aes(x=Longitude, y=Latitude, size=(WmeanDistance*4),color="orange"))

