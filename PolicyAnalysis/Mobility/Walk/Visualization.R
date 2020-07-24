library(rgdal)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(maptools)
library(rosm)
library(prettymapr)
library(rgeos)
library(ggmap)
library(viridis)

buurts = readOGR("buurten.shp")
buurt_df = data.frame(buurts)
buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:26978"))
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('walk.1.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Distance.travelled), size = 0.25) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = 'Total distance travelled by walking per day(in km)',
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=15,face="bold",hjust=0.75))  
#write.csv(clothing_consumption_per_buurt,'inspect.csv',row.names=F)

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Total.Consumption), color = "black", size = 0.25) + 
  coord_fixed() + scale_fill_distiller(palette="YlOrRd") +
  labs(title = "Total Clothes Consumption",
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=15,face="bold",hjust=0.75))  

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
