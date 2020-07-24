library(rgdal)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(maptools)
library(rosm)
library(prettymapr)
library(rgeos)
library(ggmap)

buurts = readOGR("buurten.shp")
buurt_df = data.frame(buurts)
#buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_transform = buurts
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('bus_tram.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
bus_lci = 0.100737
tram_lci = 0.0877700362
average_lci = 0.5*(bus_lci+tram_lci)
consumption_per_buurt$total_gwp_tramandbus = (consumption_per_buurt$tram.bus.km.per.capita)*average_lci*365

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = total_gwp_tramandbus),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP due to tram/bus ridership per capita (in kg CO'[2]*' equivalent)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=11,face="bold",hjust=0))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)
#write.csv(clothing_consumption_per_buurt,'inspect.csv',row.names=F)

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Total.Consumption), color = "white", size = 0.25) + 
  coord_fixed() + scale_fill_distiller(palette="YlOrRd") +
  labs(title = "Total Clothes Consumption",
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=15,face="bold",hjust=0.75))  

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
