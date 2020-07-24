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
library(ggsn)

buurts = readOGR("buurten.shp")
buurt_df = data.frame(buurts)
#buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_transform = buurts
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('water_consumption.csv')
consumption_data$percapitaconsumption = consumption_data$Water.consumption/consumption_data$Household.size
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = percapitaconsumption),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Total Water consumed per capita (in m' ^3*'/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=12,face="bold",hjust=0.75))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)

water_lci = read.csv('water_lci.csv')
method = 'IPCC 2013'
type = 'climate change'
parameter = 'GWP 100a'
water_lci = water_lci[water_lci$Method==method,]
water_lci = water_lci[water_lci$Type==type,]
water_lci = water_lci[water_lci$Parameter==parameter,]
consumption_per_buurt$GWP_water = (consumption_per_buurt$percapitaconsumption)*water_lci$Score[1]*1000

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = GWP.per.capita),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP due to water consumption per capita (in kg CO'[2]*' equivalent)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=12,face="bold",hjust=0.75))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
