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
consumption_data = read.csv('ConsumptionBuurt.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = Consumptionpercapita ),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Units of Furniture consumed per capita'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=12,face="bold",hjust=0.75))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
