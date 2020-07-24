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
lci_onekgwood = 2.002
weight_oneunit = 70.75
lci_oneunit = lci_onekgwood*weight_oneunit
consumption_per_buurt$netimpact = (consumption_per_buurt$Consumptionpercapita)*(lci_oneunit)

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = netimpact), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP due to furniture per capita (in kg CO'[2]*' equivalent)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=12,face="bold",hjust=0))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)


display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
