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
#buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_transform = buurts
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('final_electricity_consumption.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
#write.csv(clothing_consumption_per_buurt,'inspect.csv',row.names=F)

coal_electricity_lci = 1.00871
oil_electricity_lci = 0.7695418
natural_gas_electricity_lci = 0.64291
coal_production = 17.4  #billion kwh
oil_production = 3.9 #billion kwh
natural_gas_production = 71 #billion kwh

total_fossil = coal_production + natural_gas_production + oil_production
coal_share = coal_production/total_fossil
oil_share = oil_production/total_fossil
naturalgas_share = natural_gas_production/total_fossil

net_lci_nonrenewable = (coal_share*coal_electricity_lci) + (oil_share*oil_electricity_lci) + (naturalgas_share*natural_gas_electricity_lci)
consumption_per_buurt$net_gwp_nonrenewable = (consumption_per_buurt$nongreen.electricity)*net_lci_nonrenewable 

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = net_gwp_nonrenewable),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP due to non renewable electricity per capita (in kg CO'[2]*' equivalent)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=11,face="bold",hjust=0))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)


display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
