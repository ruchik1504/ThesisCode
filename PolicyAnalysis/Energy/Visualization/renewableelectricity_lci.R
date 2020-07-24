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

nuclear_electricity_lci = 0.01071
wind_electricity_lci = 0.0140963
hydro_electricity_lci = 0.0040337
solar_electricity_lci = 0.050136
waste_electricity_lci = 0.00512789

hydroelectric_production = 100000  #Mwh
wind_production = 8170000          #Mwh
solar_production = 1560000         #Mwh
waste_electricity = 6612000        #Mwh
nuclear_electricity = 3750000      #Mwh
total_renewable = hydroelectric_production + wind_production + solar_production + waste_electricity + nuclear_electricity
hydroelectric_share = hydroelectric_production/total_renewable
wind_share = wind_production/total_renewable
solar_share = solar_production/total_renewable
waste_share = waste_electricity/total_renewable
nuclear_share = nuclear_electricity/total_renewable

net_lci_renewable = (hydroelectric_share*hydro_electricity_lci) + (wind_share*wind_electricity_lci) + (solar_share*solar_electricity_lci) + (nuclear_share*nuclear_electricity_lci) + (waste_share*waste_electricity_lci)
consumption_per_buurt$net_gwp_renewable = (consumption_per_buurt$green.electricity)*net_lci_renewable 

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = net_gwp_renewable),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP due to Renewable electricity per capita (in kg CO'[2]*' equivalent)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=11,face="bold",hjust=0))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
