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
library(ggpubr)
buurts = readOGR("buurten.shp")
buurt_df = data.frame(buurts)
#buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_transform = buurts
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('lci_data_net.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
consumption_per_buurt$BoP = consumption_per_buurt$Clothes + consumption_per_buurt$Furniture + consumption_per_buurt$Paper 
#lci_onekgwood = 2.002
#weight_oneunit = 70.75
#lci_oneunit = lci_onekgwood*weight_oneunit
#consumption_per_buurt$netimpact = (consumption_per_buurt$Consumptionpercapita)*(lci_oneunit)

food = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Food), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Food GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 


  
water = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Water), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Water GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

green_elec = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Green_electricity), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Green electricity GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

nongreen_elec = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = nongreen_electricity), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Non-green electricity GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

gas = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Gas), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Gas GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

car = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Car), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Car use GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

train = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Train), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Train use GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

bus_tram = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = bustram), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Bus/tram use GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 


BoP = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = BoP), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('BoP use GWP per capita (in kg CO'[2]*' eq/year)'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 


ggarrange(
  ggarrange(food,water,BoP, ncol = 3, labels = c("A", "B","C")),                # First row with line plot
  # Second row with box and dot plots
  ggarrange(green_elec,nongreen_elec,gas, ncol = 3, labels = c("D","E","F")),
  ggarrange(car,train,bus_tram, ncol = 3, labels = c("G","H","I")),
  nrow = 3      # Label of the line plot
)


clothes = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Clothes), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of clothes purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

detergent = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Detergent), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of detergent products purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
footwear = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Footwear), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of footwear purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
furniture = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Furniture), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of furniture products purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
paper = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Paper), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of paper products purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
Personalcare = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Personalcare), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Number of personal care products purchased per capita/year'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 



ggarrange(
  ggarrange(clothes,detergent,footwear, ncol = 3, labels = c("A", "B","C")),                # First row with line plot
  # Second row with box and dot plots
  ggarrange(furniture,paper,Personalcare, ncol = 3, labels = c("D","E","F")),
  nrow = 2      # Label of the line plot
)
