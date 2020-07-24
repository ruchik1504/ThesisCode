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
consumption_data = read.csv('highcharge.csv')
consumption_per_buurt = (merge(buurt_codes,consumption_data, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
#consumption_per_buurt$BoP = consumption_per_buurt$Clothes + consumption_per_buurt$Furniture + consumption_per_buurt$Paper 
#lci_onekgwood = 2.002
#weight_oneunit = 70.75
#lci_oneunit = lci_onekgwood*weight_oneunit
#consumption_per_buurt$netimpact = (consumption_per_buurt$Consumptionpercapita)*(lci_oneunit)

low_charge = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill =GWP.reduction...Low.charge), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Low charges for parking policy'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 

medium_charge = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = GWP.reduction...Medium.charge), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Medium charges for parking policy'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
high_charge = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = GWP.reduction...high.charge), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('High charges for parking policy'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=10,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 


fig = ggarrange(
  ggarrange(low_charge,medium_charge,high_charge, ncol = 3, labels = c("", "","")),                # First row with line plot
  # Second row with box and dot plots
  nrow = 1      # Label of the line plot
)

annotate_figure(fig,
                top = text_grob(bquote("Percentage reduction in GWP per capita due to different policies"),color="black",face="bold",size=14),
                fig.lab.face = "bold"
)


error = ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Percentage.reduction.in.waste), color = "NA", size = 0) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('Percentage reduction in waste generation due to weight based policy'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.35))+
  theme(legend.title = element_blank())+north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01) 
error


