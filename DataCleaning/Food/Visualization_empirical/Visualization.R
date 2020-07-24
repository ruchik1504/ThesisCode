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
consumption_data = read.csv('Foodconsumedperbuurt.csv')
types = c("01. Potatoes and other tubers","02. Vegetables","03. Legumes")
types = c(types,"04. Fruits","05. Dairy products and substitutes")
types = c(types,"06. Cereals and cereal products","07. Meat,meat products and substitutes","08. Fish","09. Eggs and egg products","10. Fats and oils","11. Sugar and confectionery","12. Cakes and sweet biscuits","13. Non-alcoholic beverages","14. Alcoholic beverages","15. Sauces and seasonings","16. Soups and Stocks","17. Miscellaneous","18. Savoury snacks")

for (type in types){
  df = consumption_data[consumption_data$food == type,]
  df$foodpercapita = df$Amount.per.day/df$Population
  consumption_per_buurt = (merge(buurt_codes,df, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
  consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
  ggplot() +
    geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = foodpercapita),color='NA',size = 0.15) + 
    coord_fixed()  + scale_fill_viridis() +
    labs(title = paste(type," consumed per capita in (g/day)"),
         subtitle = "",
         x = "", y = "") + theme_void() +
    theme(plot.title=element_text(size=15,face="bold",hjust=0.75))+
    north(consumption_per_buurt) + 
    scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
             transform = FALSE, model = "WGS84",st.size=3,height=0.01)
  ggsave(paste(type,".png"))
  
}
#write.csv(clothing_consumption_per_buurt,'inspect.csv',row.names=F)

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = Total.Consumption), color = "white", size = 0.25) + 
  coord_fixed() + scale_fill_distiller(palette="YlOrRd") +
  labs(title = "Total fruits Consumption",
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=15,face="bold",hjust=0.75))  

display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name= "GrandBudapest1")
