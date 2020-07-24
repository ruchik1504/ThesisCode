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
library(maps)

buurts = readOGR("buurten.shp")
buurt_df = data.frame(buurts)
#buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_codes = fortify(buurts, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('Foodconsumedperbuurt.csv')
#types = c("01. Potatoes and other tubers","02. Vegetables","03. Legumes")
#types = c(types,"04. Fruits","05. Dairy products and substitutes")
#types = c(types,"06. Cereals and cereal products","07. Meat,meat products and substitutes","08. Fish","09. Eggs and egg products","10. Fats and oils","11. Sugar and confectionery","12. Cakes and sweet biscuits","13. Non-alcoholic beverages","14. Alcoholic beverages","15. Sauces and seasonings","16. Soups and Stocks","17. Miscellaneous","18. Savoury snacks")
lci_sugar = mean(c(0.5375623,1.20875))           #sugarbeet and sugarcane        

lci_value = c(lci_sugar)

types = c("11. Sugar and confectionery")
count = 1
for (type in types){
  if(count == 1){
    df = consumption_data[consumption_data$food == type,]
    df$foodpercapita = df$Amount.per.day/df$Population
    fin_df = df[c("Name","BUURTCODE")]
    fin_df$env_impact = (df$foodpercapita)*lci_value[count]
    count = count  + 1
  }else{
    df = consumption_data[consumption_data$food == type,]
    df$foodpercapita = df$Amount.per.day/df$Population
    fin_df$env_impact = fin_df$env_impact + ((df$foodpercapita)*lci_value[count])
    count = count + 1
  }
}
df = fin_df  
consumption_per_buurt = (merge(buurt_codes,df, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]
consumption_per_buurt$env_impact = (consumption_per_buurt$env_impact)*(365/1000)

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = env_impact), color = "NA", size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() + 
  labs(title = expression('GWP per capita (in kg CO'[2]*' equivalent) due to Sugar consumption'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=14,face="bold",hjust=0))+
  theme(legend.title = element_blank())

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
