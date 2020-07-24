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
buurt_transform = spTransform(buurts, CRS("+proj=longlat +init=epsg:4326"))
buurt_codes = fortify(buurt_transform, region = "BUURTCODE")
buurt_codes$id = as.numeric(as.character(buurt_codes$id))
buurt_codes$group = as.numeric(as.character(buurt_codes$group))
consumption_data = read.csv('Foodconsumedperbuurt.csv')
#types = c("01. Potatoes and other tubers","02. Vegetables","03. Legumes")
#types = c(types,"04. Fruits","05. Dairy products and substitutes")
#types = c(types,"06. Cereals and cereal products","07. Meat,meat products and substitutes","08. Fish","09. Eggs and egg products","10. Fats and oils","11. Sugar and confectionery","12. Cakes and sweet biscuits","13. Non-alcoholic beverages","14. Alcoholic beverages","15. Sauces and seasonings","16. Soups and Stocks","17. Miscellaneous","18. Savoury snacks")
lci_score_leafy = mean(c(0.2194,0.36869,0.27812,4.47161))
lci_score_fruity = mean(c(0.2232,0.49596,0.815606,2.4422837))
lci_root = mean(c(9.9244772,0.20477,0.23677))
lci_cabbage = 0.368694868
lci_stalk = mean(c(0.5648577,0.5191297))
lci_mean = mean(c(lci_score_fruity,lci_score_leafy,lci_stalk,lci_score,lci_cabbage))
lci_mushroom = lci_mean
lci_grain = lci_mean
lci_pod = lci_mean
lci_value = c(lci_score_leafy,lci_score_fruity,lci_root,lci_cabbage,lci_mushroom)
lci_value = c(lci_value,lci_grain,lci_pod,lci_stalk)
types = c("0201. Leafy vegetables (exc. cabbages)",
          "0202. Fruiting vegetables",
          "0203. Root vegetables",
          "0204. Cabbages",
          "0205. Mushrooms",
          "0206. Grain and pod vegetables",
          "0207. Leek",
          "0208. Stalk vegetables")
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
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group, fill = env_impact), color = "black", size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP per capita (in kg CO'[2]*' equivalent) due to vegetables consumption'),
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
