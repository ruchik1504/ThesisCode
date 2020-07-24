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
#types = c("01. Potatoes and other tubers","02. Vegetables","03. Legumes")
#types = c(types,"04. Fruits","05. Dairy products and substitutes")
#types = c(types,"06. Cereals and cereal products","07. Meat,meat products and substitutes","08. Fish","09. Eggs and egg products","10. Fats and oils","11. Sugar and confectionery","12. Cakes and sweet biscuits","13. Non-alcoholic beverages","14. Alcoholic beverages","15. Sauces and seasonings","16. Soups and Stocks","17. Miscellaneous","18. Savoury snacks")

# Vegetable LCI start

lci_score_leafy = mean(c(0.2194,0.36869,0.27812,4.47161))
lci_score_fruity = mean(c(0.2232,0.49596,0.815606,2.4422837))
lci_root = mean(c(9.9244772,0.20477,0.23677))
lci_cabbage = 0.368694868
lci_stalk = mean(c(0.5648577,0.5191297))
lci_mean = mean(c(lci_score_fruity,lci_score_leafy,lci_stalk,lci_root,lci_cabbage))
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
fin_df_veg = fin_df
names(fin_df_veg)[names(fin_df_veg) == "env_impact"] <- "env_impact_vegetables"
final_df = fin_df_veg
#Vegetable LCI end

#Potato LCI start
lci_score_potato = mean(c(0.3134269))
lci_value = c(lci_score_potato)
types = c("01. Potatoes and other tubers")
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
fin_df_potato = fin_df
names(fin_df_potato)[names(fin_df_potato) == "env_impact"] <- "env_impact_potato"
keep = c("Name",'env_impact_potato')
fin_df_potato = fin_df_potato[keep]
final_df = (merge(final_df,fin_df_potato, by.x = 'Name', by.y = 'Name',all=FALSE))
#potato LCI end

#Legumes LCI start
lci_score_legumes = mean(c(0.49577,0.419197))    #protien pea,fava bean
lci_value = c(lci_score_legumes)
types = c("03. Legumes")
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
fin_df_legumes = fin_df
names(fin_df_legumes)[names(fin_df_legumes) == "env_impact"] <- "env_impact_legumes"
keep = c("Name",'env_impact_legumes')
fin_df_legumes = fin_df_legumes[keep]
final_df = (merge(final_df,fin_df_legumes, by.x = 'Name', by.y = 'Name',all=FALSE))

#Legumes_lci_end

#Fruits start
lci_fruits = mean(c(0.2144,0.30274,0.26713,0.274335,0.43665,0.536875,0.27198,0.414493,0.31012,0.26957,0.281535))    #pineapple,Apple,Apricot,banana,pear,Strawberry,orange,peach,lemon,grape,mango
lci_olive = 0.4175134
lci_almond = 1.142471
lci_value = c(lci_fruits,lci_almond,lci_olive)
types = c("0401. Fruits","0402. Nuts and seeds (+ nut spread)","0403. Olives")
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
fin_df_fruits = fin_df
names(fin_df_fruits)[names(fin_df_fruits) == "env_impact"] <- "env_impact_fruits"
keep = c("Name",'env_impact_fruits')
fin_df_fruits = fin_df_fruits[keep]
final_df = (merge(final_df,fin_df_fruits, by.x = 'Name', by.y = 'Name',all=FALSE))
#Fruits end

#Milk start
lci_milk = 1.701057
lci_value = c(lci_milk)
types = c("05. Dairy products and substitutes")
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
fin_df_milk = fin_df
names(fin_df_milk)[names(fin_df_milk) == "env_impact"] <- "env_impact_milk"
keep = c("Name",'env_impact_milk')
fin_df_milk = fin_df_milk[keep]
final_df = (merge(final_df,fin_df_milk, by.x = 'Name', by.y = 'Name',all=FALSE))
#Milk end

#Cereals start
lci_flour = mean(c(0.81465,0.846566))     #Maize and wheat
lci_rice = mean(c(3.49184,1.430981))         #basmati and non basmati
lci_mean = mean(c(lci_flour,lci_rice))
lci_bread = lci_mean
lci_breakfast = lci_mean
lci_pastry = lci_mean
lci_value = c(lci_flour,lci_rice,lci_bread,lci_breakfast,lci_pastry)

types = c("0601. Flour",
          "0602. Pasta,Rice,OtherGrain",
          "0603. Bread")
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
fin_df_cereals = fin_df
names(fin_df_cereals)[names(fin_df_cereals) == "env_impact"] <- "env_impact_cereals"
keep = c("Name",'env_impact_cereals')
fin_df_cereals = fin_df_cereals[keep]
final_df = (merge(final_df,fin_df_cereals, by.x = 'Name', by.y = 'Name',all=FALSE))
# Cereals end

#Meat start
lci_redmeat = 16.74338
lci_value = lci_redmeat

types = c("07. Meat,meat products and substitutes")
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
fin_df_meat = fin_df
names(fin_df_meat)[names(fin_df_meat) == "env_impact"] <- "env_impact_meat"
keep = c("Name",'env_impact_meat')
fin_df_meat = fin_df_meat[keep]
final_df = (merge(final_df,fin_df_meat, by.x = 'Name', by.y = 'Name',all=FALSE))
#Meat end

#Fish start
lci_fish = mean(c(1.9384,2.56469))          #Marine and demersal fish
lci_value = lci_fish

types = c("08. Fish")
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
fin_df_fish = fin_df
names(fin_df_fish)[names(fin_df_fish) == "env_impact"] <- "env_impact_fish"
keep = c("Name",'env_impact_fish')
fin_df_fish = fin_df_fish[keep]
final_df = (merge(final_df,fin_df_fish, by.x = 'Name', by.y = 'Name',all=FALSE))
#Fish end

#Fats start
lci_vegetableoil = mean(c(5.6303408))        
lci_butter = mean(c(7.39713))       
lci_fishoil = mean(c(1.182828))
lci_mean = mean(c(lci_vegetableoil,lci_butter,lci_fishoil))
lci_margarine = lci_mean
lci_value = c(lci_vegetableoil,lci_butter,lci_fishoil,lci_margarine)

types = c("1001. Vegetable oils",
          "1002. Butter",
          "1003. Margarines and cooking fats",
          "1004. Other animal fats (incl fish oil)"
)
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
fin_df_fats = fin_df
names(fin_df_fats)[names(fin_df_fats) == "env_impact"] <- "env_impact_fats"
keep = c("Name",'env_impact_fats')
fin_df_fats = fin_df_fats[keep]
final_df = (merge(final_df,fin_df_fats, by.x = 'Name', by.y = 'Name',all=FALSE))
#Fats end

#sugar start
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
fin_df_sugar = fin_df
names(fin_df_sugar)[names(fin_df_sugar) == "env_impact"] <- "env_impact_sugar"
keep = c("Name",'env_impact_sugar')
fin_df_sugar = fin_df_sugar[keep]
final_df = (merge(final_df,fin_df_sugar, by.x = 'Name', by.y = 'Name',all=FALSE))

env_impact_names = c("env_impact_vegetables","env_impact_potato","env_impact_legumes")
env_impact_names = c(env_impact_names,"env_impact_fruits" ,"env_impact_milk","env_impact_cereals")
env_impact_names = c(env_impact_names,"env_impact_meat","env_impact_fish","env_impact_fats","env_impact_sugar")
final_df[env_impact_names] = final_df[env_impact_names]*(365/1000)
final_df$total_impact = rowSums(final_df[env_impact_names])
consumption_per_buurt = (merge(buurt_codes,final_df, by.x = 'id', by.y = 'BUURTCODE',all=FALSE))
consumption_per_buurt = consumption_per_buurt[order(consumption_per_buurt$order),]

ggplot() +
  geom_polygon(data =consumption_per_buurt, aes(x = long, y = lat, group = group,fill = total_impact),color='NA',size = 0.15) + 
  coord_fixed()  + scale_fill_viridis() +
  labs(title = expression('GWP per capita (in kg CO'[2]*' equivalent) per year due to food consumption'),
       subtitle = "",
       x = "", y = "") + theme_void() +
  theme(plot.title=element_text(size=11,face="bold",hjust=0.75))+
  north(consumption_per_buurt) + 
  scalebar(consumption_per_buurt, dist = 2,dist_unit="km",
           transform = FALSE, model = "WGS84",st.size=3,height=0.01)
write.csv(final_df,"environmental_impact_of_food.csv",row.names = FALSE)
