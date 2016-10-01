library(plyr)
### Read in Data ###
### Read in Data ###
### Read in Data ###
setwd ("/Users/katerabinowitz/Documents/DataLensDC/CEX-Cities")
file = list.files(pattern="*.csv")
for (i in 1:length(file)) 
  assign(file[i],read.csv(file[i]))

### DC Spend Data ###
### DC Spend Data ###
### DC Spend Data ###
DCraw<-subset(south.csv,south.csv$X!='')[c(1,3)]
colnames(DCraw)<-c('Item','DCSpend')
DCraw$Item<-as.character(DCraw$Item)

DC <- DCraw[-c(1:12,14:18,21:29,31,33:34,36,40,42:46), ]
DC$Item<-ifelse(DC$Item=="Food at home","Eating In",
                ifelse (DC$Item=="Food away from home","Eating Out",
                      ifelse(DC$Item=="Alcoholic beverages","Alcohol",
                            ifelse(DC$Item=="Vehicle purchases (net outlay)","Car Purchases (net)",
                                ifelse(DC$Item=="Public and other transportation","Public transportation",
                                      ifelse (DC$Item=="Personal care products and services","Personal Care",
                                            ifelse (DC$Item=="Tobacco products and smoking supplies","Smoking (Tobacco)",
                                                ifelse(DC$Item=="Apparel and services","Apparel",
                                                      DC$Item))))))))

write.csv(DC, 
          file="./DCSpend.csv")

### Transpose and Combine ###
### Transpose and Combine ###
### Transpose and Combine ###
data=list(midwest.csv,norteast.csv,south.csv,west.csv)
filter <- llply(data, subset, X != '')
transpose<-llply(filter,t)
mw<-as.data.frame(transpose[[1]])
ne<-as.data.frame(transpose[[2]])
south<-as.data.frame(transpose[[3]])
west<-as.data.frame(transpose[[4]])

pullname<-filter[[1]]
names<-pullname$Table.3013..Selected.midwestern.metropolitan.statistical.areas..Average.annual.expenditures.and.characteristics..Consumer.Expenditure.Survey..2013.2014
colnames(mw)<-make.names(names,unique=TRUE)
colnames(ne)<-make.names(names,unique=TRUE)
colnames(south)<-make.names(names,unique=TRUE)
colnames(west)<-make.names(names,unique=TRUE)

all.city<-rbind(mw,ne,south,west)[-c(1,2,7,8,12,13,20,21),]

### Data Cleaning and Factoring ###
### Data Cleaning and Factoring###
### Data Cleaning and Factoring###
#Assign correct formats
all.city$Income<-substring(all.city$Income.before.taxes, 2)
all.city$Spend<-substring(all.city$Average.annual.expenditures,2)
all.city<-all.city[-c(3,11)]

colnames<-t(all.city)
col<-row.names(colnames)
all.city[col] <- sapply(all.city[col],as.character)
col2<-tail(col, -1)
all.city[col2] <- sapply(all.city[col2],as.numeric)
sapply(all.city, class)
rm(list=setdiff(ls(), "all.city"))

attach(all.city)
all.city$EatIn<-(Food.at.home/Spend)*100
all.city$EatOut<-(Food.away.from.home/Spend)*100
all.city$Alcohol<-(Alcoholic.beverages/Spend)*100
all.city$Smoke<-(Tobacco.products.and.smoking.supplies/Spend)*100

all.city$PTravel<-(Public.and.other.transportation/Spend)*100
all.city$Car<-((Vehicle.purchases..net.outlay.+Gasoline.and.motor.oil+Other.vehicle.expenses)/Spend)*100
all.city$CarCosts<-Vehicle.purchases..net.outlay.+Gasoline.and.motor.oil+Other.vehicle.expenses

all.city$Look<-(Personal.care.products.and.services/Spend)*100
all.city$Dress<-(Apparel.and.services/Spend)*100

all.city$Ent<-(Entertainment/Spend)*100
all.city$City<-all.city$Item

all.city$EatRatio<-(Food.away.from.home)/(Food)*100
all.city$ReadR<-(Reading/Spend)*100

all.city$City<-ifelse(all.city$City=="Los\nAngeles","Los Angeles",
                      ifelse (all.city$City=="Minneapolis-\nSt. Paul","Minn.-St. Paul",
                              ifelse(all.city$City=="San\nFrancisco", "San Francisco",
                                     ifelse(all.city$City=="Washington,\nD.C.","Washington, DC",
                                            ifelse (all.city$City=="Dallas-\nFort Worth", "Dallas-FW",
                                            all.city$City)))))

all.city<-all.city[c('City','EatIn','Food.at.home','EatOut','Food.away.from.home','Alcohol','Alcoholic.beverages',
                     'Smoke','Tobacco.products.and.smoking.supplies','PTravel','Public.and.other.transportation',
                     'Car','CarCosts','Look','Personal.care.products.and.services','Dress','Apparel.and.services',
                     'Ent','Entertainment','ReadR','Reading','EatRatio')]

names(all.city) <- c('City','EatIn','FoodAtHome','EatOut','FoodAwayFromHome','Alcohol','Alcoholic',
                     'Smoke','Tobacco','PTravel','Public', 'Car','CarCosts','Look','PersonalCare',
                     'Dress','Apparel','Ent','Entertainment','ReadR','Reading','EatRatio')

write.csv(all.city, 
          file="./MSA-CEX-prop.csv")
