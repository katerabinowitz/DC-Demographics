library(acs)
CityFip<-geo.make(state=c("GA","AZ","OH","FL","CA","PA","MA","WA","NY","TX","TX","CA","DC","CO","TN"),
                  place=c("Atlanta city","Phoenix city","Cleveland city","Miami city","Los Angeles city","Philadelphia city",
                          "Boston city","Seattle city","New York city","Houston city","Dallas city","San Francisco city",
                          "Washington city","Denver city","Nashville"))

fetchACS<-function(x,y) {
  y<-acs.fetch(endyear=2014,span=5,geography=CityFip, 
            table.number=x, col.names="pretty")
  as.data.frame(cbind(y@geography,
                         y@estimate))
}

ageEst<-fetchACS("B07001",Age)
marEst<-fetchACS("B07008",Marry)
edEst<-fetchACS("B07009",Ed)
tenEst<-fetchACS("B07013",Ten)
incomeEst<-fetchACS("B07010",Income)
genderEst<-fetchACS("B07003",Gender)

subsetCol<-function (Z) {
 Z[grep('NAME|different county|different state|abroad',colnames(Z))]
}

ageEst2<-subsetCol(ageEst)[c(1,2,6,7,18,22,23,34,38,39)]
marEst2<-subsetCol(marEst)[c(1,2,4,8,10,14,16)]
edEst2<-subsetCol(edEst)[c(1,2,6,7,8,12,13,14,18,19)]
tenEst2<-subsetCol(tenEst)[c(1,2,4,5,7,8,10)]
incomeEst2<-subsetCol(incomeEst)[c(1,2,12,13,23,24,34)]
genderEst2<-subsetCol(genderEst)[c(1,2,4,5,7,8,10)]

Propfor2<- function(P,varname) {
colnames(P)<-c("place","dCountyTotal","dCountyStat","dStateTotal","dStateStat","dAbroadTotal","dAbroadStat")
attach(P)
P$Prop<-round((dCountyStat+dStateStat+dAbroadStat)/(dCountyTotal+dStateTotal+dAbroadTotal)*100,2)
P<-as.data.frame(P)
}

marEstProp<-Propfor2(marEst2)[c(1,8)]
colnames(marEstProp)[c(2)]<-"married"
tenEstProp<-Propfor2(tenEst2)[c(8)]
colnames(tenEstProp)[c(1)]<-"rental"
genderEstProp<-Propfor2(genderEst2)[c(8)]
colnames(genderEstProp)[c(1)]<-"female"
incomeEstProp<-Propfor2(incomeEst2)[c(8)]
colnames(incomeEstProp)[c(1)]<-"above75K"

Propfor3<- function(P) {
  colnames(P)<-c("place","dCountyTotal","dCountyStat1","dCountyStat2","dStateTotal","dStateStat1","dStateStat2",
                 "dAbroadTotal","dAbroadStat1","dAbroadStat2")
  attach(P)
  P$Prop<-round(((dCountyStat1+dCountyStat1+dStateStat1+dStateStat2+dAbroadStat1+dAbroadStat1)
                 /(dCountyTotal+dStateTotal+dAbroadTotal))*100,2)
  P<-as.data.frame(P)
}
ageEstProp<-Propfor3(ageEst2)[c(11)]
colnames(ageEstProp)[c(1)]<-"Age20s"
edEstProp<-Propfor3(edEst2)[c(11)]
colnames(edEstProp)[c(1)]<-"BAplus"

cityComparative<-cbind(marEstProp,tenEstProp,genderEstProp,incomeEstProp,ageEstProp,edEstProp)

cityComparative$City<-gsub("city","",cityComparative$place)
cityComparative$City<-gsub("\\ ,.*","",cityComparative$City)
cityComparative$City<-ifelse(grepl("Nashville",cityComparative$City),"Nashville",cityComparative$City)
cityComparative<-cityComparative[c(2:8)]

write.csv(cityComparative,"/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/cityMobilityStats.csv",row.names=FALSE)
