library(stringr)
library(plyr)
library (dplyr)
library(data.table)
setwd ("/Users/katerabinowitz/Documents/DataLensDC/ACS Migration Data/Input")
### Read in  and combine ACS csv ###
### Read in  and combine ACS csv ###
### Read in  and combine ACS csv ###
#files from https://www.census.gov/hhes/migration/data/acs/county_to_county_mig_2008_to_2012.html 
#ACS Survey 2008-2012 Mobility estimates
files = list.files(pattern="*.csv")

for (i in 1:length(files)) assign(files[i], 
                                  read.csv(files[i], skip=4, header=FALSE,fill=TRUE)[c(5:6,21:22,37)])
statelist <- list('AZ'=AZ.csv,'CA'=CA.csv, 'CO'=CO.csv,'DC'=DC.csv, 'FL'=FL.csv, 'GA'=GA.csv,'IL'=IL.csv,'IN'=IN.csv,
                  'MA'=MA.csv, 'MI'=MI.csv, 'NC'=NC.csv, 'NV'=NV.csv, 'NY'=NY.csv,  'OH'=OH.csv,'OK'=OK.csv, 'OR'=OR.csv, 
                  'PA'=PA.csv, 'TN'=TN.csv, 'TX'=TX.csv,'WA'=WA.csv)
allmigration<-rbindlist(statelist)

### Add state abbreviations for later matching ###
### Add state abbreviations for later matching ###
### Add state abbreviations for later matching ###
allmigration$V5<-str_trim(allmigration$V5)
allmigration$V21<-str_trim(allmigration$V21)
#DC not included in state.abb!
allmigration$toState<-ifelse(allmigration$V5=="District of Columbia", "DC",
                             (state.abb[match(allmigration$V5,state.name)]))
allmigration$fromState<-ifelse(allmigration$V21=="District of Columbia", "DC",
                             (state.abb[match(allmigration$V21,state.name)]))
allmigration<-subset(allmigration,select=-c(3))

colnames(allmigration)<-c("toStateFull","toCounty","fromCounty","N","toState","fromState")
allmigration<-as.data.frame(allmigration)

### Create data frame of cities of interest ###
### Create data frame of cities of interest ###
### Create data frame of cities of interest ###
city=c("Phoenix", "San Francisco", "Los Angeles", "Denver","Washington, D.C.","Atlanta","Chicago", 
       "Boston", "Charlotte","Manhattan","Brooklyn", "Bronx", "Queens","Staten Island","Columbus","Portland", 
       "Philadelphia", "Austin","Dallas","Houston","Seattle", "Miami", "Detroit","Las Vegas", "Nashville",
       "Memphis","Indianapolis","Oklahoma City")
state=c("AZ","CA","CA","CO","DC","GA","IL","MA","NC","NY", "NY","NY","NY","NY","OH","OR","PA","TX",
        "TX","TX","WA","FL","MI", "NV", "TN","TN","IN","OK")
county=c("Maricopa County","San Francisco County","Los Angeles County","Denver County",
         "District of Columbia", "Fulton County","Cook County","Suffolk County","Mecklenburg County",
         "New York County", "Kings County","Bronx County","Queens County","Richmond County","Franklin County","Multnomah County",
         "Philadelphia County","Travis County","Dallas County","Harris County","King County",
         "Miami-Dade County","Wayne County", "Clark County", "Davidson County","Shelby County","Marion County",
         "Oklahoma County")

city=data.frame(city,county,state)

### Merge to get migration data only for cities of interest ###
### Merge to get migration data only for cities of interest ###
### Merge to get migration data only for cities of interest ###
to.city<-as.data.frame(merge(allmigration, city, by.x = c("toState", "toCounty"), by.y = c("state","county")))
to.city<-subset(to.city,to.city$fromState!='NA')
rm(list=(ls()[ls()!="to.city"]))

### Add population ###
### Add population ###
### Add population ###
countypop<-read.csv('NotIncluded/CountyPop.csv', skip=2, header=FALSE,fill=TRUE)[c(3,8)]
countypop$V3<-str_trim(countypop$V3)
countypop$population<-countypop$V8
countypop$toCounty<-sapply(strsplit(countypop$V3, ","), "[", 1)
countypop$toStateFull<-sapply(strsplit(countypop$V3, ", "), "[", 2)

to.citypop<-as.data.frame(merge(to.city, countypop, by=c("toStateFull" ,"toCounty")))
to.citypop<-subset(to.citypop,select=-c(1,8:9))

### Combine NYC boroughs into one city: sum from counts ###
### Combine NYC boroughs into one city: sum from counts ###
### Combine NYC boroughs into one city: sum from counts ###
#calculate total NYC population
toNYpop<-subset(countypop,toStateFull=='New York') 
toNYpop<-subset(toNYpop,toCounty=="New York County"|toCounty=="Kings County" |
                  toCounty=="Bronx County"|toCounty=="Queens County"|toCounty=="Richmond County")
sum(toNYpop$population)

toNY<-subset(to.citypop,toState=='NY') 
toNYC<-ddply(toNY, .(toState,fromState, fromCounty), summarize, 
             N= sum(N))
toNYC$toCounty<-'New York City'
toNYC$city<-'New York'
toNYC$population<-8336697
toNYC<-toNYC[c(1,5,6,2,3,4,7)]

noNY<-subset(to.citypop,toState!='NY')
noNY<-noNY[c(1,2,6,5,3,4,7)]
to.allcity<-rbind(toNYC,noNY)

### Retrieve latitude and longitude data; merge onto migration file ###
### Retrieve latitude and longitude data; merge onto migration file ###
### Retrieve latitude and longitude data; merge onto migration file ###
latlong<-as.data.frame(read.csv("Gaz_counties_national.txt", fill=TRUE, sep='\t')[c(1,4,11:12)])
#simplify Dona Ana County to match latlong file
to.allcity$fromCounty<-ifelse(to.allcity$fromCounty=='Doña Ana County',
                              'Dona Ana County',as.character(to.allcity$fromCounty))

latlong$state<-str_trim(latlong$USPS)

to.latlong<-merge(x=to.allcity, y=latlong, by.x = c("toState", "toCounty"), by.y = c("state","NAME"),all.x=TRUE)
to.latlong$toLat<-to.latlong$INTPTLAT
to.latlong$toLong<-to.latlong$INTPTLONG
to.latlong<-subset(to.latlong,select=-c(INTPTLAT,INTPTLONG,USPS))

full<-merge(x=to.latlong, y=latlong, by.x = c("fromState", "fromCounty"), by.y = c("state","NAME"),all.x=TRUE)
full$fromLat<-full$INTPTLAT
full$fromLong<-full$INTPTLONG
full<-subset(full,select=-c(INTPTLAT,INTPTLONG,USPS))

### Combine NYC boroughs into one city: avg lat/long for boroughs ###
### Combine NYC boroughs into one city: avg lat/long for boroughs ###
### Combine NYC boroughs into one city: avg lat/long for boroughs ###
#Remove inter-borough migration from NYC
cull<-full[!(full$toState=="NY" & full$fromState=="NY"),]

NYOnly<-subset(latlong,USPS=='NY' & NAME=='New York County' | USPS=='NY' & NAME=='Kings County'
               | USPS=='NY' & NAME=='Bronx County' | USPS=='NY' & NAME=='Queens County'
               | USPS=='NY' & NAME=='Richmond County')
meanlat<-aggregate(NYOnly$INTPTLAT, list(NYOnly$USPS),mean)
meanlong<-aggregate(NYOnly$INTPTLONG, list(NYOnly$USPS),mean)

cull$toLat2<-as.numeric(ifelse(cull$toState=='NY',meanlat$x,cull$toLat))
cull$toLong2<-as.numeric(ifelse(cull$toState=='NY',meanlong$x,cull$toLong))

### Calculate geodistances from origin to city ###
### Calculate geodistances from origin to city ###
### Calculate geodistances from origin to city ###
geodistance <- function (lat1, lon1, lat2, lon2)
{   
  lon2 <- lon2*pi/180;
  lat2 <- lat2*pi/180;
  lon1 <- lon1*pi/180;
  lat1 <- lat1*pi/180;
  
  dlon <- lon2 - lon1
  dlat<- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2( sqrt(a), sqrt(1-a) ) 
  d <- 3961 * c;
}   


cull$distance<-geodistance(cull$fromLat, cull$fromLong, cull$toLat2, cull$toLong2)

cull$Migrants<-as.numeric(gsub(",","",cull$N))
travel.distance<-subset(cull,select=c(city, distance, Migrants, toLat2,toLong2,population))
travel.distance<-travel.distance[order(travel.distance$city, travel.distance$distance),]

###Calculate stats on migration distances
MigrationCityComp<-ddply(travel.distance,c("city"),summarise,
                  medianDistance=median(distance),
                  meanDistance=mean(distance),
                  WmeanDistance=weighted.mean(distance,Migrants),
                  totalMigrants=sum(Migrants),
                  Latitude=mean(toLat2),
                  Longitude=mean(toLong2),
                  Population=mean(population))
attach(MigrationCityComp)
MigrationCityComp$Migrant2Population<-(totalMigrants/Population)
MigrationCityComp$Migrant2Pop<-round((totalMigrants/Population)*100,digits=2)
MigrationCityComp$Migration<-WmeanDistance*MigrationCityComp$Migrant2Population
MigrationCityComp$wmDistance<-round(WmeanDistance,digits=0)
MigrationCityComp$popGroup<-ifelse(Population>2000000,'Large city (population 2 million+)',
                                   ifelse(Population>900000,'Medium-to-Large city (population 900,000 up to 2 million)',
                                          'Medium-sized city (population below 900,000)'))
MCC<-MigrationCityComp[c('city','Latitude','Longitude','Migration', 'wmDistance','Migrant2Pop')]
MCC$center<-paste(MCC$Latitude,MCC$Longitude,sep=",")

MCC<-MCC[order(MCC$Migration),]

write.csv(MigrationCityComp, file="/Users/katerabinowitz/Documents/DataLensDC/ACS Migration Data/Output/Migration Distance Output.csv")
write.csv(MCC, file="/Users/katerabinowitz/Documents/DataLensDC/ACS Migration Data/acsMigrationCityComp/Output/cityMigration.csv")
