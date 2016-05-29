library(plyr)
library (dplyr)
library(data.table)
library(stringr)
library(gdata)

setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/InState")

### Read in and combine IRS SOI csv ###
### Read in and combine IRS SOI csv ###
### Read in and combine IRS SOI csv ###
files = list.files(pattern="*.xls")
names<-c("in0809", "in0910","in1011","in0304", "in9900", "in0001","in0102",
         "in0203","in9394","in9495","in9697","in9798","in9899","in9091", "in9192", 
         "in0405", "in0506","in0607", "in0708","in9293","in9596")

for (i in 1:length(files)) 
assign(names[i], read.xls(files[i], sheet=1,na.strings=c("","NA"))[,1:6])

mylist <- list('2000'=in0001,'2001'=in0102, '2002'=in0203, '2003'=in0304, '2004'=in0405, 
               '2005'=in0506, '2006'=in0607,'2007'=in0708,'2008'=in0809, '2009'=in0910, '2010'=in1011,
               '1992'=in9293, '1993'=in9394, '1994'=in9495, '1995'=in9596, '1996'=in9697,
               '1997'=in9798, '1998'=in9899, '1999'=in9900)
allin <- rbindlist(mylist)
allin$year <- rep(names(mylist), sapply(mylist, nrow))
rm(list=(ls()[ls()!="allin"]))
allin <- na.omit(allin)
names(allin) <- c("NState", "state", "stateName","NReturns","NExemptions","aggGrossIncome","year")

### Trim and Transform SOI data ###
### Trim and Transform SOI data ###
### Trim and Transform SOI data ###
allin$NState<-str_trim(allin$NState, side = "both")
allin$state<-str_trim(allin$state, side = "both")
allin$state<-toupper(allin$state)
allin$NExemptions<-str_trim(allin$NExemptions, side = "both")

allin$NExemptions<-as.numeric(gsub(",","", allin$NExemptions))
allin$NReturns<-as.numeric(gsub(",","", allin$NReturns))
allin$aggGrossIncome<-as.numeric(gsub(",","", allin$aggGrossIncome))

toDC<-subset(allin,state!="DC")

toDC$stateN<-ifelse(toDC$NState=="1","01",
                         ifelse(toDC$NState=="2","02",
                                ifelse(toDC$NState=="3","03",
                                       ifelse(toDC$NState=="4","04",
                                              ifelse(toDC$NState=="5","05",
                                                     ifelse(toDC$NState=="6","06",
                                                            ifelse(toDC$NState=="7","07",
                                                                   ifelse(toDC$NState=="8","08", 
                                                                          ifelse(toDC$NState=="9","09",toDC$NState)))))))))

### Read in Census data for region definitions ###
### Read in Census data for region definitions ###
### Read in Census data for region definitions ###
census.div<-read.csv("https://www.census.gov/popest/about/geo/state_geocodes_v2013.txt",skip=5,
                     col.name=c("all"),colClasses=c('character'))
divisions<-as.data.frame(str_split_fixed(census.div$all, "     ", 4)[,2:4])
names(divisions)<-c("NDiv","stateN","State")

### Trim ###
divisions$NDiv<-str_trim(divisions$NDiv, side = "both")
divisions$stateN<-str_trim(divisions$stateN, side = "both")

###Create regional names ###
###Create regional names ###
###Create regional names ###
divisions$Div<-ifelse(divisions$NDiv=="0",'Null',
                      ifelse(divisions$NDiv=="1",'New England',
                             ifelse(divisions$NDiv=='2','Mid-Atlantic',
                                    ifelse(divisions$NDiv=='3','E. North Central',
                                           ifelse(divisions$NDiv=='4', 'W. North Central',
                                                  ifelse(divisions$NDiv=='5','S. Atlantic',
                                                         ifelse(divisions$NDiv=='6','E. South Central',
                                                                ifelse(divisions$NDiv=='7', 'W. South Central',
                                                                       ifelse(divisions$NDiv=='8','Mountain','Pacific')))))))))

### Join and output full county level DC inflow data ###
### Join and output full county level DC inflow data ###
### Join and output full county level DC inflow data ###
fulldata <- join(toDC, divisions, by='stateN',type='inner',match='all')
rm(list=(ls()[ls()!="fulldata"]))

###Read in Latitude & Longitude data for map viz ###    
###Read in Latitude & Longitude data for map viz ###  
###Read in Latitude & Longitude data for map viz ###  
latlong<-read.csv("https://dev.maxmind.com/static/csv/codes/state_latlon.csv")
join<- join(fulldata, latlong, by='state',type='inner',match='all')

### State Final ###
### State Final ###
### State Final ###
stateFinal<-subset(join, select=c("year","state","latitude","longitude","NExemptions","NReturns","aggGrossIncome"))

### Aggregate to Division Level ###
### Aggregate to Division Level ###
### Aggregate to Division Level ###
divisionFinal<-ddply(join,c("Div","year"),summarise,
                     exemptSum=sum(NExemptions),
                     returnSum=sum(NReturns),
                     aggIncomeSum=sum(aggGrossIncome),
                     latAvg=mean(latitude),
                     longAvg=mean(longitude))

#Output                  
setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/Output")                  
saveRDS(divisionFinal,file="DivisionSum.rds")
saveRDS(stateFinal,file="StateSum.rds")

rm(list=ls())