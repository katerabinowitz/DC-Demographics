library(plyr)
library (dplyr)
library(data.table)
library(stringr)
setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/In")

### Read in and combine IRS SOI csv ###
### Read in and combine IRS SOI csv ###
### Read in and combine IRS SOI csv ###
files = list.files(pattern="*.csv")
names<-c("in9293", "in0001","in0304","in0405", "in0506","in0607","in0708",
         "in0809","in0910","in1011","in0102","in0203","in9394","in9495", "in9596", 
         "in9697", "in9798","in9899", "in9900")

for (i in 1:length(files)) assign(names[i], 
          read.csv(files[i],
                   skip=7, fill=TRUE,
                      col.names=c("a","b","NState","NCounty",
                        "Statemix","County","NReturns","NExemptions","aggGrossIncome"))[,3:9])

mylist <- list('2000'=in0001,'2001'=in0102, '2002'=in0203, '2003'=in0304, '2004'=in0405, 
               '2005'=in0506, '2006'=in0607,'2007'=in0708,'2008'=in0809, '2009'=in0910, '2010'=in1011,
               '1992'=in9293, '1993'=in9394, '1994'=in9495, '1995'=in9596, '1996'=in9697,
               '1997'=in9798, '1998'=in9899, '1999'=in9900)

allin <- rbindlist(mylist)
allin$year <- rep(names(mylist), sapply(mylist, nrow))
allin$State<-toupper(allin$Statemix)
allinBeltway<-subset(allin,allin$State=='VA'|allin$State=='MD')
rm(list=(ls()[ls()!="allinBeltway"]))

### Create indicator for counties in DC Beltway and subset ###
### Create indicator for counties in DC Beltway and subset ###
### Create indicator for counties in DC Beltway and subset ###
allinBeltway$County<-str_trim(allinBeltway$County, side = "both")
allinBeltway$County<-toupper(allinBeltway$County)
allinBeltway$state<-str_trim(allinBeltway$State, side = "both")

allinBeltway$NExemptions<-as.numeric(gsub(",","", allinBeltway$NExemptions))
allinBeltway$NReturns<-as.numeric(gsub(",","", allinBeltway$NReturns))
allinBeltway$aggGrossIncome<-as.numeric(gsub(",","", allinBeltway$aggGrossIncome))

allinBeltway$Beltway<-ifelse(allinBeltway$County=="PRINCE GEORGE'S COUN" & allinBeltway$state=='MD','1',
                      ifelse(allinBeltway$County=="PRINCE GEORGE'S" & allinBeltway$state=='MD','1',
                        ifelse(allinBeltway$County=='MONTGOMERY COUNTY' & allinBeltway$state=='MD','1',
                        ifelse(allinBeltway$County=='MONTGOMERY' & allinBeltway$state=='MD','1',
                             ifelse(allinBeltway$County=='ARLINGTON COUNTY' & allinBeltway$state=='VA','1',
                             ifelse(allinBeltway$County=='ARLINGTON' & allinBeltway$state=='VA','1',
                                    ifelse(allinBeltway$County=='FAIRFAX COUNTY' & allinBeltway$state=='VA','1',
                                    ifelse(allinBeltway$County=='FAIRFAX' & allinBeltway$state=='VA','1',
                                    ifelse(allinBeltway$County=='FAIRFAX CITY' & allinBeltway$state=='VA','1',
                                           ifelse(allinBeltway$County=='ALEXANDRIA' & allinBeltway$state=='VA','1',
                                                  ifelse(allinBeltway$County=='FALLS CHURCH' & allinBeltway$state=='VA','1',
                                                         ifelse(allinBeltway$County=='FALLS CHURCH CITY' & allinBeltway$state=='VA','1',
                                                            ifelse(allinBeltway$County=='ALEXANDRIA CITY' & allinBeltway$state=='VA','1',
                                                                   ifelse(allinBeltway$County=="PRINCE GEORGE'S COUNTY" & allinBeltway$state=='MD','1','0'))))))))))))))
Beltway<-as.data.frame(subset(allinBeltway,allinBeltway$Beltway=='1'))

### Aggregate migration up to state level by year ###
### Aggregate migration up to state level by year ###
### Aggregate migration up to state level by year ###
beltwaySum<-ddply(Beltway,c("state","year"),summarise,
                   exemptSum=sum(NExemptions),
                   returnSum=sum(NReturns),
                   aggIncomeSum=sum(aggGrossIncome))

#Output                  
setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/Output")                  
saveRDS(beltwaySum,file="BeltwaySum.rds")

rm(list=ls())
  
