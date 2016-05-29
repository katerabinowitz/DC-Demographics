library(plyr)
setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/Output")    

### Read in data files created in DC Beltway Migration.R and stateflow.R ###
### Read in data files created in DC Beltway Migration.R and stateflow.R ###
### Read in data files created in DC Beltway Migration.R and stateflow.R ###
divSum<-readRDS(file="DivisionSum.rds")
stateSum<-readRDS(file="StateSum.rds")
beltwaySum<-readRDS(file="BeltwaySum.rds")

stateBeltway<-subset(stateSum,stateSum$state=='VA'|stateSum$state=='MD')

stateBeltway<-stateBeltway[order(stateBeltway$year,stateBeltway$state),]
beltwaySum<-beltwaySum[order(beltwaySum$year,beltwaySum$state),]

### State Level ###
### State Level ###
### State Level ###
###Add latitude and longitude to beltwaySum for rbind with final state doc ###
beltwaySum$latitude<-stateBeltway$latitude
beltwaySum$longitude<-stateBeltway$longitude
beltwaySum$state<-ifelse(beltwaySum$state=="VA","VA-beltway","MD-beltway")

### Rebuild stateSum with variables for VA & MD without Beltway migration ###
lessBeltway<-stateBeltway[c("year","latitude","longitude")]

lessBeltway$returnSum<-stateBeltway$NReturns-beltwaySum$returnSum
lessBeltway$exemptSum<-stateBeltway$NExemptions-beltwaySum$exemptSum
lessBeltway$aggIncomeSum<-stateBeltway$aggGrossIncome-beltwaySum$aggIncomeSum
lessBeltway$state<-paste(stateBeltway$state,"noBeltway",sep="-")

###Put together State.Final ###
names(stateSum)[5:7]<-c("exemptSum","returnSum","aggIncomeSum")
State.Final<-rbind(stateSum,beltwaySum,lessBeltway)

#### Structure for map viz in Tableau ###
State.Final$PathID<-paste(State.Final$state, 'DC', sep="-")
State.Final$Path<-'1'

path2<-data.frame(state=rep("DC",each=1026),Path=rep('2',each=1026), latitude=rep(38.8964,each=1026),
                  longitude=rep(-77.0262,each=1026),PathID=State.Final$PathID,year=State.Final$year,
                  exemptSum=State.Final$exemptSum, returnSum=State.Final$returnSum,
                  aggIncomeSum=State.Final$aggIncomeSum)
statePaths<-rbind(State.Final,path2)

statePaths<-statePaths[ order(statePaths$PathID,statePaths$year,statePaths$Path), ]

### Output to CSV ###    
write.csv(State.Final, file="DC Migration,State.csv")
write.csv(statePaths, file="DC Migration Paths,State.csv")

###Division Level ###
###Division Level ###
###Division Level ###
###Add latitude and longitude to beltwaySum for rbind with final div doc ###
beltwayDivSum<-ddply(beltwaySum,c("year"),summarise,
                  exemptSum=sum(exemptSum),
                  returnSum=sum(returnSum),
                  aggIncomeSum=sum(aggIncomeSum))
beltwayDivSum$Div<-"Beltway"
beltwayDivSum$latAvg<-38.25
beltwayDivSum$longAvg<- -77.85

### Rebuild divSum with variables for VA & MD without Beltway migration ###
sAtlantic<-subset(divSum,divSum$Div=="S. Atlantic")

divNoBeltway<-sAtlantic[c("year","latAvg","longAvg")]

divNoBeltway$returnSum<-sAtlantic$returnSum-beltwayDivSum$returnSum
divNoBeltway$exemptSum<-sAtlantic$exemptSum-beltwayDivSum$exemptSum
divNoBeltway$aggIncomeSum<-sAtlantic$aggIncomeSum-beltwayDivSum$aggIncomeSum
divNoBeltway$Div<-paste(sAtlantic$Div,"noBeltway",sep="-")

Division.Final<-rbind(divSum,beltwayDivSum,divNoBeltway)
names(Division.Final)[6:7]<-c("Latitude","Longitude")

#### Structure for map viz in Tableau ###
Division.Final$PathID<-paste(Division.Final$Div, 'DC', sep="-")
Division.Final$Path<-'1'

path2<-data.frame(Div=rep("DC",each=209),Path=rep('2',each=209), Latitude=rep(38.8964,each=209),
                  Longitude=rep(-77.0262,each=209),PathID=Division.Final$PathID,year=Division.Final$year,
                  exemptSum=Division.Final$exemptSum, returnSum=Division.Final$returnSum,
                  aggIncomeSum=Division.Final$aggIncomeSum)
divPaths<-rbind(Division.Final,path2)

divPaths<-divPaths[ order(divPaths$PathID,divPaths$year,divPaths$Path), ]

### Output to CSV ###   
write.csv(Division.Final, file="DC Migration,Div.csv")
write.csv(divPaths, file="DC Migration Paths,Div.csv")
