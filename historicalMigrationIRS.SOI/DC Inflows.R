library(plyr)
library (dplyr)
library(data.table)
library(stringr)
setwd ("/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data")

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
                        "Statemix","County","NReturns","NExemptions","Agg.AGI"))[,3:8])

mylist <- list('2000'=in0001,'2001'=in0102, '2002'=in0203, '2003'=in0304, '2004'=in0405, 
               '2005'=in0506, '2006'=in0607,'2007'=in0708,'2008'=in0809, '2009'=in0910, '2010'=in1011,
               '1992'=in9293, '1993'=in9394, '1994'=in9495, '1995'=in9596, '1996'=in9697,
               '1997'=in9798, '1998'=in9899, '1999'=in9900)

allin <- rbindlist(mylist)
allin$year <- rep(names(mylist), sapply(mylist, nrow))
allin$State<-toupper(allin$Statemix)
rm(list=(ls()[ls()!="allin"]))

### Trim and Transform SOI data ###
### Trim and Transform SOI data ###
### Trim and Transform SOI data ###
allin$NState<-str_trim(allin$NState, side = "both")
allin$County<-str_trim(allin$County, side = "both")
allin$NExemptions<-str_trim(allin$NExemptions, side = "both")

allin$Exemptions<-as.numeric(gsub(",","", allin$NExemptions))


### Create indicator for counties in DC Beltway ###
### Create indicator for counties in DC Beltway ###
### Create indicator for counties in DC Beltway ###
county<-allin[Statemix=='MD'|Statemix=='VA']
sortedcounty<-county[ order(county$County), ]

allin$Beltway<-ifelse(allin$County=="Prince George's Coun",'1',
                      ifelse(allin$County=='Montgomery County','1',
                             ifelse(allin$County=='Arlington County','1',
                                    ifelse(allin$County=='Fairfax County','1',
                                           ifelse(allin$County=='Alexandria','1',
                                                  ifelse(allin$County=='Falls Church','1',
                                                         ifelse(allin$County=='Falls Church city','1',
                                                            ifelse(allin$County=='Alexandria City','1',
                                                                   ifelse(allin$County=="Prince George's County",'1','0')))))))))
remove(county,sortedcounty)                      

### Read in Census data for region definitions ###
### Read in Census data for region definitions ###
### Read in Census data for region definitions ###
census.div<-read.csv("https://www.census.gov/popest/about/geo/state_geocodes_v2013.txt",skip=5,
                     col.name=c("all"),colClasses=c('character'))
divisions<-as.data.frame(str_split_fixed(census.div$all, "     ", 4)[,2:4])
colnames(divisions)<-c("NDiv","StateN","State")


### Trim and Transform ###
### Trim and Transform ###
### Trim and Transform ###
divisions$NDiv<-str_trim(divisions$NDiv, side = "both")
divisions$StateN<-str_trim(divisions$StateN, side = "both")

                      
###Create regional names and fix division numeric ###
###Create regional names and fix division numeric ###
###Create regional names and fix division numeric ###
divisions$Div<-ifelse(divisions$NDiv=="0",'Null',
                ifelse(divisions$NDiv=="1",'New England',
                 ifelse(divisions$NDiv=='2','Mid-Atlantic',
                  ifelse(divisions$NDiv=='3','E. North Central',
                   ifelse(divisions$NDiv=='4', 'W. North Central',
                    ifelse(divisions$NDiv=='5','S. Atlantic',
                     ifelse(divisions$NDiv=='6','E. South Central',
                      ifelse(divisions$NDiv=='7', 'W. South Central',
                       ifelse(divisions$NDiv=='8','Mountain','Pacific')))))))))
                      
divisions$NState<-ifelse(divisions$StateN=="01","1",
                   ifelse(divisions$StateN=="02","2",
                      ifelse(divisions$StateN=="03","3",
                         ifelse(divisions$StateN=="04","4",
                          ifelse(divisions$StateN=="05","5",
                           ifelse(divisions$StateN=="06","6",
                            ifelse(divisions$StateN=="07","7",
                               ifelse(divisions$StateN=="08","8", 
                                ifelse(divisions$StateN=="09","9",divisions$StateN)))))))))
divisions<-divisions[divisions$StateN!='00',]

### Output State/Division map
write.csv(divisions, file="/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/Output/DivisionStateXWalk.csv",
          col.names = TRUE, sep = ",", qmethod = "double")

### Join and output full county level DC inflow data ###
### Join and output full county level DC inflow data ###
### Join and output full county level DC inflow data ###
fulldata <- join(allin, divisions, by='NState',type='inner',match='all')
in9211<-fulldata[NState!='11']
write.csv(in9211, file="D:\\DataLensDC\\DC Inflow Data-County.csv",
          col.names = TRUE, sep = ",", qmethod = "double")
rm(list=(ls()[ls()!="in9211"]))


###Read in Latitude & Longitude data for map viz ###    
###Read in Latitude & Longitude data for map viz ###  
###Read in Latitude & Longitude data for map viz ###  
latlong<-read.csv("https://dev.maxmind.com/static/csv/codes/state_latlon.csv")

### Aggregate migration up to state level by year ###
### Aggregate migration up to state level by year ###
### Aggregate migration up to state level by year ###
in9211$state<-in9211$State
st.yr.sum<-aggregate(Exemptions ~ year + state, data = in9211, FUN = 'sum')


### Create Average Latitude for Divisions ###
### Create Average Latitude for Divisions ###
### Create Average Latitude for Divisions ###
join <- join(in9211, latlong, by='state',type='inner',match='all')
divlatlong<-select(join,Div,state,latitude,longitude)
unq.div.location<-unique(divlatlong)


#### Structure for map viz in Tableau ###
#### Structure for map viz in Tableau ###
#### Structure for map viz in Tableau ###
st.yr.sum$PathID<-paste(st.yr.sum$state, 'DC', sep="-")
st.yr.sum$Path<-'1'
                      
path2<-data.frame(state=rep("DC",each=857),Path=rep('2',each=857),PathID=st.yr.sum$PathID,
                            Exemptions=st.yr.sum$Exemptions, year=st.yr.sum$year)
rpaths<-rbind(st.yr.sum,path2)
                      
all.paths <- join(rpaths, latlong, by='state',type='inner',match='all')
sortedpath<-all.paths[ order(all.paths$PathID,all.paths$year,all.paths$Path), ]
sortedpath$NExempt<-ifelse(sortedpath$Path=="1",NULL,sortedpath$Exemptions)

###Output to CSV                    
write.csv(sortedpath, file="/Users/katerabinowitz/Documents/DataLensDC/IRS SOI Data/Output/DC Inflow-Paths.csv",
          col.names = TRUE, sep = ",", qmethod = "double")
