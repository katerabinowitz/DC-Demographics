library(stringr)
library(plyr)
library(Hmisc)
### Read in Data ###
### Read in Data ###
### Read in Data ###
hoodRaw<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/neighborhooddata.csv',na.strings=c("", "NA"),
                   strip.white=TRUE) 
hoodRaw$income0812<-as.numeric(gsub(",","", hoodRaw$income0812))
hoodRaw$income00<-as.numeric(gsub(",","", hoodRaw$income00))

### Changes over Time ###
### Changes over Time ###
### Changes over Time ###
hoodDiff<-hoodRaw
attach(hoodDiff)
hoodDiff$income<-((income0812-income00)/income00)*100
hoodDiff$bl<-((bl10-bl00)/bl00)*100
hoodDiff$wh<-wh10-wh00
hoodDiff$hs<-hs10-hs00
hoodDiff$emp<-emp0812-emp00
hoodDiff$pCrime<-((pCrime10-pCrime00)/pCrime00)*100
hoodDiff$vCrime<-((vCrime10-vCrime00)/vCrime00)*100
hoodDiff$poverty<-poverty0812-poverty00

hoodDiff$home00<-as.numeric(as.character(gsub(',','',home00)))
hoodDiff$home13<-as.numeric(as.character(gsub(',','',home13)))

hoodDiff$NAME<-paste("Cluster ", hoodDiff$X.built.in.function.id.,sep="")


### Explore Key Aspects of Gentrification ###
### Explore Key Aspects of Gentrification ###
### Explore Key Aspects of Gentrification ###
#income, home price, proportion of minorities and proportion of families/children
hoodDiff<-hoodDiff[order(-hoodDiff$income), ]
describe(hoodDiff)
#subset neighborhoods to those below median income in 2000
hoodBMI<-subset(hoodDiff,hoodDiff$income00<77515)
attach(hoodBMI)
cor(bl, income)
plot(kidCh10,income)

hoodHome<-subset(hoodBMI,!(hoodBMI$home13=='i'))
hoodHome$home<-((hoodHome$home13-hoodHome$home00)/hoodHome$home00)*100
hoodHome<-hoodHome[order(-hoodHome$home), ]

tocsv<-hoodBMI[c('NAME','income','bl','kidCh10')]
tocsv2<-hoodHome[c('NAME','home')]


write.csv(tocsv, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/gentdemo.csv",row.names=FALSE)
write.csv(tocsv2, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/genthouse.csv",row.names=FALSE)
