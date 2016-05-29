library(Hmisc)
library(plyr)
### Read in Data ###
### Read in Data ###
### Read in Data ###
tractpop<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/dctractpop.csv',na.strings=c("", "NA"),
                  strip.white=TRUE) [c(1,4:5,8,11:12,15,17:18,20:21,23:24,26:27,33:34)]
tractwell<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/dctractwell.csv',na.strings=c("", "NA"),
                   strip.white=TRUE) [c(4:5,7:8,11:12,15:16,19:20,27:28)]
tracthousing<-read.csv('/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/dctracthousing.csv',na.strings=c("", "NA"),
                   strip.white=TRUE)[c(10:11,14:15,21,31,33,39,49,51:53)]
dctract<-cbind(tractpop,tractwell,tracthousing)
#str(dctract)

### Changes over Time ###
### Changes over Time ###
### Changes over Time ###
tractDif<-dctract
attach(tractDif)

tractDif$income<-as.numeric(((AvgFamilyIncAdj_2007_11-AvgFamilyIncAdj_2000)/AvgFamilyIncAdj_2000 )*100)
tractDif$income2000<-as.numeric(AvgFamilyIncAdj_2000)
tractDif$AA<-as.numeric(((PctBlackNonHispBridge_2010-PctBlackNonHispBridge_2000)/PctBlackNonHispBridge_2000)*100)
tractDif$White<-as.numeric(((PctWhiteNonHispBridge_2010-PctWhiteNonHispBridge_2000)/PctWhiteNonHispBridge_2000)*100)
tractDif$Poor<-as.numeric(((PctPoorPersons_2007_11-PctPoorPersons_2000)/PctPoorPersons_2000)*100)
tractDif$Unemp<-as.numeric(((PctUnemployed_2007_11-PctUnemployed_2000)/PctUnemployed_2000)*100)
tractDif$Name<-gsub("Tract ", "",tractDif$GEO2010)

tractDif<-tractDif[c('Name','income2000','income','PctBlackNonHispBridge_2000','AA','PctWhiteNonHispBridge_2000','White',
                     'PctPoorPersons_2000','Poor','PctUnemployed_2000','Unemp','PctAnnChgRMPriceSf_10yr',
                     'PctChgTotPop_2000_2010','PctChgPopUnder18Yea_2000_2010')]
tractDif$incomeGrp <- as.numeric(cut2(tractDif$income, g=6))
tractDif<-tractDif[order(tractDif$income), ]

tractDif<-tractDif[!(is.na(tractDif$income)),]
cor(tractDif$income,tractDif$White)
describe(tractDif)
tractDif$pop<-as.numeric(tractDif$PctChgTotPop_2000_2010)
tractDif$kids<-as.numeric(tractDif$PctChgPopUnder18Yea_2000_2010)

tractDif$BAI<-ifelse(tractDif$income2000<71034,1,0)

tractIncomeD<-ddply(tractDif,c("BAI"),summarise,
                       meanInc=mean(income),
                       medInc=median(income))

plot(tractDif$BAI,tractDif$income)

BAItract<-subset(tractDif,tractDif$income2000<71034)
AAItract<-subset(tractDif,tractDif$income2000>=71034)
cor(AAItract$AA,AAItract$income)
plot(BAItract$AA,BAItract$income)



BAItract<-BAItract[order(BAItract$AA), ]


write.csv(tractDif, 
          file="/Users/katerabinowitz/Documents/DataLensDC/DC-Neighborhoods/tractout.csv")


