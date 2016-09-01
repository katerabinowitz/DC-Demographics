setwd("/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/spendRetort")
library(stringr)

###tax foundation data
spend<-read.csv("https://raw.githubusercontent.com/TaxFoundation/data/master/real-purchasing-power/current/real-purchasing-power-2013.csv")
spend<-spend[order(spend$rpp.in.100.dollars),]


###real personal income
rpiSt<-read.csv("realIncomeSt.csv",stringsAsFactors=FALSE, strip.white=TRUE, skip=6,head=F)
colnames(rpiSt)<-c("fips","name","realPersonalIncome")

rpiMSA<-read.csv("realIncomeMSA.csv",stringsAsFactors=FALSE, strip.white=TRUE, skip=6,head=F)
colnames(rpiMSA)<-c("fips","name","realPersonalIncome")

rpi<-rbind(rpiSt,rpiMSA)
rpi<-subset(rpi,nchar(rpi$fips)==5 & !grepl("(Metropolitan Portion)",rpi$name))

rpi$name<-gsub(" \\(Metropolitan Statistical Area)","",rpi$name)

rpiSpend<-merge(rpi,spend,by="name")
rpiSpend<-rpiSpend[order(-rpiSpend$realPersonalIncome),]
write.csv(rpiSpend,"rpiSpend.csv")

summary(rpiSpend$realPersonalIncome)

### wage v spend

#wage data via http://download.bls.gov/pub/time.series/oe/
wageArea<-read.table("/Users/katerabinowitz/Documents/DataLensDC Org/oe.area.txt", sep="\t",
                     row.names=NULL,header=T, stringsAsFactors=FALSE, strip.white=TRUE, encoding="latin1",
                     quote="",comment='',
                     colClasses=c("character","character","character","character","character"))[c(2,4)]
colnames(wageArea)<-c("area","name")

wage<-read.table("/Users/katerabinowitz/Documents/DataLensDC Org/oe.data.0.current.txt", sep="\t",
                 row.names=NULL,header=T, stringsAsFactors=FALSE, strip.white=TRUE)

annWage<-subset(wage,str_sub(wage$series_id,-2,-1)=="08")
annWage$seasonal<-str_sub(annWage$series_id,3,3)
annWage$area<-str_sub(annWage$series_id,5,11)
annWage$industry<-str_sub(annWage$series_id,12,17)
annWage$occupation<-str_sub(annWage$series_id,18,23)
annWage$dataType<-str_sub(annWage$series_id,24,25)

allWage<-subset(annWage,annWage$industry=="000000")
allWage<-subset(allWage,allWage$occupation=="000000")

wagePlace<-merge(allWage,wageArea,by="area")

#adjust NE localities
wagePlace$name2<-gsub(" NECTA Division","",wagePlace$name)
wagePlace$name2<-ifelse(wagePlace$name2=="New Haven, CT","New Haven-Milford, CT",
                    ifelse(wagePlace$name2=="Norwich-New London-Westerly, CT-RI","Norwich-New London, CT",
                      ifelse(wagePlace$name2=="Coeur dAlene, ID","Coeur d'Alene, ID",
                        ifelse(wagePlace$name2=="Boston-Cambridge-Newton, MA","Boston-Cambridge-Newton, MA-NH",
                          ifelse(wagePlace$name2=="Springfield, MA-CT","Springfield, MA",
                            ifelse(wagePlace$name2=="Manchester, NH","Manchester-Nashua, NH",
                                   wagePlace$name2))))))
wagePlace<-wagePlace[c(1:10,12)]

spend$name2<-gsub(" \\(Nonmetropolitan Portion)","",spend$name)

wageSpend<-merge(wagePlace,spend,by="name2")

wageSpend$value<-as.numeric(wageSpend$value)
wageSpend$rel100<-(100/wageSpend$rpp.in.100.dollars)*100
wageSpend$ratio<-wageSpend$rel100/wageSpend$value
wageSpend<-wageSpend[order(wageSpend$ratio),]
colnames(wageSpend)
wageSpend<-wageSpend[c(2,6,12:17)]
write.csv(wageSpend,"wageSpend.csv")
summary(wageSpend$ratio)

