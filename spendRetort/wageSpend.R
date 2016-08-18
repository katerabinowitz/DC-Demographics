library(stringr)

spend<-read.csv("https://raw.githubusercontent.com/TaxFoundation/data/master/real-purchasing-power/current/real-purchasing-power-2013.csv")
spend<-spend[order(spend$rpp.in.100.dollars),]

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
wagePlace$name<-gsub(" NECTA Division","",wagePlace$name)

spend$name<-gsub(" \\(Nonmetropolitan Portion)","",spend$name)

wageSpend<-merge(wagePlace,spend,by="name")
check<-subset(wagePlace,!(wagePlace$name %in% wageSpend$name))
check2<-subset(spend,!(spend$name %in% wageSpend$name))
checka<-subset(check,grepl("NH|MA|CT",check$name))

wageSpend$value<-as.numeric(wageSpend$value)
wageSpend$ratio<-wageSpend$rpp.in.100.dollars/wageSpend$value
wageSpend<-wageSpend[order(wageSpend$ratio),]
colnames(wageSpend)
wageSpend<-wageSpend[c(1,2,6,7,12:16)]
write.csv(wageSpend,"/Users/katerabinowitz/Documents/DataLensDC Org/wageSpend.csv")
