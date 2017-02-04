require(dplyr)
require(acs)
require(rgdal)
require(ggplot2)
require(ggmap)
require(scales)
require(classInt)
setwd ("/Users/katerabinowitz/Documents/DataLensDCOrg/ACS")

### Pull DC data for ACS 2010 and 2015 ###
### Pull DC data for ACS 2010 and 2015 ###
### Pull DC data for ACS 2010 and 2015 ###
dc <- geo.make(state="DC",county="District of Columbia",tract="*")

race15 <- acs.fetch(geography=dc, endyear=2015, table.number="B02001", col.names="pretty") 
race10 <- acs.fetch(geography=dc, endyear=2010, table.number="B02001", col.names="pretty")

age15 <- acs.fetch(geography=dc, endyear=2015, table.number="B01001", col.names="pretty") 
age10 <- acs.fetch(geography=dc, endyear=2010, table.number="B01001", col.names="pretty")

inc15 <- acs.fetch(geography=dc, endyear=2015, table.number="B19001", col.names="pretty") 
inc10 <- acs.fetch(geography=dc, endyear=2010, table.number="B19001", col.names="pretty")

pov15 <- acs.fetch(geography=dc, endyear=2015, table.number="B06012", col.names="pretty") 
pov10 <- acs.fetch(geography=dc, endyear=2010, table.number="B06012", col.names="pretty")

ra15 <- cbind.acs(race15, age15)
rai15 <- cbind.acs(ra15, inc15)
acs15 <- cbind.acs(rai15, pov15)

ra10 <- cbind.acs(race10, age10)
rai10 <- cbind.acs(ra10, inc10)
acs10 <- cbind.acs(rai10, pov10)

acs15 <- (as.data.frame(cbind(acs15@geography$tract, acs15@estimate)))[c(1:3,13,21:24,37,45:48,61,74:79)]
acs10 <- (as.data.frame(cbind(acs10@geography$tract, acs10@estimate)))[c(1:3,13,21:24,37,45:48,61,74:79)]

rm(race15, race10, age15, age10, inc15, inc10, pov15, pov10, ra15, rai15, ra10, rai10, dc)

colnames(acs15) <- c("tract", "race15", "white15", "male15", "male2215", "male2515", "male3015", "male3515",
                    "female15", "female2215", "female2515", "female3015", "female3515",
                    "inc15", "inc10015", "inc12515", "inc15015", "inc20015",
                    "pov15", "pov10015")
colnames(acs10) <- c("tract", "race10", "white10", "male10", "male2210", "male2510", "male3010", "male3510",
                     "female10", "female2210", "female2510", "female3010", "female3510",                    
                     "inc10", "inc10010", "inc12510", "inc15010", "inc20010",
                     "pov10", "pov10010")

acs <- inner_join(acs15, acs10, by=c("tract"))
cols = c(2:39)  
acs[,cols] = apply(acs[,cols], 2, function(x) as.numeric(x))

### Map tract to neighborhood clusters ###
### Map tract to neighborhood clusters ###
### Map tract to neighborhood clusters ###
#mapping parameters provided by Office of Planning and NeighborhoodInfoDC

acs$cluster <- case_when(acs$tract %in% c("004001","004002","003900","003800","004100") ~ 1,
                          acs$tract %in% c("002701","002801","002802","002702","003200",
                                            "002900","003100","003000","003500","003600","003700") ~ 2,
                          acs$tract %in% c("003400","004400","004300") ~ 3,
                          acs$tract %in% c("000201","000202","000300","000100") ~ 4,
                          acs$tract %in% c("005701","005600", "010800") ~ 5,
                          acs$tract %in% c("005301","004201","004202","005401","005402","005202","005500","010700") ~ 6,
                          acs$tract %in% c("004801","005201","004901","004902","004802","005000", "005001","005002") ~ 7,
                          acs$tract %in% c("005800","005900","004700","005100", "004701","004702","010100") ~ 8,
                          acs$tract %in% c("006001","006201","006400","006002","006301","006100","006302", "010200", 
                                            "010500", "011000") ~ 9,
                          acs$tract %in% c("001401","001402","001500") ~ 10,
                          acs$tract %in% c("001001","001100") ~ 11,
                          acs$tract %in% c("001302","001301","001200") ~ 12,
                          acs$tract %in% c("000901","000902","000801","000802") ~ 13,
                          acs$tract %in% c("001002","000701","000702") ~ 14,
                          acs$tract %in% c("000501","000502","000600","000400") ~ 15,
                          acs$tract %in% c("001600") ~ 16,
                          acs$tract %in% c("001701","001702","001803","001804","001902","001901","010300") ~ 17,
                          acs$tract %in% c("002201","002501","002301","002502","002002","002101","002102","002202",
                                            "002600","002400", "002001") ~ 18,
                          acs$tract %in% c("009505","009507","009508","009501") ~ 19,
                          acs$tract %in% c("009504","009503","009509") ~ 20,
                          acs$tract %in% c("009201","009203","003301","009204","008702","008701","003302","004600","002302") ~ 21,
                          acs$tract %in% c("009301","009302","009102", "011100") ~ 22,
                          acs$tract %in% c("008803","008804","008802","008903","008904") ~ 23,
                          acs$tract %in% c("009101","009000","009400") ~ 24,
                          acs$tract %in% c("008001","007901","007903","008301","008302","008402","008002","008100",
                                            "008200","008600","008500","008401", "008410", "010600") ~ 25,
                          acs$tract %in% c("006804","006801","006802","006600","006700","006500","006900","007000") ~ 26,
                          acs$tract %in% c("007100","007200") ~ 27,
                          acs$tract %in% c("007503","007504") ~ 28,
                          acs$tract %in% c("009601") ~ 29,
                          acs$tract %in% c("009602","007803") ~ 30,
                          acs$tract %in% c("007808","007806","007807","007809","007804") ~ 31,
                          acs$tract %in% c("009604","009603","007708","007703") ~ 32,
                          acs$tract %in% c("009906","009903","009904","009905","009907","007707") ~ 33,
                          acs$tract %in% c("009901","007709","009902","007601","007605") ~ 34, 
                          acs$tract %in% c("007603","007604") ~ 35,
                          acs$tract %in% c("007502","007408") ~ 36,
                          acs$tract %in% c("007407","007406","007401") ~ 37,
                          acs$tract %in% c("007404","007403","007409") ~ 38,
                          acs$tract %in% c("007302","009700","007304","009804","009803","009807","009801",
                                            "009802","009806", "009810","009811", "010400", "010900") ~ 39)

### Create grouped and change variable ###
### Create grouped and change variable ###
### Create grouped and change variable ###
acs <- acs %>% mutate(age10 = male10 + female10,
                      age15 = male15 + female15,
                      ageGroup10 = male2210 + male2510 + male3010 + male3510 + female2210 + female2510 + female3010 + female3510,
                      group2010 = male2210 + male2510 + female2210 + female2510 + male3010 + female3010,
                      ageGroup15 = male2215 + male2515 + male3015 + male3515 + female2215 + female2515 + female3015 + female3515,
                      group2015 = male2215 + male2515 + female2215 + female2515 + male3015 + female3015,
                      incGroup10 = inc10010 + inc12510 + inc15010 + inc20010,
                      incGroup15 = inc10015 + inc12515 + inc15015 + inc20015) %>%
              select(-male10,-female10,-male2210,-male2510,-male3010,-male3510,-female2210,-female2510,-female3010,-female3510,
                     -male2215,-male2515,-male3015,-male3515,-female2215,-female2515,-female3015,-female3515, -tract,
                     -inc10010, -inc12510, -inc15010, -inc20010, -inc10015, -inc12515, -inc15015, -inc20015)
detach(package:acs)
detach(package:plyr)

acsC <- acs %>% 
  group_by(cluster) %>% 
  summarise_each(funs(sum), -cluster) %>%
  mutate(whiteP10=(white10/race10)*100,
         whiteP15=(white15/race15)*100,
         incP15=(incGroup15/inc15)*100,
         incP10=(incGroup10/inc10)*100,
         povP10015=(pov10015/pov15)*100,
         povP10010=(pov10010/pov10)*100,
         ageP15=(ageGroup15/age15)*100,
         ageP10=(ageGroup10/age10)*100,
         age20P15=(group2015/age15)*100,
         age20P10=(group2010/age10)*100,
        id=as.character(cluster)) %>%
  select(id,whiteP10,whiteP15,incP10,incP15,povP10015,povP10010,ageP15,ageP10,age20P15,age20P10)

acsChange <- acsC %>%
  mutate(white=whiteP15-whiteP10,
         inc=incP15-incP10,
         povP100=povP10015-povP10010,
         age=ageP15-ageP10,
         age20=age20P15-age20P10) %>%
  select(white, inc, age20, id, whiteP10,whiteP15,incP10,incP15,age20P15,age20P10)

#Create natural breaks
natSet <- function(index) {
  nat = classIntervals(acsChange[[index]], n = 5, style = 'jenks')$brks
  nat
}

nat <- lapply(1:3, FUN=natSet)

#Change closest natural break to zero to distinguish positive/negative change
nat[[1]][3]=0
nat[[2]][2]=0
nat[[3]][3]=0

group <- function(index) {
  df = cut(acsChange[[index]], breaks=nat[[index]], include.lowest=TRUE)
  df
}

acsGroup <- lapply(1:3, FUN=group)

acsGroup <- as.data.frame(cbind((matrix(unlist(acsGroup),nrow=40)),acsChange$id))

colnames(acsGroup)<-c("white", "inc", "age20", "id")

acsGroup <- acsGroup %>% mutate(white=factor(white, levels(white)[c(5,1,2,4,3)]),
                                inc=factor(inc,levels(inc)[c(5,1,3,4,2)]),
                                age20=factor(age20,levels(age20)[c(5,1,2,3,4)]))

###Bind data to geographic clusters###
###Bind data to geographic clusters###
###Bind data to geographic clusters###
cluster = readOGR("http://opendata.dc.gov/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson",
               "OGRGeoJSON")
cluster@data$NAME2 <- gsub("Cluster ", "", as.character(cluster@data$NAME))

acsCluster <- merge(cluster, acsGroup, by.y="id",by.x="NAME2")

writeOGR(acsCluster, 'acsDemCluster.geojson','acsDemCluster', driver='GeoJSON',check_exists = FALSE)

###Map data###
###Map data###
###Map data###
rc <- fortify(acsCluster, region="NAME2")
rc$id <- as.numeric(rc$id)
mapRC <- merge(rc,acsGroup,by="id")

Palette = c('#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')
incPalette = c("#f6e8c3","#c7eae5","#80cdc1","#35978f","#01665e")

pdf("whiteChange.pdf",width=8, height=8)
ggplot() +
    geom_polygon(data = mapRC, 
                 aes(x = long, y = lat, group = group, fill = white), 
                 color = "white", size = 0.05) + 
    scale_fill_manual(values=Palette) +
    coord_map()+
    theme_nothing(legend = TRUE)
dev.off()

pdf("age20Change.pdf",width=8, height=8)
ggplot() +
  geom_polygon(data = mapRC, 
               aes(x = long, y = lat, group = group, fill = age20), 
               color = "white", size = 0.05) + 
  scale_fill_manual(values=Palette) +
  coord_map()+
  theme_nothing(legend = TRUE)
dev.off()


pdf("incChange.pdf",width=8, height=8)
ggplot() +
  geom_polygon(data = mapRC, 
               aes(x = long, y = lat, group = group, fill = inc), 
               color = "white", size = 0.05) + 
  scale_fill_manual(values=incPalette) +
  coord_map()+
  theme_nothing(legend = TRUE)
dev.off()


#find cleaner breaks for labels
whiteACS<-acsChange %>% arrange(white) %>% select(white)
levels(acsGroup$white)

incACS<-acsChange %>% arrange(inc) %>% select(inc)
levels(acsGroup$inc)

ageACS<-acsChange %>% arrange(age20) %>% select(age20)
levels(acsGroup$age20)