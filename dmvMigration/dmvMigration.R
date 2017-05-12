library(dplyr)
setwd("/Users/katerabinowitz/Documents/DataLensDCOrg/migration")

# data from: https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html

### Read in DMV migration data and format ###
### Read in DMV migration data and format ###
### Read in DMV migration data and format ###
dcMig <- read.csv("DC.csv",stringsAsFactors=FALSE, strip.white=TRUE, skip=2)[c(1:16)]
mdMig <- read.csv("MD.csv",stringsAsFactors=FALSE, strip.white=TRUE, skip=2)[c(1:16)]
vaMig <- read.csv("VA.csv",stringsAsFactors=FALSE, strip.white=TRUE, skip=2)[c(1:16)]

colnames(dcMig)<-c("stateA","countyA","stateB","countyB","stateNameA","cntyNameA","stateNameB","cntyNameB",
                          "BAflow","BAmoe","ABflow","ABmoe","netBA","netBAmoe","grossBA","grossBAmoe")
colnames(mdMig)<-c("stateA","countyA","stateB","countyB","stateNameA","cntyNameA","stateNameB","cntyNameB",
                   "BAflow","BAmoe","ABflow","ABmoe","netBA","netBAmoe","grossBA","grossBAmoe")
colnames(vaMig)<-c("stateA","countyA","stateB","countyB","stateNameA","cntyNameA","stateNameB","cntyNameB",
                   "BAflow","BAmoe","ABflow","ABmoe","netBA","netBAmoe","grossBA","grossBAmoe")

dcMig[9:16] = apply(dcMig[9:16], 2, function(x) as.numeric(gsub(",", "", x)))
mdMig[9:16] = apply(mdMig[9:16], 2, function(x) as.numeric(gsub(",", "", x)))
vaMig[9:16] = apply(vaMig[9:16], 2, function(x) as.numeric(gsub(",", "", x)))

### Subset to top five to/from  DC locations ###
### Subset to top five to/from  DC locations ###
### Subset to top five to/from  DC locations ###
dcMig <- dcMig %>% subset(stateA=="011" & !(is.na(countyB))) %>%
                   arrange(desc(ABflow))
dcMig <- dcMig[c(1:5),]

mdMig <- mdMig %>% subset((cntyNameA=="Montgomery County" | cntyNameA=="Prince George's County")
                          & (cntyNameB %in% c("Montgomery County","Prince George's County",
                                              "Arlington County","Fairfax County","Alexandria city")) & 
                          (stateNameB=="Virginia" | stateNameB=="Maryland")) %>%
  arrange(desc(grossBA)) %>%
  subset(!(stateNameB=="Virginia" & cntyNameB=="Montgomery County"))

vaMig <- vaMig %>% subset((cntyNameB %in% c("Arlington County","Fairfax County","Alexandria city")) & 
                            (cntyNameA %in% c("Arlington County","Fairfax County","Alexandria city")))

dmvMig <- rbind(dcMig,mdMig, vaMig)

### See which flow directions hold up against margin of error ###
### See which flow directions hold up against margin of error ###
### See which flow directions hold up against margin of error ###
dmvMig <- dmvMig %>%
        mutate(BAlow=BAflow-BAmoe,
               BAhigh=BAflow+BAmoe,
               ABlow=ABflow-ABmoe, 
               ABhigh=ABflow+ABmoe,
               MOE=ifelse((ABlow > BAlow & ABlow < BAhigh) | (ABhigh > BAlow & ABhigh < BAhigh), 0, 1)) %>%
        select(cntyNameA, cntyNameB, BAlow, BAflow, BAhigh, ABlow, ABflow, ABhigh, MOE)