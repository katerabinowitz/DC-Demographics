require(dplyr)
require(ggmap)
require(rgdal)
setwd('/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/rainbowHistory')

rbHistory <- read.csv("Rainbow History Project Places and Spaces.csv", stringsAsFactors = FALSE)


dcRB <- rbHistory %>% 
                      # filter down to DC locations
                      filter(grepl("Washington DC", Address) & (!grepl("MD|Arlington|VA|1801 H St SE|Alexandria|Various locations|Silver Spring",
                                                                Address))) %>%
                      # extract clean open and close years, addresses
                      mutate(yrOpen = ifelse(substr(Date.Opened, 1, 2) == "18" | substr(Date.Opened, 1, 2) == "17", Date.Opened,
                                              ifelse(grepl("\\?",Date.Opened), substr(Date.Opened, nchar(Date.Opened)-5, nchar(Date.Opened) -4),
                                                 ifelse(grepl("s",Date.Opened), substr(Date.Opened, nchar(Date.Opened)-2, nchar(Date.Opened) -1),
                                              substr(Date.Opened, nchar(Date.Opened)-1, nchar(Date.Opened))))), 
                                 
                                 yrClose = ifelse(Date.Closed == "5-May", "",
                                              ifelse(grepl("\\?",Date.Closed), substr(Date.Closed, nchar(Date.Closed)-5, nchar(Date.Closed) -4),
                                                ifelse(grepl("s",Date.Closed), substr(Date.Closed, nchar(Date.Closed)-2, nchar(Date.Closed) -1),
                                                  substr(Date.Closed, nchar(Date.Closed)-1, nchar(Date.Closed))))),
 
                                 Address = gsub("th \\&", "00", Address), 
                             
                                # adjust bars on O St SE to O St SW since address no longer exists
                                 Address = ifelse(Name == "Circus", "1823 L St NW Washington DC", 
                                                  ifelse(Address=="19th St & Dupont Circle NW Washington DC", "21 Dupont Circle NW Washington DC",
                                                    ifelse(Name == "Velvet Nation", "1015 Half St SE Washington DC",
                                                           gsub("O St SE", "O St SW", Address))))) %>%
  
                        filter(yrOpen != "" | yrClose != "") %>%
                        mutate(decadeOpen = ifelse(substr(Date.Opened, 1, 2) == "18" | substr(Date.Opened, 1, 2) == "17", 
                                                     "70", paste0(substr(yrOpen, 1,1),0)),
                                 decadeClose = paste0(substr(yrClose, 1, 1),0),
                                 
                                 decadeOpen = ifelse(yrOpen == "", decadeClose, decadeOpen),
                                 decadeClose = ifelse(yrClose == "", decadeOpen, decadeClose),
                                 
                                 decadeOpen = as.numeric(ifelse(decadeOpen %in% c("00", "10") & grepl("19", Date.Opened), paste0("19",decadeOpen),
                                                     ifelse(decadeOpen %in% c("00", "10"), paste0("20", decadeOpen), paste0("19", decadeOpen)))), 
                                 decadeClose = as.numeric(ifelse(decadeClose %in% c("00", "10"), paste0("20", decadeClose),
                                                                 paste0("19", decadeClose))), 
                                 
                                 pre1960 = ifelse(decadeOpen <= 1950 & decadeClose > 1950, 1, 0),
                                 dec1960 = ifelse(decadeOpen <= 1960 & decadeClose > 1950, 1, 0),
                                 dec1970 = ifelse(decadeOpen <= 1970 & decadeClose > 1960, 1, 0),
                                 dec1980 = ifelse(decadeOpen <= 1980 & decadeClose > 1970, 1, 0),
                                 dec1990 = ifelse(decadeOpen <= 1990 & decadeClose > 1980, 1, 0),
                                 dec200010 = ifelse(decadeOpen <= 2010 & decadeClose > 1990, 1, 0), 
                               
                                  Group = ifelse(grepl("Theatre|Faith Temple|Collective|Coffee|Lambda", Name) |
                                                   grepl("book|Book|coffee|Coffee|cinema|collective|Collective", Notes),
                                                 "Community Site", Group),
                                
                                #flag individual locations for highlight on map  
                                singleOut = ifelse(grepl("Nob Hill", Name),"nobHill", 
                                              ifelse(grepl("Uptown|Gayety Buffet", Name), "mpdRaid", 
                                                ifelse(grepl("Plus One", Name), "plusOne",
                                                  ifelse(grepl("Jo-Anna's", Name), "joAnna",
                                                    ifelse(grepl("Guild Press", Name), "guildPress",
                                                      ifelse(grepl("Washington Free Clinic", Name), "clinicFirst", 
                                                        ifelse(grepl("Village Books", Name), "villageBooks", 
                                                          ifelse(grepl("Lambda Rising", Name), "lambdaBooks",
                                                            ifelse(grepl("Lammas", Name), "lammasBooks",
                                                              ifelse(grepl("GLF", Name), "glf", 
                                                                ifelse(grepl("Furies", Name), "furies",
                                                                  ifelse(grepl("Pier Nine", Name), "pier9",
                                                                    ifelse(grepl("JR's", Name), "highHeelDrag", 
                                                                      ifelse(grepl("Equus", Name), "equus",
                                                                        ifelse(grepl("Cheers", Name), "cheers", 
                                                                          ifelse(grepl("Margaret", Name), "margaret",
                                                                            ifelse(grepl("Quaker House", Name), "quaker",
                                                                              ifelse(grepl("Georgetown Lutheran", Name), "gtown",
                                                                                ifelse(grepl("Green Lantern|Cobalt", Name), "greenCobalt",
                                                                                  ifelse(grepl("9:30", Name), "club930", 
                                                                                    ifelse(grepl("Black Cat", Name), "blackCat",
                                                                                    ifelse(grepl("Phase 1", Name), "phase1",
                                                                                           ifelse(grepl("Nellie", Name), "nellie",
                                                                             ""))))))))))))))))))))))))

LatLong <- geocode(dcRB$Address, source="google")

dcRBGeo <- cbind(dcRB, LatLong)

dcRBpre60 <- dcRBGeo %>% filter(pre1960==1) %>% mutate(decade = "pre1960") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB60 <- dcRBGeo %>% filter(dec1960==1) %>% mutate(decade = "1960") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB70 <- dcRBGeo %>% filter(dec1970==1) %>% mutate(decade = "1970") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB80 <- dcRBGeo %>% filter(dec1980==1) %>% mutate(decade = "1980") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB90 <- dcRBGeo %>% filter(dec1990==1) %>% mutate(decade = "1990") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB0010 <- dcRBGeo %>% filter(dec200010==1) %>% mutate(decade = "2000-Present") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)

dcRainbowHistory <- rbind(dcRBpre60, dcRB60, dcRB70, dcRB80, dcRB90, dcRB0010)
Encoding(dcRainbowHistory$Name)<-"UTF-8"
Encoding(dcRainbowHistory$Notes)<-"UTF-8"

write.csv(dcRainbowHistory, "dcRBGeo.csv", row.names = FALSE)

dcRainbowHistory.SP  <- SpatialPointsDataFrame(dcRainbowHistory[,c(15,14)], dcRainbowHistory[,-c(15,14)])

writeOGR(dcRainbowHistory.SP, '/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/rainbowHistory/dcRH.geojson','dcRH', driver='GeoJSON', check_exists = FALSE)
