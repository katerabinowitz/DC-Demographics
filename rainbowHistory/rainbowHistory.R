require(dplyr)
require(ggmap)
require(rgdal)

setwd('/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/rainbowHistory')

rbHistory <- read.csv("Rainbow History Project Places and Spaces.csv", stringsAsFactors = FALSE)

dcRB <- rbHistory %>% filter(grepl("Washington DC", Address) & (!grepl("MD|Arlington|VA|1801 H St SE|Alexandria|Various locations|Silver Spring", Address))) %>%
  
                      mutate(yrOpen = ifelse(substr(Date.Opened, 1, 2) == "18" | substr(Date.Opened, 1, 2) == "17", Date.Opened,
                                              ifelse(grepl("\\?",Date.Opened), substr(Date.Opened, nchar(Date.Opened)-5, nchar(Date.Opened) -4),
                                                 ifelse(grepl("s",Date.Opened), substr(Date.Opened, nchar(Date.Opened)-2, nchar(Date.Opened) -1),
                                              substr(Date.Opened, nchar(Date.Opened)-1, nchar(Date.Opened))))), 
                                 
                                 yrClose = ifelse(Date.Closed == "5-May", "",
                                              ifelse(grepl("\\?",Date.Closed), substr(Date.Closed, nchar(Date.Closed)-5, nchar(Date.Closed) -4),
                                                ifelse(grepl("s",Date.Closed), substr(Date.Closed, nchar(Date.Closed)-2, nchar(Date.Closed) -1),
                                                  substr(Date.Closed, nchar(Date.Closed)-1, nchar(Date.Closed))))),
 
                                 Address = gsub("th \\&", "00", Address), 
                                 Address = ifelse(Name == "Circus", "1823 L St NW Washington DC", 
                                                  ifelse(Address=="19th St & Dupont Circle NW Washington DC", "21 Dupont Circle NW Washington DC", 
                                                         Address))) %>%
  
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
                                
                                  singleOut = ifelse(grepl("Nob Hill", Name),"nobHill", 
                                              ifelse(grepl("Uptown|Gayety Buffet", Name), "mpdRaid", 
                                                ifelse(grepl("Plus One|Jo-Anna's|Guild Press", Name), "BarracksRow", 
                                                  ifelse(grepl("Washington Free Clinic", Name), "clinicFirst", 
                                                    ifelse(grepl("Curiosity Book|Mark I|Community Bookshop|Lambda Rising|Village Books|Lammas", Name), "books", 
                                                      ifelse(grepl("Follies|Pier Nine|Washington Square|O'Henry's|Other Side", Name), "SW", 
                                                        ifelse(grepl("Furies", Name), "furies", 
                                                          ifelse(grepl("Whitman|VD", Name), "clinics", 
                                                            ifelse(grepl("Church", Name), "church",
                                                              ifelse(grepl("JR's", Name), "highHeelDrag", 
                                                                ifelse(grepl("Equus", Name), "marineAttack", 
                                                                  ifelse(grepl("Cheers", Name), "mpdMask", 
                                                                      ifelse(grepl("Remington's", Name), "tearGas", 
                                                                             ""))))))))))))))

LatLong <- geocode(dcRB$Address, source="google")

dcRBGeo <- cbind(dcRB, LatLong)

dcRBpre60 <- dcRBGeo %>% filter(pre1960==1) %>% mutate(decade = "pre1960") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB60 <- dcRBGeo %>% filter(dec1960==1) %>% mutate(decade = "1960") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB70 <- dcRBGeo %>% filter(dec1970==1) %>% mutate(decade = "1970") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB80 <- dcRBGeo %>% filter(dec1980==1) %>% mutate(decade = "1980") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB90 <- dcRBGeo %>% filter(dec1990==1) %>% mutate(decade = "1990") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB0010 <- dcRBGeo %>% filter(dec200010==1) %>% mutate(decade = "2000-Present") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)

dcRainbowHistory <- rbind(dcRBpre60, dcRB60, dcRB70, dcRB80, dcRB90, dcRB0010)

write.csv(dcRainbowHistory, "dcRBGeo.csv", row.names = FALSE)

dcRainbowHistory.SP  <- SpatialPointsDataFrame(dcRainbowHistory[,c(15,14)], dcRainbowHistory[,-c(15,14)])

writeOGR(dcRainbowHistory.SP, '/Users/katerabinowitz/Documents/DataLensDC/DC-Demographics/rainbowHistory/dcRH.geojson','dcRH', driver='GeoJSON', check_exists = FALSE)
