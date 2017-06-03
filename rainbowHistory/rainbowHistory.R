require(dplyr)
require(ggmap)

rbHistory <- read.csv("Rainbow History Project Places and Spaces.csv", stringsAsFactors = FALSE)

cen18 <- rbHistory %>% subset(substr(Date.Opened, 1, 2) == "18" | substr(Date.Opened, 1, 2) == "17")

dcRB <- rbHistory %>% filter(grepl("Washington DC", Address) & (!grepl("MD|Arlington|VA|1801 H St SE|Alexandria|Various locations", Address))) %>%
  
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
                                 dec200010 = ifelse(decadeOpen <= 2010 & decadeClose > 1990, 1, 0))

LatLong <- geocode(dcRB$Address, source="google")

dcRBGeo <- cbind(dcRB, LatLong)

dcRBpre60 <- dcRBGeo %>% filter(pre1960==1) %>% mutate(dec = "pre1950") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB60 <- dcRBGeo %>% filter(dec1960==1) %>% mutate(dec = "1960") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB70 <- dcRBGeo %>% filter(dec1970==1) %>% mutate(dec = "1970") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB80 <- dcRBGeo %>% filter(dec1980==1) %>% mutate(dec = "1980") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB90 <- dcRBGeo %>% filter(dec1990==1) %>% mutate(dec = "1990") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)
dcRB0010 <- dcRBGeo %>% filter(dec200010==1) %>% mutate(dec = "2000-Present") %>% select(-pre1960, -dec1960, -dec1970, -dec1980, -dec1990, -dec200010)

dcRainbowHistory <- rbind(dcRBpre60, dcRB60, dcRB70, dcRB80, dcRB90, dcRB0010)

write.csv(dcRainbowHistory, "dcRBGeo.csv", row.names = FALSE)

