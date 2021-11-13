library(tidyverse)
setwd("C:/Users/krist/OneDrive/Desktop/drug data")
read.delim("NSDUH_2019_Tab.txt")
NSDUH_2019_Tab <- read.delim("NSDUH_2019_Tab.txt")
#what factors are associated with heroin use
#HERREC time since last used heroin
#factors of interest include:
#impresp how much difficulty did you have taking care of your daily responsibilities at work or school?
#AGE2 (CATA6G) age
#NOMARR2 (number of times married)
#SERVICE military service
#HEatlh (overall health)
#SEXIDENT (sexual identity)
#NEWRACE2 (race)
#EDUHIGHCAT (highered)
#(HLTH24)
#STDANYYR had std
#poverty3 income
#IRSEX imputed sex

NSDUH_2019_Tab  <- NSDUH_2019_Tab  %>% select(HEREVER, HERREC, IRSEX, IMPRESP, 
                                              CATAG6, NOMARR2,SERVICE, HEALTH,
                                              SEXIDENT, NEWRACE2, EDUHIGHCAT, POVERTY3, STDANYYR)

glimpse(NSDUH_2019_Tab)

summary(NSDUH_2019_Tab)

#Looking at heroin use ever
NSDUH_2019_Tab %>% 
  group_by(HEREVER)%>%
 tally(sort = TRUE)

#looking at heroin last used

NSDUH_2019_Tab %>%
  group_by(HERREC)%>%
  tally(sort = TRUE)

#going to do some feature engineering. cut offs---
#less than 12 months ago, more than 12 months ago, never used

NSDUH_2019_Tab <- NSDUH_2019_Tab$HERREC %>%
  filter %in% c(1, 2, 3, 8, 9, 91)

NSDUH_2019_Tab <- NSDUH_2019_Tab %>% mutate(HERREC = recode(HERREC, "1"= "used_last_year",
                                                             "2" = "used_last_year",
                                                             "8" = "used_last_year",
                                                            "3" = "used_ever",
                                                            "9"= "used_ever",
                                                            "91" = "never_used",
                                                            ))

used_heroin <- NSDUH_2019_Tab %>% filter(HEREVER==1)
no_heroin <- NSDUH_2019_Tab %>% filter(HEREVER==2)

used_heroin %>% 
  group_by(WRKSTATWK2)%>%
  tally(sort = TRUE)

used_heroin %>% ggplot(aes(WRKSTATWK2, fill= WRKSTATWK2))+
  geom_histogram()

