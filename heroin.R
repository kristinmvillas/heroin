library(tidyverse)
setwd("C:/Users/krist/OneDrive/Desktop/drug data")
read.delim("NSDUH_2019_Tab.txt")
NSDUH_2019_Tab <- read.delim("NSDUH_2019_Tab.txt")
#what factors are associated with heroin use

NSDUH_2019_Tab  <- NSDUH_2019_Tab  %>% select(AGE2:POVERTY3, HEREVER:HR30EST)
NSDUH_2019_Tab <-NSDUH_2019_Tab %>% select(HEREVER:HR30EST, AGE2:MILSTAT, HEALTH:IIHH65_2)
glimpse(NSDUH_2019_Tab)

#Looking at heroin ever
NSDUH_2019_Tab %>% 
  group_by(HEREVER)%>%
 tally(sort = TRUE)

used_heroin <- NSDUH_2019_Tab %>% filter(HEREVER==1)
no_heroin <- NSDUH_2019_Tab %>% filter(HEREVER==2)

used_heroin %>% 
  group_by(WRKSTATWK2)%>%
  tally(sort = TRUE)

used_heroin %>% ggplot(aes(WRKSTATWK2, fill= WRKSTATWK2))+
  geom_histogram()

no_heroin %>% 
  group_by(WRKSTATWK2)%>%
  tally(sort = TRUE)