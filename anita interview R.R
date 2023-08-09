library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
Household_Baseline<-read.csv("L4H_household_baseline_sample.csv")
Indivudual <-read.csv("L4H_individual_baseline_sample.csv")
mother <- read.csv("L4H_mother_baseline_sample.csv")

HB1<- Household_Baseline %>%
clean_names()

Ind1 <- Indivudual %>%
   clean_names()
Mama <- mother %>%
  clean_names()
HB2 <- HB1 %>%
  mutate(hh_eligible = recode(hh_eligible,"1"= "Yes","0"="No"))
HB3 <-HB2 %>%
  filter(hh_eligible =="Yes")
Ind2<-Ind1 %>%
  rename(number_0=number)
A <-Ind2 %>%
  left_join(Mama,by="number_0")
A1 <- HB3 %>%
  left_join(A,by= "household_id")
A2 <- A1 %>%
  mutate(reason_for_ineligibility=recode(reason_for_ineligibility,"1"="No adult occupier>16","2"="withdrawal","3"="Other Reason"))%>%
  mutate(rspntgndr=recode(rspntgndr,"1"="Male","2"="Female"))%>%
  mutate(h_hfrml_eductn=recode(h_hfrml_eductn,"1"="Not completed Primary scholol","2"="Primary school","3"="Secondary school","4"="College-graduate","5"="Madrassa","6"="Other"))%>%
  mutate(rspndtmarital=recode(rspndtmarital,"1"="Single","2"="Married monogamous","3"="Married polygamous","4"="Divorced","5"="Separated","6"="Widow(er)"))%>%
  mutate(rspndt_eductn=recode(rspndt_eductn,"1"="None","2"="Primary school","3"="Secondary school","4"="College-graduate","5"="Madrassa","6"="Other"))
%>%
  mutate(maincme=recode(maincme,"1"="Sale of livestock & livestock products","2"="Sale of crops","3"="Trading/Business","4"="Employment(salaried income)","5"="Sale of personal items","6"="Remittance","7"="Other"))
 library(tidyverse)
A3 <- A2 %>% 
  separate(lvstckown,into = stringr::str_c("livestock_" ,1:30),sep = "") %>%
  separate(herdynamics,into = stringr :: str_c("herd_",1:3),sep = "")
  
A4 <-A3%>%
  mutate(study_arm= ifelse(village.x %in% c("Lependera", "Saale-Sambakah","Namarei","Lokoshula","TubchaDakhane","Rengumo-Gargule"), "Study arm 1", 
                           ifelse( village.x %in% c("Uyam village","Manyatta K.A.G","Ltepes Ooodo","Lbaarok1"), "Study arm 2",
                           "Study arm 3")))
herd_dynamics<-A4%>%
  select(interview_date.x,household_id,study_arm,cwsbrth,shpbrth,goatsbrth,cmlsbrth,calves_death,bulls_death,cows_death,sheep_death,msheep_death,fsheep_death,goats_death,mgoats_death,fgoats_death,camels_death,mcamels_death,fcamels_death,cowsgft,sheepgfts,goatsgft,cmlsgft)
 
herd_dynamics1<-herd_dynamics %>%
  separate(interview_date.x,into = c("year","month","day"),sep = "-")
herd_dynamics2<- herd_dynamics1 %>%
  mutate(monthyear=paste0(year,"-",month))
A4<-herd_dynamics2 %>%
  group_by(monthyear,study_arm)%>%
  mutate(cows_births=sum(cwsbrth, na.rm = TRUE)) %>%
  mutate(cows_deaths=sum(cows_death,na.rm = TRUE)) %>%
  mutate(sheep_birth=sum(shpbrth))

