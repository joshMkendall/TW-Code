setwd("C:/Users/JoshKendall/Third Way/Economic Program - Documents/Two economies/Josh's Code and Data")
library(ipumsr)
library(tidyverse)

#IPUMS CPS 2022
ddi_cps <- read_ipums_ddi("cps_00015.xml")
data_cps <- read_ipums_micro(ddi_cps)
#IPUMS USA 2021 (latest year)
ddi_usa <- read_ipums_ddi("usa_00003.xml")
data_usa <- read_ipums_micro(ddi_usa)
#IPUMS CPS 2019
ddi_19 <- read_ipums_ddi("cps_00020.xml")
cps_19 <- read_ipums_micro(ddi_19)

data_usa <- data_usa %>%
  mutate(ARRdark = factor(case_when(
    ARRIVES %in% c(1900:2359, 0004:0700) ~ "Dark",
    ARRIVES==0000 ~ "NIU",
    TRUE ~ "Light"
  ))) %>%
  mutate(shifts = case_when(
    ARRIVES %in% c(1900:2359) ~ "Night Shift",
    ARRIVES %in% c(0400:0600) ~ "Early Morning",
    ARRIVES %in% c(0601:0700) ~ "6-7",
    TRUE ~ "drop"
  )) %>%
  mutate(WFH = ifelse(TRANWORK==80, 1, 0)) %>%
  mutate(DEG = case_when(
    DEGFIELD == 0 ~ "No Degree",
    TRUE ~ "Degree"
  )) %>%
  mutate(racecl4 = case_when(
    HISPAN!=0 ~ "Hispanic",
    HISPAN==0 & RACE==2 ~ "Black",
    HISPAN==0 & RACE==3 ~ "American Indian or Alaska Native",
    HISPAN==0 & RACE%in%c(4:6) ~ "Asian",
    HISPAN==0 & RACE==1 ~ "White",
    TRUE ~ "Other"
  )) %>%
  mutate(DEG2 = ifelse(EDUC>=10, "College", "Non-college"))

data_cps <- data_cps %>%
  mutate(DEG = case_when(
    EDUC %in% c(111,123,124,125) ~ "Degree",
    TRUE ~ "No degree"
  )) %>%
  mutate(MOVEbin = ifelse(WHYMOVE==0, 0,1)) %>%
  mutate(MOVEpushpull = case_when(
    WHYMOVE %in% c(2,4,6,7,9,10,11,14,15,20) ~ "Pull",
    WHYMOVE==1 & MARST %in% c(1,2) ~ "Pull", 
    WHYMOVE %in% c(0,3,8,17) ~ "Drop",
    TRUE ~ "Push"
  )) %>%
  ungroup()
cps_19 <- cps_19 %>%
  mutate(DEG = ifelse(EDUC %in% c(111,123,124,125), "Degree", "No degree"))

#Filtering for Age and Year
data_cps_22 <- filter(data_cps, YEAR==2022)
data_cps_22 <- filter(data_cps_22, AGE %in% c(25:64))
data_cps_22_heads <- filter(data_cps_22, RELATE==0101)

data_usa_cohort <- filter(data_usa, AGE %in% c(25:64))

#Counting households for SCF percentages
cps_19_heads <- filter(cps_19, PERNUM==1)
group_by(filter(cps_19_heads, AGE %in% c(55:64)), DEG) %>%
  summarize(count = sum(ASECWTH))

group_by(filter(cps_19_heads, AGE %in% c(25:64)), DEG) %>%
  summarize(count = sum(ASECWTH))

#Home Ownership ~1% is no cash rent no own
group_by(data_cps_22_heads, DEG, OWNERSHP) %>%
  summarize(count = sum(ASECWTH)) %>%
  mutate(pct = 100*count/sum(count))

#Health Insurance
group_by(data_cps_22, DEG, ANYCOVNW) %>% #Any Coverage
  summarize(count=sum(ASECWT)) %>%
  mutate(pct = 100*count/sum(count))
group_by(data_cps_22, DEG, PUBCOVNW) %>% # Public Insurance
  summarize(count=sum(ASECWT)) %>%
  mutate(pct = 100*count/sum(count))
group_by(filter(data_cps_22, ANYCOVNW==1 & PUBCOVNW!=2), DEG, PRVTCOVNW) %>% # Private/Employer Insurance
  summarize(count=sum(ASECWT)) %>%
  mutate(pct = 100*count/sum(count))

#Why did you move?
group_by(data_cps_22, DEG, MOVEbin) %>%
  summarize(count=sum(ASECWT)) %>%
  mutate(pct = 100*count/sum(count))
group_by(filter(data_cps_22, WHYMOVE!=0), DEG, WHYMOVE) %>%
  summarize(count=sum(ASECWT)) %>%
  mutate(pct = 100*count/sum(count)) %>%
  print(n=42)
group_by(filter(data_cps_22, MOVEpushpull!="Drop"), DEG, MOVEpushpull) %>%
  summarize(count=sum(ASECWT)) %>%
  mutate(pct=100*count/sum(count))


#Goes to work in the dark (a lot of people are NIU)
group_by(filter(data_usa_cohort, ARRdark!="NIU"), DEG, ARRdark) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count))

group_by(filter(data_usa_cohort, ARRdark=="Dark"), DEG, shifts) %>%
  summarize(count=sum(PERWT)) %>%
  mutate(pct=100*count/sum(count))

group_by(filter(data_usa_cohort, ARRdark=="Dark"), DEG, SEX) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count))

group_by(filter(data_usa_cohort, ARRdark=="Dark"), DEG, racecl4) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count))

group_by(filter(data_usa_cohort, ARRdark=="Dark"), racecl4, DEG) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count))

group_by(filter(data_usa_cohort, ARRdark!="NIU"), racecl4, ARRdark, DEG) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count)) %>%
  print(n=40)

emps <- filter(data_usa_cohort, EMPSTAT==1)

sum(emps$PERWT)
group_by(emps, SEX) %>%
  summarize(count=sum(PERWT)) %>%
  mutate(pct=100*count/sum(count))
group_by(emps, DEG, SEX) %>%
  summarize(count=sum(PERWT)) %>%
  mutate(pct=100*count/sum(count))
group_by(filter(emps, ARRdark=="Dark"), DEG, SEX) %>%
  summarize(count = sum(PERWT)) %>%
  mutate(pct = 100*count/sum(count))



#Actually that's kind of reasonable
view <- filter(data_usa, racecl4=="American Indian or Alaska Native")
view <- filter(data_usa_cohort, ARRdark=="Dark" & racecl4=="American Indian or Alaska Native")
sum(view$PERWT)

view2 <- filter(data_usa, racecl4=="White")
view2 <- filter(data_usa_cohort, racecl4=="White" & ARRdark=="Dark")
