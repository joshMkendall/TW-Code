setwd("C:/Users/JoshKendall/OneDrive - Third Way/Documents/Professional Development/R/Social Security Haircut")
library(tidyverse)

econraw <- read.csv("DP03_1yr_500.csv")
demoraw <- read.csv("DP05_1yr_500.csv")
socialraw <- read.csv("DP02_1yr_500.csv")

econSub <- filter(econraw, PROFLN %in% c(3.0, 4.0, 5.0, 66.0, 67.0, 70.0, 71.0, 74.0)) %>% select(GEONAME, TITLE, PRF_ESTIMATE)
econSub$PRF_ESTIMATE <- as.numeric(gsub(",","",econSub$PRF_ESTIMATE))

demoSub <- filter(demoraw, PROFLN %in% c(1.0, 13.0:17.0)) %>% select(GEONAME, TITLE, PRF_ESTIMATE)
demoSub$PRF_ESTIMATE <- as.numeric(gsub(",","",demoSub$PRF_ESTIMATE))

socialSub <- filter(socialraw, PROFLN==70.0) %>% select(GEONAME, TITLE, PRF_ESTIMATE)
socialSub$PRF_ESTIMATE <- as.numeric(gsub(",","", socialSub$PRF_ESTIMATE))

econSub <- econSub |>
  pivot_wider(names_from = TITLE, values_from=PRF_ESTIMATE)
demoSub <- demoSub |>
  pivot_wider(names_from = TITLE, values_from=PRF_ESTIMATE) |>
  mutate(`Near Retirement` = `55 to 59 years`+`60 to 64 years`) |>
  mutate(`Over 64` = `65 to 74 years`+`75 to 84 years`+`85 years and over`) |>
  select(GEONAME, `Total population`, `Near Retirement`, `Over 64`)
socialSub <- socialSub |>
  pivot_wider(names_from = TITLE, values_from = PRF_ESTIMATE)


districts <- left_join(econSub, demoSub, by="GEONAME")
districts <- left_join(districts, socialSub, by="GEONAME")

natpop <- sum(districts$`Total population`)

districts <- districts |>
  mutate(districtBenefits = (`Mean Social Security income (dollars)`*`With Social Security`)*1.059*1.087) %>%
  mutate(SSIsum = (`Mean Supplemental Security Income (dollars)`*`With Supplemental Security Income`*1.059*1.087)) %>%
  mutate(`Unemployment Rate` = 100*Unemployed/`Civilian labor force`) %>%
  mutate(`Default Job Loss` = (`Civilian labor force`*(`Unemployment Rate`+(3.8)/2)/100)-Unemployed) 

out <- select(districts, GEONAME, `Default Job Loss`, `Near Retirement`, `With Social Security`, `districtBenefits`, 
              `With Supplemental Security Income`, `SSIsum`, `With Food Stamp/SNAP benefits in the past 12 months`, `Civilian veterans`)

debtincreasepercap <- 850000000000/sum(districts$`Total population`) 

write.csv(out, "C:\\Users\\JoshKendall\\OneDrive - Third Way\\Documents\\Professional Development\\R\\Social Security Haircut\\Dominoes Districts.csv")

