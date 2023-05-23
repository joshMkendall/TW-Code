setwd("C:/Users/JoshKendall/Third Way/Economic Program - Documents/Two economies/Josh's Code and Data")
library(lodown)
library(survey) 
library(mitools)
library(tidyverse)
library(srvyr)
library(modelsummary)
library(mi)

#Only need to run once to download. It's currently in your folder
#scf_cat <- get_catalog("scf", output_dir = file.path(path.expand("~"), "SCF")) 
#scf_cat <- subset(scf_cat, year==2019) #Can be changed to download all SCF data
#scf_cat <- lodown("scf", scf_cat)

scf_imp <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2019.rds" ) )

scf_rw <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2019 rw.rds" ) )

scf_design <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw[ , -1 ] , 
    data = imputationList( scf_imp ) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

#Updating the design with new variables
scf_design <- update(scf_design , 
                     edcl = factor(edcl, labels = 
                                     c("less than high school" , 
                                       "high school or GED" , 
                                       "some college" , 
                                       "college degree" 
                                     )),
                     deg = factor(case_when(
                       edcl=="college degree" ~ "Degree",
                       TRUE ~ "No degree"
                     )),
                     turndown = factor(turndown),
                     #NOTE: The summed variables (networth, debt, assets) are composed of the individual values of the various debts and assets. 
                     #The following calculations are used to isolate specific sub-totals
                     nonMorDEBT = debt-nh_mort, #this debt only accounts for mortgages and home equity loans secured by primary residence. Other variables account for different loans secured by the residence
                     MORperDEBT = case_when(
                       debt>0 ~ nh_mort*100/debt,
                       TRUE ~ -1
                     ),
                     edlBIN = factor(edn_inst>0),
                     DEBTperINC = case_when(
                       income>0 ~ (debt-nh_mort)*100/income,
                       TRUE ~ -1
                     ),
                     MORperINC = case_when(
                       income>0 ~ nh_mort*100/income,
                       TRUE ~ -1
                     ),
                     WORTHnores = networth+nh_mort-houses,
                     ASSETnores = asset-houses,
                     resASSETpct = case_when(
                       asset>0 ~ houses*100/asset,
                       TRUE ~ 0
                     ),
                     HOMEbin = factor(houses>0),
                     pvtret = retqliq - futpen - currpen,
                     pens = futpen + currpen,
                     pvtretBIN = factor(ifelse(pvtret>0,"Has private retirement", "Lacks private retirement")),
                     penBIN = factor(ifelse(pens>0, 1,0)),
                     stockBIN = factor(case_when(
                       stocks>0 | reteq>0 | nmmf>0 ~ 1,
                       TRUE ~ 0
                     ))
)

scf_design_work <- subset(scf_design, age %in% c(25:64))

########################### Renters, owners, and movers ###############################
#Median residence assets
scf_MIcombine( with( subset(scf_design_work, houses!=0) , svyby(
  ~ houses , ~deg  , svyquantile ,
  0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' ,
  keep.var = TRUE , ci = TRUE)
) )
#Average mortgage as a percentage of income
scf_MIcombine(with(subset(scf_design_work, MORperINC!=-1), svyby(~MORperINC, ~deg, svymean)))
#Denied credit tiles
inccatTHRESH <- scf_MIcombine(with(scf_design_work, svyquantile(
  ~income, c(.2,.4,.6,.8), 
  interval.type="quantile", keep.var=TRUE
)))
inccatTHRESHdf <- data.frame(as.list(inccatTHRESH$coefficients))
inccatTHRESHdf <- pivot_longer(inccatTHRESHdf, cols = income.0.2:income.0.8, names_to = "Threshold")

scf_design_work <- update(scf_design_work, #Hardcoded thresholds for simplicity
                          subinccat=case_when(
                            income<=30544 ~ "0-20",
                            income>30544 & income<=52942 ~ "20-40",
                            income>52942 & income<=84097 ~ "40-60",
                            income>84097 & income<=139889 ~ "60-80",
                            TRUE ~ "80-100"
                          ))

#These are categorical variables, so Chi Squared is the way to go
MIsvychisq(~turndown+deg, scf_design_work) #The p is zero but it's just a very small p (Josh can explain if curious)
MIsvychisq(~turndown+deg, subset(scf_design_work, subinccat=="0-20")) # insignificant
MIsvychisq(~turndown+deg, subset(scf_design_work, subinccat=="20-40")) # insignificant
MIsvychisq(~turndown+deg, subset(scf_design_work, subinccat=="40-60")) # significant
MIsvychisq(~turndown+deg, subset(scf_design_work, subinccat=="60-80")) # significant
MIsvychisq(~turndown+deg, subset(scf_design_work, subinccat=="80-100")) # insignificant

temp<-scf_MIcombine(with(scf_design_work, svyby(~turndown, ~subinccat+deg, svytotal)))
tempdf <- data.frame(as.list(temp$coefficients))
tempdf <- pivot_longer(tempdf, cols = X0.20.Degree.turndown0:X80.100.No.degree.turndown1, names_to = "Category")
tempdf2 <- tempdf[11:20,]
colnames(tempdf2)[2] <- 'denied value'
tempdf <- tempdf[1:10,]
tempdf <- cbind(tempdf, tempdf2$`denied value`)
tempdf <- tempdf %>%
  mutate(pct = tempdf2$`denied value`*100/(value+tempdf2$`denied value`))
turndown_graph <- tempdf %>%
  mutate(Deg = factor(if_else(str_detect(Category, "No.degree"),"Non-College", "College"), levels=c("Non-College","College"))) %>%
  mutate(Tile = factor(case_when(
    str_detect(Category, "X0") ~ "Bottom",
    str_detect(Category, "X20") ~ "Second",
    str_detect(Category, "X40") ~ "Third",
    str_detect(Category, "X60") ~ "Fourth",
    TRUE ~ "Highest"
  ), levels=c("Bottom","Second","Third","Fourth","Highest")))
ggplot() +
  geom_col(data = turndown_graph,
           mapping = aes(x=Tile, y=pct, fill=Deg),
           position = "dodge", stat="identity") +
  theme_bw() +
  labs(title="Non College households are more likely to have been denied credit",
       x="Household income quintile",
       y="Percent of households that have been denied credit in the past 5 years",
       fill="")


############################### Little to no wealth #######################################
#Median household assets
scf_MIcombine(with(scf_design_work, svyby(
  ~asset, ~deg, svyquantile,
  .5, se = TRUE, method = 'constat', interval.type='quantile'
)))
#Median Net worth
scf_MIcombine( with( scf_design_work , svyby(
  ~ networth , ~deg  , svyquantile ,
  0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' ,
  keep.var = TRUE , ci = TRUE)
) )

#Average house as a percentage of assets
scf_MIcombine(with(subset(scf_design_work, resASSETpct!=0), svyby(~resASSETpct, ~deg, svymean)))
#Stock stuff
scf_MIcombine(with(scf_design_work, svyby(~one, ~stockBIN+deg, svytotal)))

#THERE IS A CLEAR CORRELATION BETWEEN HAVING A DEGREE AND ASSETS
#assetcatTHRESH <- scf_MIcombine(with(scf_design_work, svyquantile( #YOU DON'T NEED TO RUN THIS SINCE YOU HARDCODED, KEEP FOR BUG TESTING
#  ~asset, c(.2,.4,.6,.8), 
#  interval.type="quantile", keep.var=TRUE
#)))
#assetcatTHRESHdf <- data.frame(as.list(assetcatTHRESH$coefficients))
#assetcatTHRESHdf <- pivot_longer(assetcatTHRESHdf, cols = asset.0.2:asset.0.8, names_to = "Threshold")

scf_design_work <- update(scf_design_work,
                          subassetcat=factor(case_when( #Thresholds are hardcoded
                            asset<=18268 ~ "0-20",
                            asset>18268 & asset<=128800 ~ "20-40",
                            asset>128800 & asset<=307502 ~ "40-60",
                            asset>307502 & asset<=655138 ~ "60-80",
                            TRUE ~ "80-100"
                          )))
quintiles <- scf_MIcombine(with(scf_design_work, svyby(~deg, ~subassetcat, svytotal))) 
quintilesdf <- data.frame(as.list(quintiles$coefficients))
quintilesdf <- pivot_longer(quintilesdf, cols = X0.20.degDegree:X80.100.degNo.degree, names_to = "Category")
quintilesdf <- quintilesdf %>%
  mutate(Deg = factor((if_else(str_detect(Category, "No"),"Non-College", "College")), levels=c("Non-College", "College"))) %>%
  mutate(Tile = factor(case_when(
    str_detect(Category, "X0") ~ "Bottom",
    str_detect(Category, "X20.40") ~ "Second",
    str_detect(Category, "X40.60") ~ "Third",
    str_detect(Category, "X60.80") ~ "Fourth",
    TRUE ~ "Highest"
  ), levels = c("Bottom", "Second","Third","Fourth","Highest")))
quintilesdf <- quintilesdf %>%
  group_by(Tile) %>%
  mutate(subpop = sum(value)) %>%
  ungroup() %>%
  mutate(tilepct = 100*value/subpop)
ggplot(quintilesdf, aes(fill=Deg, y=value, x=Tile)) +
  geom_bar(position="fill", stat="identity") +
  labs(title = "There is a clear correlation between having a degree and amount of assets",
       x="Household asset quintiles",
       y="Share",
       fill="")

#Mortgage as a percentage of debt
scf_MIcombine(with(subset(scf_design_work, MORperDEBT!=-1), svyby(~MORperDEBT, ~deg, svymean)))
#Households with education loan debt
scf_MIcombine(with(scf_design_work, svyby(~one, ~edlBIN+deg, svytotal)))
#Median debt as a percentage of income
scf_MIcombine( with( subset(scf_design_work, DEBTperINC!=-1), svyby(
  ~ DEBTperINC, ~deg, svyquantile ,
  0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' ,
  keep.var = TRUE , ci = TRUE)
) )
scf_MIcombine(with(subset(scf_design_work, DEBTperINC!=-1), svyby(~DEBTperINC, ~deg, svymean)))
#Median nonmortgage debt
scf_MIcombine( with( scf_design_work , svyby(
  ~ nonMorDEBT , ~deg  , svyquantile ,
  0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' ,
  keep.var = TRUE , ci = TRUE)
) )

############################## Reliant on federal retirement programs ###################################
#Median 55-64 private retirement balance
scf_MIcombine( with( subset(scf_design_work, pvtret>0 & age %in% (55:64)) , svyby(
  ~pvtret  , ~deg  , svyquantile ,
  0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' ,
  keep.var = TRUE , ci = TRUE)
) )
#55-64 private retirement count
scf_MIcombine(with(subset(scf_design_work, age %in% c(55:64)), svyby(~one, ~deg+pvtretBIN, svytotal)))
view <- MIsvychisq(~pvtretBIN+deg, subset(scf_design_work, age %in% c(55:64))) #p of 0, Josh can explain
MIsvyttest(pvtret~deg, subset(scf_design_work, age %in% c(55:64) & pvtret>0)) #significant
#Traditional employer pension count
scf_MIcombine(with(scf_design_work, svyby(~one, ~deg+penBIN, svytotal)))
#Saved money in 2018
scf_MIcombine(with(scf_design_work, svyby(~one, ~saved+deg, svytotal)))
