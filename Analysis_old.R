############### Reading necessary libraries #############

library(dplyr)
library(plm)
library(AER)
library(stargazer)

############### Reading in the Data #############

EmpData <- read.csv(file = "Data/EmploymentDistricts2.csv", header = TRUE, sep =",")

############### Transform the data ##############

EmpData <- EmpData %>% mutate(
                popshare = pop/sum(pop),
                Workingpop = pop_15_19 + pop_20_24 + pop_25_29 + 
                             pop_30_34 + pop_35_39 + pop_40_44 + 
                             pop_45_49 + pop_50_54 + pop_55_59 + 
                             pop_60_64,
                ivpop = pop_y14 + pop_15_19 + pop_20_24 + pop_25_29 + 
                            pop_30_34 + pop_35_39 + pop_40_44 + 
                            pop_45_49 + pop_50_54,
                logemprate = log(emp_por/Workingpop),
                loglemprate = log(emp_lskill/Workingpop),
                logmemprate = log(emp_mskill/Workingpop),
                loghemprate = log(emp_hskill/Workingpop),                
                logforshare = log(emp_for/Workingpop),
                loggershare = log(emp_ger/Workingpop),
                logbirthshare = log(pop_y14/ivpop),
                inmigrationrate = icom/pop,
                outmigrationrate= ocom/pop,
                logyouthshare = log((pop_15_19+pop_20_24)/Workingpop),
                logyouthempger = log(emp_germany_15_24/Workingpop),
                logyouthempeu = log(emp_europe_eu_15_24/Workingpop),
                logyouthempafr = log(emp_africa_15_24/Workingpop),
                logyouthempame = log(emp_america_15_24/Workingpop),
                logyouthempasi = log(emp_asia_15_24/Workingpop),
                logyouthempaus = log(emp_australia_15_24/Workingpop),
                logyouthempnoneu = log(emp_europe_noneu_15_24/Workingpop)
)

############### First Analysis #################

m1 <- lm(logemprate~logyouthshare, data = EmpData, weights = popshare)
m2a <- plm(logemprate~logyouthshare, data = EmpData, effect = "twoways",index = c("district_id","year"))
m2a1 <- plm(logemprate~logyouthshare:district_id, data = EmpData, effect = "twoways",index = c("district_id","year"))
m2b <- plm(logemprate~logyouthempger+logyouthempeu+logyouthempafr+logyouthempame+logyouthempasi+logyouthempaus+logyouthempnoneu, 
           data = EmpData, effect = "twoways",index = c("district_id","year"))
m2l <- plm(loglemprate~logyouthempger+logyouthempeu+logyouthempafr+logyouthempame+logyouthempasi+logyouthempaus+logyouthempnoneu, 
           data = EmpData, effect = "twoways",index = c("district_id","year"))
m2m <- plm(logmemprate~logyouthempger+logyouthempeu+logyouthempafr+logyouthempame+logyouthempasi+logyouthempaus+logyouthempnoneu, 
           data = EmpData, effect = "twoways",index = c("district_id","year"))
m2h <- plm(loghemprate~logyouthempger+logyouthempeu+logyouthempafr+logyouthempame+logyouthempasi+logyouthempaus+logyouthempnoneu, 
           data = EmpData, effect = "twoways",index = c("district_id","year"))
m3a <- plm(logemprate~logyouthshare|logbirthshare, data = EmpData, effect = "twoways",
          index = c("district_id","year"), inst.method="baltagi")
m3a <- plm(logemprate~logyouthshare|logbirthshare, data = EmpData, effect = "twoways",
           index = c("district_id","year"), inst.method="baltagi")
m4 <- lm(logemprate~logyouthshare + factor(year)+factor(district_id), data = EmpData, weights=popshare)
m5 <- ivreg(logemprate~logyouthshare + factor(year)+factor(district_id)|.-logyouthshare+logbirthshare, data = EmpData, weights =popshare)

################# Second analysis ####################
# Now look at the impact of employment on population
mp1 <- plm(log(pop)~log(emp_ger)+log(emp_africa_total)+log(emp_asia_total)+
               log(emp_europe_eu_total)+log(emp_europe_noneu_total)+log(emp_america_total), 
           data = EmpData, effect = "twoways",index = c("district_id","year"))
mp1 <- plm(log(emp)~log(pop), data = EmpData, effect = "twoways",index = c("district_id","year"))
