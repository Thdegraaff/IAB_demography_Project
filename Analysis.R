############## Set random number allocator ######
set.seed(2)
############### Reading necessary libraries #############
library(dplyr)
library(tidyr)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(ggmap)
library(plm)
library(AER)
library(lmtest)
library(sandwich)
library(flexmix)
library(texreg)
library(stargazer)
library(foreign)
library(survey)
############### Reading in the Data #############

EmpData <- read.csv("./Data/EmpData.csv", header = TRUE, sep = ",")
EmpData <- arrange(EmpData, year)
#EmpData <- svydesign(id=~-1, strata = NULL, weights = ~EmpData$popshare, data=EmpData) 

############### Analyses #################

# I need to reestimate everything now the data is correctly arranged at year!

############### First general regressions ######

m3 <- plm(logemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m4 <- plm(logemprate~logyouthshare + logLhat, data = EmpData, model = "fd", index = c("lmr_id","year"))
m5 <- plm(logemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m6 <- plm(logemprate~logyouthshare + logyouthsharefor + logLhat|logLhat + loginstrument + logforinstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m7 <- plm(logunemprate~logyouthshare + logLhat, data = EmpData, model = "fd", index = c("lmr_id","year"))
m8 <- plm(logunemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# 1 Heterogeneous impact of ethnicity

m9 <- plm(logforemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m10 <- plm(loggeremprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# 2 Heterogeneous impact of ethnicity: Impact of foreign youth on native employment

m10a <- plm(loggeremprate~logyouthsharefor + logLhat|logLhat + logforinstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
#!!perhaps we should include here age-specific employment impact of foreigners on natives??

# Heterogeneous impact of age cohort

m11 <- plm(logemprate2024~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m12 <- plm(logemprate2534~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m13 <- plm(logemprate3544~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m14 <- plm(logemprate4554~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m14a <- plm(logemprate5564~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# Heterogeneous impact by task

m15 <- plm(logemptask1~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m16 <- plm(logemptask2~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m17 <- plm(logemptask3~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m18 <- plm(logemptask4~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m19 <- plm(logemptask5~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m20 <- plm(logemptask6~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m21 <- plm(logemptask7~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m22 <- plm(logemptask8~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m23 <- plm(logemptask9~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m24 <- plm(logemptask10~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m25 <- plm(logemptask11~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m26 <- plm(logemptask12~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# Impact on sectors

m27 <- plm(logempoccup1~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m28 <- plm(logempoccup2~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m29 <- plm(logempoccup3~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m30 <- plm(logempoccup4~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m31 <- plm(logempoccup5~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m32 <- plm(logempoccup6~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

############### Get robust standard errors with default HC3 method (different than Stata who uses HC1 method) #######

# robust_se_m4 <- sqrt(diag(vcovHC(m4, type = "HC3")))
# robust_se_m5 <- sqrt(diag(vcovHC(m5, type = "HC3")))
# robust_se_m7 <- sqrt(diag(vcovHC(m7, type = "HC3")))
# robust_se_m8 <- sqrt(diag(vcovHC(m8, type = "HC3")))
# robust_se_m9 <- sqrt(diag(vcovHC(m9, type = "HC3")))
# robust_se_m10 <- sqrt(diag(vcovHC(m10, type = "HC3")))
# robust_se_m10a <- sqrt(diag(vcovHC(m10a, type = "HC3")))
# robust_se_m11 <- sqrt(diag(vcovHC(m11, type = "HC3")))
# robust_se_m12 <- sqrt(diag(vcovHC(m12, type = "HC3")))
# robust_se_m13 <- sqrt(diag(vcovHC(m13, type = "HC3")))
# robust_se_m14 <- sqrt(diag(vcovHC(m14, type = "HC3")))
# robust_se_m14a<- sqrt(diag(vcovHC(m14a, type = "HC3")))
# robust_se_m15 <- sqrt(diag(vcovHC(m15, type = "HC3")))
# robust_se_m16 <- sqrt(diag(vcovHC(m16, type = "HC3")))
# robust_se_m17 <- sqrt(diag(vcovHC(m17, type = "HC3")))
# robust_se_m18 <- sqrt(diag(vcovHC(m18, type = "HC3")))
# robust_se_m19 <- sqrt(diag(vcovHC(m19, type = "HC3")))
# robust_se_m20 <- sqrt(diag(vcovHC(m20, type = "HC3")))
# robust_se_m21 <- sqrt(diag(vcovHC(m21, type = "HC3")))
# robust_se_m22 <- sqrt(diag(vcovHC(m22, type = "HC3")))
# robust_se_m23 <- sqrt(diag(vcovHC(m23, type = "HC3")))
# robust_se_m24 <- sqrt(diag(vcovHC(m24, type = "HC3")))
# robust_se_m25 <- sqrt(diag(vcovHC(m25, type = "HC3")))
# robust_se_m26 <- sqrt(diag(vcovHC(m26, type = "HC3")))
# robust_se_m27 <- sqrt(diag(vcovHC(m27, type = "HC3")))
# robust_se_m28 <- sqrt(diag(vcovHC(m28, type = "HC3")))
# robust_se_m29 <- sqrt(diag(vcovHC(m29, type = "HC3")))
# robust_se_m30 <- sqrt(diag(vcovHC(m30, type = "HC3")))
# robust_se_m31 <- sqrt(diag(vcovHC(m31, type = "HC3")))
# robust_se_m32 <- sqrt(diag(vcovHC(m32, type = "HC3")))
# 
# 
# coefm2 <- coeftest(m2, vcov=vcovHC)
# coefm4 <- coeftest(m4, vcovHC(m4,"HC3"))
# coefm5 <- coeftest(m5, vcovHC(m5,"HC3"))
# coefm7 <- coeftest(m7, vcovHC(m7,"HC3"))
# coefm8 <- coeftest(m8, vcovHC(m8,"HC3"))
# coefm9 <- coeftest(m9, vcovHC(m9,"HC3"))
# coefm10 <- coeftest(m10, vcovHC(m10,"HC3"))
# coefm10a <- coeftest(m10a, vcovHC(m10a,"HC3"))
# coefm11 <- coeftest(m11, vcovHC(m11,"HC3"))
# coefm12 <- coeftest(m12, vcovHC(m12,"HC3"))
# coefm13 <- coeftest(m13, vcovHC(m13,"HC3"))
# coefm14 <- coeftest(m14, vcovHC(m14,"HC3"))
# coefm14a<- coeftest(m14a, vcovHC(m14a,"HC3"))
# coefm15 <- coeftest(m15, vcovHC(m15,"HC3"))
# coefm16 <- coeftest(m16, vcovHC(m16,"HC3"))
# coefm17 <- coeftest(m17, vcovHC(m17,"HC3"))
# coefm18 <- coeftest(m18, vcovHC(m18,"HC3"))
# coefm19 <- coeftest(m19, vcovHC(m19,"HC3"))
# coefm20 <- coeftest(m20, vcovHC(m20,"HC3"))
# coefm21 <- coeftest(m21, vcovHC(m21,"HC3"))
# coefm22 <- coeftest(m22, vcovHC(m22,"HC3"))
# coefm23 <- coeftest(m23, vcovHC(m23,"HC3"))
# coefm24 <- coeftest(m24, vcovHC(m24,"HC3"))
# coefm25 <- coeftest(m25, vcovHC(m25,"HC3"))
# coefm26 <- coeftest(m26, vcovHC(m26,"HC3"))
# coefm27 <- coeftest(m27, vcovHC(m27,"HC3"))
# coefm28 <- coeftest(m28, vcovHC(m28,"HC3"))
# coefm29 <- coeftest(m29, vcovHC(m29,"HC3"))
# coefm30 <- coeftest(m30, vcovHC(m30,"HC3"))
# coefm31 <- coeftest(m31, vcovHC(m31,"HC3"))
# coefm32 <- coeftest(m32, vcovHC(m32,"HC3"))

# Robustness Checks 1: Shimer's (2001) definition

m33 <- plm(logemprate_Shimer~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m34 <- plm(logunemprate_Shimer~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# robust_se_m33 <- sqrt(diag(vcovHC(m33, type = "HC3")))
# robust_se_m34 <- sqrt(diag(vcovHC(m34, type = "HC3")))
# coefm33 <- coeftest(m33, vcovHC(m33,"HC3"))
# coefm34 <- coeftest(m34, vcovHC(m34,"HC3"))

# Robustness Checks 2.1: Role of Youth share on age specific employment

m35 <- plm(logemprate2024~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m36 <- plm(logemprate2529~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# robust_se_m35 <- sqrt(diag(vcovHC(m35, type = "HC3")))
# robust_se_m36 <- sqrt(diag(vcovHC(m36, type = "HC3")))
# coefm35 <- coeftest(m35, vcovHC(m35,"HC3"))
# coefm36 <- coeftest(m36, vcovHC(m36,"HC3"))

# Robustness Checks 2.2: Role of Youngest Cohort and Excl University Towns
#m35a <- ivreg(logemprate~logyouthshare_Y1 + logLhat + factor(year)+factor(lmr_id)|.-logyouthshare_Y1 + logLhat+loginstrument, data = EmpData, weights=popshare) 
#m36a <- ivreg(logemprate~logyouthshare_Y2 + logLhat + factor(year)+factor(lmr_id)|.-logyouthshare_Y2 + logLhat+loginstrument, data = EmpData, weights=popshare) 

#robust_se_m35a <- sqrt(diag(vcovHC(m35a, type = "HC3")))
#robust_se_m36a <- sqrt(diag(vcovHC(m36a, type = "HC3")))
#coefm35a <- coeftest(m35a, vcovHC(m35a,"HC3"))
#coefm36a <- coeftest(m36a, vcovHC(m36a,"HC3"))


# We exclude the regions where the major economic activity is driven by universities
UniTowns <- c(44, 125, 74, 8, 44, 86, 90)
#Now run ReadData.R until line 'Saving Data' and continue regression analysis !!!!!!
EmpData_UniTowns <- EmpData %>% filter(!lmr_id %in% UniTowns)

m37 <- plm(logemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData_UniTowns, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# robust_se_m37 <- sqrt(diag(vcovHC(m37, type = "HC3")))
# coefm37 <- coeftest(m37, vcovHC(m37,"HC3"))


# Robustness Checks 3: Excl Eastern Germany
#First we exclude the eastern German regions

EastGermany <- c(8, 9, 13, 93, 94, 104:141)
#Now run ReadData.R until line 'Saving Data' and continue regression analysis !!!!!
EmpData_EastGermany <- EmpData %>% filter(!lmr_id %in% EastGermany)

m38 <- plm(logemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData_EastGermany, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m39 <- plm(logunemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData_EastGermany, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# robust_se_m38 <- sqrt(diag(vcovHC(m38, type = "HC3")))
# robust_se_m39 <- sqrt(diag(vcovHC(m39, type = "HC3")))
# coefm38 <- coeftest(m38, vcovHC(m38,"HC3"))
# coefm39 <- coeftest(m39, vcovHC(m39,"HC3"))

# Robustness Checks 4: Exluding the crisis

EmpData_nocrisis <- EmpData %>% filter(year <= 2007)

m40 <- plm(logemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData_nocrisis, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")
m41 <- plm(logunemprate~logyouthshare + logLhat|logLhat + loginstrument, data = EmpData_nocrisis, model = "fd", index = c("lmr_id","year"), inst.method="baltagi")

# robust_se_m40 <- sqrt(diag(vcovHC(m40, type = "HC3")))
# robust_se_m41 <- sqrt(diag(vcovHC(m41, type = "HC3")))
# coefm40 <- coeftest(m40, vcovHC(m40,"HC3"))
# coefm41 <- coeftest(m41, vcovHC(m41,"HC3"))

############### Write results to Latex file ###########
# Be careful with overwriting (all additional fixed effects are estimated as factors because of population weights)

stargazer(m4, m5, m7, m8, style = "demography",
           align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", 
           dep.var.labels = c("log(employment)", "log(unemployment)"), 
           out = "./Output/GenericImpact.tex",
           title ="Generic impact of youth share (18--64) on log((un-)employment rate)",
           covariate.labels = c("log(Youth share)", "$\\hat{L}_{rt}$"), 
           add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                            c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                            c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
           #notes = ""
 )

stargazer(m10, m9, m10a, style = "demography",
          align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("natives", "foreigner", "natives"), 
          out = "./Output/EthnicImpact.tex",
          title ="Generic impact of youth share (18--64) on log(employment rates) by ethnicity",
          covariate.labels = c("log(Youth share)", "log(Youth share)$_{\\text{foreigners}}$","$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)

stargazer(m33, m34, style = "demography",
          align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("log(employment)", "log(unemployment)"), 
          out = "./Output/ShimerDefinition.tex",
          title ="Generic impact of youth share (18--64) on employment rates by using Shimer's definition",
          covariate.labels = c("log(Youth share)", "$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)

stargazer(m38, m39, style = "demography",
          align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant", "logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("log(employment)", "log(unemployment)"), 
          out = "./output/ExclEasternGermanyImpact.tex",
          title ="Generic impact of youth share (18--64) on employment rates excluding Eastern Germany",
          covariate.labels = c("log(Youth share)", "$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)

stargazer(m35, m36, m37,  style = "demography",
          align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant", "logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("Youth share (20--24)", "Youth share (25--29)", "Excl. uni. towns"), 
          out = "./output/YoungestImpact.tex",
          title ="Generic impact of youth share on employment rates by altered definition of youth share",
          covariate.labels = c("log(Youth share)", "$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)

stargazer(m11, m12, m13, m14, m14a, style = "demography",
          align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("Age 20--25", "Age 25--35", "Age 30--45", "Age 45--55", "Age 55--65"), 
          out = "./output/CohortImpact.tex",
          title ="Generic impact of youth share (18--64) on employment rate of specific age cohorts",
          covariate.labels = c("log(Youth share)", "$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)

stargazer(m15, m16, m17,m18, m19, m20, m21,m22, m23, m24, m25, m26, 
           style = "demography",
          keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", digits=2,
          dep.var.labels = c("Agra.", "S man.","Q. man.","Tech.", "Eng.","S serv.", "Q. serv.",
                             "S. prof.","Prof.","S. adm.","Q. adm.", "Man"),
          out = "./output/TaskImpact.tex",
          title ="Generic impact of youth share (18--64) on employment rates by task complexity",
          covariate.labels = c("log(YS)", "$\\hat{L}_{rt}$")
          , 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = ""
)


stargazer(m27, m28, m29, m30, m31, m32, style = "demography",
          keep.stat=c("rsq","n"), no.space=TRUE, omit = c("factor","Constant","logLhat"), model.numbers = FALSE, notes.align = "l", 
          dep.var.labels = c("Agri", "Mining", "Prod.", "Technical", "Services", "Other"),
          out = "Output/SectorImpact.tex",
          title ="Generic impact of youth share (18--64) on employment rates by sectors",
          covariate.labels = c("log(YS)", "$\\hat{L}_{rt}$"), 
          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
                           c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = "" 
)


# Create first difference variables and select population share minus the first year
logunemprate_lag <- diff(EmpData$logunemprate, 141, 1)
logemprate_lag <- diff(EmpData$logemprate, 141, 1)
loginstrument_lag <- diff(EmpData$loginstrument, 141, 1)
logyouthshare_lag <- diff(EmpData$logyouthshare, 141, 1)
bartik <- diff(EmpData$logLhat, 141,1)
popshare <- EmpData$popshare[142:length(EmpData$popshare)]

# Create as well first difference of years and make variables of the factor
years <- model.matrix(~factor(EmpData$year)-1)
years <- years[,2:ncol(years)]
years <- diff(years, 141, 1)

# Delete the first year from the dataset to keep trac of esp. id variable
EmpData <- EmpData %>% filter(year >= 2002)
lmr_id <- EmpData$lmr_id

# Multiply all first difference variables with square root of popshare to weight
logemprate_transform <- logemprate_lag*sqrt(EmpData$popshare)
logunemprate_transform <- logunemprate_lag*sqrt(EmpData$popshare)
loginstrument_transform <- loginstrument_lag*sqrt(EmpData$popshare)
logyouthshare_transform <- logyouthshare_lag*sqrt(EmpData$popshare)
bartik_transform <- bartik*sqrt(EmpData$popshare)
years_transform <- years*sqrt(EmpData$popshare)

# Important: Delete one year from the variables (otherwise we have a constant again)
years_transform <- years_transform[,2:ncol(years_transform)]

##################### Extension of Garloff et al. with fmm ####################

f2 <- stepFlexmix(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform|lmr_id, k=1:8, nrep=5)

pdf("./Figs/Convergence.pdf")
plot(f2)
dev.off()

f <- getModel(f2, which="4")
#f<- relabel(f, by = "model", which="logyouthsharetransform")

pdf("./Figs/Rootogram.pdf")
plot(f)
dev.off()

Probs <- data.frame(posterior(f))
EmpData$Cluster <- clusters(f)
f <- refit(f, method = "mstep")
Nocluster <- f@k
Probs$lmr_id <- EmpData$lmr_id

# Take the mean over the various periods
clustermean <- EmpData %>% group_by(lmr_id) %>% 
    summarise(clustermean=mean(Cluster))
Probabilities <-  Probs %>% group_by(lmr_id) %>% 
    summarise(PrCluster1=mean(X1),
              PrCluster2=mean(X2),
              PrCluster3=mean(X3),
              PrCluster4=mean(X4))
clustermean$id <- clustermean$lmr_id

# Write clustermean to be used by Ceren and Probabilities to be used by Dani
write.dta(clustermean, "Data/Clusters.dta")
write.dta(Probabilities, "Data/ProbabilitiesFMM.dta")
write.csv(Probabilities, "Data/ProbabilitiesFMM.csv")

##################### Use the instrument on the 4 clusters! ##################################

cluster1 <- lm(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform, weights = Probs[,2])
iv_cluster1 <- ivreg(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform|
                                         0 + bartik_transform + loginstrument_transform + years_transform, weights = Probs[,2])

cluster2 <- lm(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform, weights = Probs[,3])
iv_cluster2 <- ivreg(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform|
                         0 + bartik_transform + loginstrument_transform + years_transform, weights = Probs[,3])
                                        
cluster3 <- lm(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform, weights = Probs[,4])
iv_cluster3 <- ivreg(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform|
                         0 + bartik_transform + loginstrument_transform + years_transform, weights = Probs[,4])

cluster4 <- lm(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform, weights = Probs[,5])
iv_cluster4 <- ivreg(logemprate_transform~0 + bartik_transform + logyouthshare_transform + years_transform|
                         0 + bartik_transform + loginstrument_transform + years_transform, weights = Probs[,5])

stargazer(cluster1, cluster2, cluster3, cluster4, iv_cluster1, iv_cluster2, iv_cluster3, iv_cluster4, style = "demography",
          keep.stat=c("rsq","n"), no.space=TRUE, omit = c("years","bartik"), model.numbers = TRUE, notes.align = "l", 
          custom.labels = c("1", "2", "3", "4", "1", "2", "3", "4"),
          out = "Output/ClusterIV.tex",
          title ="Generic impact of youth share (18--64) on employment rates by clusters (OLS = 1--4; IV = 5--8)",
          covariate.labels = c("log(YS)")
#          add.lines = list(c("First differenced", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
 #                          c("Time dummies", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"),
 #                          c("Bartik index", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}", "\\text{Yes}"))
          #notes = "" 
)

##################### Read in the shape file #################################################

RegionData <- filter(EmpData,year==2010) # Just pick one year
Regions <- readOGR(dsn = "Data/AMR_2012", layer = "AMR_2012") # Read in shape file
centroids <- as.data.frame(coordinates(Regions)) # Get centroids of regions for labels
names(centroids) <- c("Longitude", "Latitude")  # Rename x and y coordinates
centroids$id <- Regions$lmr2_id  #  Generate id variable

Regions <- fortify(Regions, region="lmr2_id") # Create large data frame from shape file
Regions$id <- strtoi(Regions$id, base = 0L)   # id should be an integer and not a string
RegionData$id <- RegionData$lmr_id # Generate ID variable for database

RegionData <- left_join(RegionData, centroids, by="id") # Join database with centroids
RegionData <- left_join(RegionData, clustermean, by="id") # Join database with clustermean
Regions <- left_join(Regions, RegionData, by="id") # Join shape file with database
 
# ###################### Create general lay-out for cluster map #####################################
# To see other colors in palette run: RColorBrewer::display.brewer.all()

p <- ggplot() + scale_fill_distiller(palette = "Paired", breaks = pretty_breaks(n = Nocluster-1), direction=1)
p <- p + guides(fill = guide_legend(reverse = TRUE))
p <- p + theme_nothing(legend=TRUE)
p <- p + theme(plot.title = element_text(size = rel(2), colour = "black"))

# ######################## Create cluster map ###########################

p_clusters <- p +  geom_polygon(data=Regions, aes(x= long, y = lat, group = group, fill = clustermean), color = "black", size = 0.25) +
    labs(title = "Spatial distribution of clusters across Germany", fill = "") +
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
p_clusters
ggsave(filename = "Figs/Clusters.pdf", width = 9.65, height = 11)

