############### Reading necessary libraries #############

library(dplyr)
library(plm)
library(AER)
library(flexmix)
library(maptools)
library(RColorBrewer)
set.seed(2)
############### Reading in the Data #############

EmpData = read.csv("Data/EmpData.csv")

############### Transform the data ##############

EmpData <- EmpData %>% mutate(
                popshare = pop/sum(pop),
                Workingpop = pop_20_24 + pop_25_29 + 
                             pop_30_34 + pop_35_39 + pop_40_44 + 
                             pop_45_49 + pop_50_54 + pop_55_59 + 
                             pop_60_64,
                Working = emp_20_24 + emp_25_29 + 
                    emp_30_34 + emp_35_39 + emp_40_44 + 
                    emp_45_49 + emp_50_54 + emp_55_59 + 
                    emp_60_64,
                #ivpop = pop_y14 + pop_15_19 + pop_20_24 + pop_25_29 + 
                #           pop_30_34 + pop_35_39 + pop_40_44 + 
                #            pop_45_49 + pop_50_54,
                logemprate = log(emp/Working),
                logforshare = log(emp_for/Workingpop),
                #logbirthshare = log(pop_y14/ivpop),
                logyouthshare = log((pop_20_24)/Workingpop)
)

############### First Analysis #################

m1 <- lm(logemprate~logyouthshare, data = EmpData, weights = popshare)
m2 <- plm(logemprate~logyouthshare, data = EmpData, effect = "twoways",index = c("lmr_id","year"))
#m3 <- plm(logemprate~logyouthshare|logbirthshare, data = EmpData, effect = "twoways", index = c("district_id","year"), inst.method="baltagi")
m4 <- lm(logemprate~logyouthshare + factor(lmr_id), data = EmpData, weights=popshare)
#m5 <- ivreg(logemprate~logyouthshare + factor(year)+factor(district_id)|.-logyouthshare+logbirthshare, data = EmpData, weights =popshare)

############### Demean the data ###############

EmpData <- EmpData %>% group_by(year) %>% 
    mutate(
        logemprate_year = mean(logemprate),
        logyouthshare_year = mean(logyouthshare),
        logforshare_year  = mean(logforshare)
    )

EmpData <- EmpData %>% group_by(lmr_id) %>% 
    mutate(
        logemprate_id = mean(logemprate),
        logyouthshare_id = mean(logyouthshare),
        logforshare_id = mean(logforshare)
    )

EmpData <- EmpData %>% mutate(
            logempratetransform = logemprate - logemprate_year - logemprate_id,
            logyouthsharetransform = logyouthshare - logyouthshare_year - logyouthshare_id, 
            logforsharetransform = logforshare - logforshare_year - logforshare_id
)

##################### Extension of Garloff et al. with fmm ####################

summary(lm(logempratetransform~logyouthsharetransform, data = EmpData))

f1 <- flexmix(logempratetransform~logyouthsharetransform|lmr_id, data = EmpData, k = 4)
f2 <- stepFlexmix(logempratetransform~logyouthsharetransform|lmr_id, data = EmpData, k=2:10, nrep=3)
plot(f2)
ICL(f2)
f <- getModel(f2, which=4)
plot(f)
EmpData$Cluster <- clusters(f)
f <- refit(f, method="mstep")
Nocluster <- f@k

clustermean <- EmpData %>% group_by(lmr_id) %>% 
    summarise(clustermean=mean(Cluster))

Districts<-readShapePoly("Data/AMR_2012/AMR_2012.shp")
Districts$lmr_id <- Districts$lmr2_id

Districts <- merge(Districts, clustermean, by="lmr_id")
pdf("Figs/ClustersYouth.pdf")
spplot(Districts,"clustermean",col.regions=brewer.pal(Nocluster,"Set1"),cuts = Nocluster-1, main="Regions classification based upon impact Youth")
dev.off()

##################### Extension of Garloff et al. with fmm and foreign employment share ####################

f2 <- flexmix(logempratetransform~logyouthsharetransform+logforsharetransform|district_id, data = EmpData, k = 4)
#f2 <- stepFlexmix(logempratetransform~logyouthsharetransform|district_id, data = EmpData, k=2:10, nrep=3)
plot(f2)
Nocluster <- f2@k
#f <- getModel(f1, which=8)
f2_fit <- refit(f2, method="mstep")

EmpData$Cluster <- clusters(f2)
clustermeanf2 <- EmpData %>% group_by(district_id) %>% 
    summarise(clustermeanf2=mean(Cluster))
#write.dta(clustermeanf2,"Data/clustermeanf2.dta")
#write.dta(EmpData,"Data/EmpData.dta")

Districts<-readShapePoly("Data/krs2013/krs2013.shp")
Districts$district_id <- Districts$KRS1213/1000

Districts <- merge(Districts, clustermeanf2, by="district_id")
pdf("Figs/ClustersYouthForeign.pdf")
spplot(Districts,"clustermeanf2",col.regions=brewer.pal(Nocluster,"Dark2"),cuts = Nocluster-1, main="Regions classification based upon Youth and Foreign employment")
dev.off()

########################### Make map of beta coefficients ###########################################

m2 <- plm(logemprate~logyouthshare:lmr_id, data = EmpData, effect = "twoways",index = c("lmr_id","year"))
myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)
betacoef <- m2$coefficients
district_id <- EmpData %>% group_by(lmr_id) %>% 
    summarise(clustermean=mean(lmr_id))
lmr_id <- clustermean$lmr_id
betacoef <- data.frame(betacoef, lmr_id)
Districts <- merge(Districts, betacoef, by="lmr_id")
pdf("Figs/BetaPlot.pdf")
spplot(Districts,"betacoef",col.regions=myPal,cuts=8, at = seq(-0.1, 0.22, by = 0.01))
dev.off()

