############## Set random number allocator ######
set.seed(2)
############### Reading in the Data #############

EmpData = read.csv("Data/EmpData.csv")

############### Analyses #################

m1 <- lm(logemprate~logyouthshare, data = EmpData, weights = popshare)
m2 <- plm(logemprate~logyouthshare, data = EmpData, effect = "twoways",index = c("lmr_id","year"))
m3 <- plm(logemprate~logyouthshare|loginstrument, data = EmpData, effect = "twoways", index = c("lmr_id","year"), inst.method="baltagi")
m4 <- lm(logemprate~logyouthshare + factor(lmr_id) + factor(year), data = EmpData, weights=popshare)
m5 <- ivreg(logemprate~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare)
m6 <- plm(logemprate~logyouthshare:lmr_id, data = EmpData, effect = "twoways",index = c("lmr_id","year"))
m7 <- lm(logunemprate~logyouthshare + factor(lmr_id) + factor(year), data = EmpData, weights=popshare)
m8 <- ivreg(logunemprate~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 

# Heterogeneous impact of ethnicity

m9 <- ivreg(logforemprate~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m10 <- ivreg(loggeremprate~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 

# Heterogeneous impact of age cohort

m11 <- ivreg(logemprate2030~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m12 <- ivreg(logemprate3040~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m13 <- ivreg(logemprate4050~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m14 <- ivreg(logemprate5065~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 

# Heterogeneous impact by task

m15 <- ivreg(logemptask1~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m16 <- ivreg(logemptask2~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m17 <- ivreg(logemptask3~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m18 <- ivreg(logemptask4~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m19 <- ivreg(logemptask5~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m20 <- ivreg(logemptask6~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m21 <- ivreg(logemptask7~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m22 <- ivreg(logemptask8~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m23 <- ivreg(logemptask9~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m24 <- ivreg(logemptask10~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m25 <- ivreg(logemptask11~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 
m26 <- ivreg(logemptask12~logyouthshare + factor(year)+factor(lmr_id)|.-logyouthshare+loginstrument, data = EmpData, weights=popshare) 

############### Get robust standard errors with default HC3 method (different than Stata who uses HC1 method) #######

robust_se_m4 <- sqrt(diag(vcovHC(m4, type = "HC3")))
robust_se_m5 <- sqrt(diag(vcovHC(m5, type = "HC3")))
robust_se_m7 <- sqrt(diag(vcovHC(m7, type = "HC3")))
robust_se_m8 <- sqrt(diag(vcovHC(m8, type = "HC3")))
robust_se_m9 <- sqrt(diag(vcovHC(m9, type = "HC3")))
robust_se_m10 <- sqrt(diag(vcovHC(m10, type = "HC3")))
robust_se_m11 <- sqrt(diag(vcovHC(m11, type = "HC3")))
robust_se_m12 <- sqrt(diag(vcovHC(m12, type = "HC3")))
robust_se_m13 <- sqrt(diag(vcovHC(m13, type = "HC3")))
robust_se_m14 <- sqrt(diag(vcovHC(m14, type = "HC3")))
robust_se_m15 <- sqrt(diag(vcovHC(m15, type = "HC3")))
robust_se_m16 <- sqrt(diag(vcovHC(m16, type = "HC3")))
robust_se_m17 <- sqrt(diag(vcovHC(m17, type = "HC3")))
robust_se_m18 <- sqrt(diag(vcovHC(m18, type = "HC3")))
robust_se_m19 <- sqrt(diag(vcovHC(m19, type = "HC3")))
robust_se_m20 <- sqrt(diag(vcovHC(m20, type = "HC3")))
robust_se_m21 <- sqrt(diag(vcovHC(m21, type = "HC3")))
robust_se_m22 <- sqrt(diag(vcovHC(m22, type = "HC3")))
robust_se_m23 <- sqrt(diag(vcovHC(m23, type = "HC3")))
robust_se_m24 <- sqrt(diag(vcovHC(m24, type = "HC3")))
robust_se_m25 <- sqrt(diag(vcovHC(m25, type = "HC3")))
robust_se_m26 <- sqrt(diag(vcovHC(m26, type = "HC3")))

coefm2 <- coeftest(m2, vcov=vcovHC)
coefm4 <- coeftest(m4, vcovHC(m4,"HC3"))
coefm5 <- coeftest(m5, vcovHC(m5,"HC3"))
coefm7 <- coeftest(m7, vcovHC(m7,"HC3"))
coefm8 <- coeftest(m8, vcovHC(m8,"HC3"))
coefm9 <- coeftest(m9, vcovHC(m9,"HC3"))
coefm10 <- coeftest(m10, vcovHC(m10,"HC3"))
coefm11 <- coeftest(m11, vcovHC(m11,"HC3"))
coefm12 <- coeftest(m12, vcovHC(m12,"HC3"))
coefm13 <- coeftest(m13, vcovHC(m13,"HC3"))
coefm14 <- coeftest(m14, vcovHC(m14,"HC3"))
coefm15 <- coeftest(m15, vcovHC(m15,"HC3"))
coefm16 <- coeftest(m16, vcovHC(m16,"HC3"))
coefm17 <- coeftest(m17, vcovHC(m17,"HC3"))
coefm18 <- coeftest(m18, vcovHC(m18,"HC3"))
coefm19 <- coeftest(m19, vcovHC(m19,"HC3"))
coefm20 <- coeftest(m20, vcovHC(m20,"HC3"))
coefm21 <- coeftest(m21, vcovHC(m21,"HC3"))
coefm22 <- coeftest(m22, vcovHC(m22,"HC3"))
coefm23 <- coeftest(m23, vcovHC(m23,"HC3"))
coefm24 <- coeftest(m24, vcovHC(m24,"HC3"))
coefm25 <- coeftest(m25, vcovHC(m25,"HC3"))
coefm26 <- coeftest(m26, vcovHC(m26,"HC3"))

############### Write results to Latex file ###########
# Be careful with overwriting (all additional fixed effects are
# estimated as factors because of population weights)

stargazer(m4, m5, m7, m8, se = list(robust_se_m4, robust_se_m5, robust_se_m7, robust_se_m8), file = "Output/GenericImpact.tex",
          title ="Generic impact of youth shares on (un)employment rate", align=TRUE, keep.stat=c("rsq","n"), no.space=TRUE)

############### Demean the data ###############

# m7 <- lm(logyouthshare~loginstrument, data = EmpData, weights=popshare)
# EmpData$logyouthshare <- predict.lm(m7)

EmpData <- EmpData %>% group_by(year) %>% 
    mutate(
        logemprate_year = mean(logemprate),
        logyouthshare_year = mean(logyouthshare),
        loginstrument_year = mean(loginstrument) 
    )

EmpData <- EmpData %>% group_by(lmr_id) %>% 
    mutate(
        logemprate_id = mean(logemprate),
        logyouthshare_id = mean(logyouthshare),
        loginstrument_id = mean(loginstrument)
    )

EmpData <- EmpData %>% mutate(
            logempratetransform = logemprate - logemprate_year - logemprate_id,
            logyouthsharetransform = logyouthshare - logyouthshare_year - logyouthshare_id, 
            loginstrumenttransform = loginstrument - loginstrument_year - loginstrument_id
)

##################### Extension of Garloff et al. with fmm ####################

# m7 <- lm(logyouthsharetransform~loginstrumenttransform, data = EmpData, weights=popshare)
# EmpData$logyouthsharetransform <- predict.lm(m7)

f1 <- flexmix(logempratetransform~logyouthsharetransform|lmr_id, data = EmpData, k = 4)
f2 <- stepFlexmix(logempratetransform~logyouthsharetransform|lmr_id, data = EmpData, k=2:10, nrep=3)
plot(f2)
ICL(f2)
f <- getModel(f2, which=3)
plot(f)
EmpData$Cluster <- clusters(f)
f <- refit(f, method="mstep")
Nocluster <- f@k

clustermean <- EmpData %>% group_by(lmr_id) %>% 
    summarise(clustermean=mean(Cluster))
clustermean$id <- clustermean$lmr_id

betacoef <- m6$coefficients
betacoef <- data.frame(betacoef, clustermean$id)
betacoef$id <- betacoef$clustermean.id

##################### Read in the shape file #################################################

# RegionData <- filter(EmpData,year==2010) # Just pick one year
# Regions <- readOGR(dsn = "Data/AMR_2012", layer = "AMR_2012") # Read in shape file
# centroids <- as.data.frame(coordinates(Regions)) # Get centroids of regions for labels 
# names(centroids) <- c("Longitude", "Latitude")  # Rename x and y coordinates
# centroids$id <- Regions$lmr2_id  #  Generate id variable
# 
# Regions <- fortify(Regions, region="lmr2_id") # Create large data frame from shape file
# Regions$id <- strtoi(Regions$id, base = 0L)   # id should be an integer and not a string
# RegionData$id <- RegionData$lmr_id # Generate ID variable for database
# 
# RegionData <- left_join(RegionData, centroids, by="id") # Join database with centroids
# RegionData <- left_join(RegionData, clustermean, by="id") # Join database with clustermean
# RegionData <- left_join(RegionData, betacoef, by="id")
# Regions <- left_join(Regions, RegionData, by="id") # Join shape file with database
# 
# ###################### Create general lay-out for figures #####################################
# 
# p <- ggplot() + scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = Nocluster-1), direction=1) 
# p <- p + guides(fill = guide_legend(reverse = TRUE))
# p <- p + theme_nothing(legend=TRUE)
# p <- p + theme(plot.title = element_text(size = rel(2), colour = "black")) 
# 
# p1 <- ggplot() + scale_fill_distiller(palette = "PRGn", breaks = pretty_breaks(n = 10), direction=1) 
# p1 <- p1 + guides(fill = guide_legend(reverse = TRUE))
# p1 <- p1 + theme_nothing(legend=TRUE)
# p1 <- p1 + theme(plot.title = element_text(size = rel(2), colour = "black")) 
# 
# ######################## Create maps ###########################
# 
# p_clusters <- p +  geom_polygon(data=Regions, aes(x= long, y = lat, group = group, fill = clustermean), color = "black", size = 0.25) + 
#     labs(title = "Spatial distribution of clusters across Germany", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/Clusters.pdf", width = 9.65, height = 11)
# 
# p_betacoef <- p1 +  geom_polygon(data=Regions, aes(x= long, y = lat, group = group, fill = betacoef), color = "black", size = 0.25) + 
#     labs(title = "Spatial distribution of beta coefficients across Germany", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/BetaMap.pdf", width = 9.65, height = 11)