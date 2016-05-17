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
library(Cairo)
library(plm)
library(AER)
library(lmtest)
library(sandwich)
library(flexmix)
library(stargazer)
library(foreign)

############### Reading in the Data #############

EmpData <- read.csv(file = "Data/Employment_lmr.csv", header = TRUE, sep =",")

############### Filter Empdata for years, regions and university towns ################

# We work standard with the dataset 2000-2010, because the year 2011
# seems to be a bit of an outlier.

EastGermany <- c(8, 9, 13, 93, 94, 104:141)
UniTowns <- c(44, 125, 74, 8, 44, 86, 90)
EmpData <- EmpData %>% filter(year <= 2010)
# EmpData <- EmpData %>% filter(!lmr_id %in% EastGermany)
# EmpData <- EmpData %>% filter(!lmr_id %in% UniTowns)

############### Recoding data ###################

EmpData <- EmpData %>% 
    mutate(
        Workingpop = #pop_15_17 + 
            #pop_18_19 + 
            pop_20_24 + 
            pop_25_29 + 
            pop_30_34 + pop_35_39 + pop_40_44 + 
            pop_45_49 + pop_50_54 + pop_55_59 + 
            pop_60_64,
        Workingpopfor = pop_for_20_24 + pop_for_25_29 + 
            pop_for_30_34 + pop_for_35_39 + pop_for_40_44 + 
            pop_for_45_49 + pop_for_50_54 + pop_for_55_59 + 
            pop_for_60_64,
        Workingpopger = pop_ger_20_24 + pop_ger_25_29 + 
            pop_ger_30_34 + pop_ger_35_39 + pop_ger_40_44 + 
            pop_ger_45_49 + pop_ger_50_54 + pop_ger_55_59 + 
            pop_ger_60_64,
        Emp2030 = emp_20_24 + emp_25_29,
        Emp3040 = emp_30_34 + emp_35_39,
        Emp4050 = emp_40_44 + emp_45_49,
        Emp5065 = emp_50_54 + emp_55_59 + emp_65,
        logemprate2030 = log((Emp2030)/(pop_20_24 + pop_25_29)),
        logemprate3040 = log((Emp3040)/(pop_30_34 + pop_35_39)),
        logemprate4050 = log((Emp4050)/(pop_40_44 + pop_45_49)),
        logemprate5065 = log((Emp5065)/(pop_50_54 + pop_55_59 + pop_60_64)),        
        logemptask1 = emp_task_1/emp,
        logemptask2 = emp_task_2/emp,
        logemptask3 = emp_task_3/emp,
        logemptask4 = emp_task_4/emp,
        logemptask5 = emp_task_5/emp,
        logemptask6 = emp_task_6/emp,
        logemptask7 = emp_task_7/emp,
        logemptask8 = emp_task_8/emp,
        logemptask9 = emp_task_9/emp,
        logemptask10 = emp_task_10/emp,
        logemptask11 = emp_task_11/emp,
        logemptask12 = emp_task_12/emp,
        unemprate = unemp/Workingpop,
        unemprate_Shimer = unemp/(emp+unemp),
        emprate = emp/Workingpop,
        emprate_Shimer = emp/(emp+unemp),
        foremprate = emp_for/Workingpopfor,
        geremprate = emp_ger/Workingpopger,
        participationrate = (unemp + emp)/Workingpop,
        youthshare = (pop_18_19 + pop_20_24)/Workingpop,
        popshare = pop/sum(pop),
        logforemprate = log(foremprate),
        loggeremprate = log(geremprate),
        logemprate = log(emprate),
        logemprate_Shimer = log(emprate_Shimer),
        logunemprate = log(unemprate),
        logunemprate_Shimer = log(unemprate_Shimer),
        logyouthshare = log(youthshare), 
        loginstrument = log(pop_18_24_ins2)
    )

############################ Create Bartik Index ################################

nr_regions <- nrow(EmpData)/11
EmpData <- EmpData %>% 
    group_by(lmr_id) %>%
        mutate(emp_lag = lag(emp,1),
           emp_occ1_lag = lag(emp_occ1, na.rm = TRUE),
           emp_occ2_lag = lag(emp_occ2, na.rm = TRUE),
           emp_occ3_lag = lag(emp_occ3, na.rm = TRUE),
           emp_occ4_lag = lag(emp_occ4, na.rm = TRUE),
           emp_occ5_lag = lag(emp_occ5, na.rm = TRUE),
           emp_occ6_lag = lag(emp_occ6, na.rm = TRUE)           
           )
EmpData$emp_occ2_lag[EmpData$emp_occ2_lag == 0] <- 1
EmpData <- EmpData %>%
    group_by(year) %>%
        mutate(
            emp_occ1_gr = sum(emp_occ1/emp_occ1_lag, na.rm = TRUE)/nr_regions,
            emp_occ2_gr = sum(emp_occ2/emp_occ2_lag, na.rm = TRUE)/nr_regions,
            emp_occ3_gr = sum(emp_occ3/emp_occ3_lag, na.rm = TRUE)/nr_regions,
            emp_occ4_gr = sum(emp_occ4/emp_occ4_lag, na.rm = TRUE)/nr_regions,
            emp_occ5_gr = sum(emp_occ5/emp_occ5_lag, na.rm = TRUE)/nr_regions,
            emp_occ6_gr = sum(emp_occ6/emp_occ6_lag, na.rm = TRUE)/nr_regions            
               )
EmpData[is.na(EmpData)] <- 0
EmpData <- EmpData %>%
            mutate(
                Lhat = emp_occ1_gr * emp_occ1_lag +        
                    emp_occ2_gr * emp_occ2_lag +
                    emp_occ3_gr * emp_occ3_lag +
                    emp_occ4_gr * emp_occ4_lag +
                    emp_occ5_gr * emp_occ5_lag +
                    emp_occ6_gr * emp_occ6_lag,                     
                logLhat = log(Lhat)
            )

EmpData <- EmpData %>% filter(year >= 2001)

############### Saving Data ########################

write.csv(EmpData, file="Data/EmpData.csv", row.names=TRUE)

############### Making temporal descriptive plots ##############

ggplot(subset(EmpData, lmr_name %in% c("Berlin", "Stuttgart", "Hamburg", "Frankfurt am Main")),
    aes(x=year, y=youthshare, color=lmr_name))+
    geom_point() + geom_line() + theme_tufte() + labs(x = "Year", y = "Youth population share") + theme(legend.title=element_blank()) +
    #theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    scale_x_continuous(breaks = pretty_breaks(n=11))
ggsave(filename = "Figs/YouthPopulationExamples.pdf", width = 6, height = 4)

YearData <- EmpData %>% group_by(year) %>%
            summarise(meanyouthshare= mean(youthshare),
                      meanemprate = mean(emprate),
                      meanunemprate= mean(unemprate)) #%>%
            #gather("id","value", meanyouthshare:meanunemprate)
ggplot(data = YearData, aes(x=year, y = meanemprate)) +geom_line()

############### Making descriptive maps #################

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
# Regions <- left_join(Regions, RegionData, by="id") # Join shape file with database
# 
# ###################### Create general lay-out for figures #####################################
# 
# p <- ggplot() + scale_fill_distiller(palette = "Greens", labels = percent, breaks = pretty_breaks(n = 10), direction=1) 
# p <- p + guides(fill = guide_legend(reverse = TRUE))
# p <- p + theme_nothing(legend=TRUE)
# p <- p + theme(plot.title = element_text(size = rel(2), colour = "black")) 
# 
# ###################### Make specific maps for different variables
# 
# p_unem <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = unemprate), color = "black", size = 0.25) + 
#     labs(title = "Percentage of unemployment in\n German labor market regions in 2010", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/UnemploymentRate.pdf", width = 9.65, height = 11)
# 
# p_em <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = emprate), color = "black", size = 0.25) + 
#     labs(title = "Percentage of employment in\n German labor market regions in 2010", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/EmploymentRate.pdf", width = 9.65, height = 11)
# 
# p_youth <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = youthshare), color = "black", size = 0.25) + 
#     labs(title = "Youth share in\n German labor market regions in 2010", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/YouthShare.pdf", width = 9.65, height = 11)
# 
# p_pr <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = participationrate), color = "black", size = 0.25) + 
#     labs(title = "Participation rate in\n German labor market regions in 2010", fill = "") + 
#     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
# ggsave(filename = "Figs/ParticipationRate.pdf", width = 9.65, height = 11)

##################### Clean Up #####################

rm(list = ls())
