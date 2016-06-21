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
library(stargazer)
library(foreign)

############### Reading in the Data #############

EmpData <- read.csv(file = "Data/Employment_lmr.csv", header = TRUE, sep =",")

############### Filter Empdata for years, regions and university towns ################

# We work standard with the dataset 2000-2010, because the year 2011
# seems to be a bit of an outlier.
EmpData <- EmpData %>% filter(year <= 2010)

# We further filter the data for robustness analysis.The steps below are used where appropriate in regression Analysis script.  
# EastGermany <- c(8, 9, 13, 93, 94, 104:141)
# UniTowns <- c(44, 125, 74, 8, 44, 86, 90)
# EmpData <- EmpData %>% filter(!lmr_id %in% EastGermany)
# EmpData <- EmpData %>% filter(!lmr_id %in% UniTowns)

############### Recoding data ###################

EmpData <- EmpData %>% 
    mutate(
        Workingpop = #pop_15_17 +  # Be careful here with nominator and denominator
            pop_18_19 + 
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
        Emp2024 = emp_20_24, 
        Emp2534 = emp_25_29+emp_30_34,
        Emp3544 = emp_35_39+emp_40_44,
        Emp4554 = emp_45_49+emp_50_54,
        Emp5564 = emp_55_59+emp_65,
        Emp2529 = emp_25_29,
        logemprate2024 = log((Emp2024)/(pop_20_24)),
        logemprate2534 = log((Emp2534)/(pop_25_29 + pop_30_34)),
        logemprate3544 = log((Emp3544)/(pop_35_39 + pop_40_44)),
        logemprate4554 = log((Emp4554)/(pop_45_49 + pop_50_54)),
        logemprate5564 = log((Emp5564)/(pop_55_59 + pop_60_64)),    
        logemprate2529 = log((Emp2529)/(pop_25_29)),
        unemprate = unemp/Workingpop,
        unemprate_Shimer = unemp/(emp+unemp),
        emprate = emp/Workingpop,
        emprate_Shimer = emp/(emp+unemp),
        foremprate = emp_for/Workingpopfor,
        geremprate = emp_ger/Workingpopger,
        participationrate = (unemp + emp)/Workingpop,
        youthshare = (pop_18_19 + pop_20_24)/Workingpop,
        youthshareger = (pop_ger_20_24)/Workingpop,
        youthsharefor = (pop_for_20_24)/Workingpop,
        youthsharefor2= (pop_for_20_24)/Workingpopfor,
        youthshare_Y1= (pop_20_24)/Workingpop,
        youthshare_Y2= (pop_25_29)/Workingpop,
        popshare = pop/sum(pop),
        logforemprate = log(foremprate),
        loggeremprate = log(geremprate),
        logyouthshareger = log(youthshareger),
        logyouthsharefor = log(youthsharefor), 
        logyouthsharefor2 = log(youthsharefor2), 
        logyouthshare_Y1 = log(youthshare_Y1),
        logyouthshare_Y2 = log(youthshare_Y2),
        logemprate = log(emprate),
        logemprate_Shimer = log(emprate_Shimer),
        logunemprate = log(unemprate),
        logunemprate_Shimer = log(unemprate_Shimer),
        logyouthshare = log(youthshare), 
        logemptask1 = log(emp_task_1/emp),
        logemptask2 = log(emp_task_2/emp),
        logemptask3 = log(emp_task_3/emp),
        logemptask4 = log(emp_task_4/emp),
        logemptask5 = log(emp_task_5/emp),
        logemptask6 = log(emp_task_6/emp),
        logemptask7 = log(emp_task_7/emp),
        logemptask8 = log(emp_task_8/emp),
        logemptask9 = log(emp_task_9/emp),
        logemptask10 =log(emp_task_10/emp),
        logemptask11 =log(emp_task_11/emp),
        logemptask12 =log(emp_task_12/emp),
        logempoccup1 = log((emp_occ1)/(emp)),
        logempoccup2 = log((emp_occ2)/(emp)),
        logempoccup3 = log((emp_occ3)/(emp)),
        logempoccup4 = log((emp_occ4)/(emp)),
        logempoccup5 = log((emp_occ5)/(emp)),
        logempoccup6 = log((emp_occ6)/(emp)),
        loginstrument = log(pop_18_24_ins2),
        logforinstrument = log(pop_for_7y)
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

 RegionData <- filter(EmpData,year==2010) # Just pick one year
 Regions <- readOGR(dsn = "Data/AMR_2012", layer = "AMR_2012") # Read in shape file
 centroids <- as.data.frame(coordinates(Regions)) # Get centroids of regions for labels 
 names(centroids) <- c("Longitude", "Latitude")  # Rename x and y coordinates
 centroids$id <- Regions$lmr2_id  #  Generate id variable
 
 Regions <- fortify(Regions, region="lmr2_id") # Create large data frame from shape file
 Regions$id <- strtoi(Regions$id, base = 0L)   # id should be an integer and not a string
 RegionData$id <- RegionData$lmr_id # Generate ID variable for database
 
 RegionData <- left_join(RegionData, centroids, by="id") # Join database with centroids
 Regions <- left_join(Regions, RegionData, by="id") # Join shape file with database
 
 ###################### Create general lay-out for figures #####################################
 
 p <- ggplot() + scale_fill_distiller(palette = "Greens", labels = percent, breaks = pretty_breaks(n = 10), direction=1) 
 p <- p + guides(fill = guide_legend(reverse = TRUE))
 p <- p + theme_nothing(legend=TRUE)
 p <- p + theme(plot.title = element_text(size = rel(2), colour = "black")) 
 
 ###################### Make specific maps for different variables
 
 p_unem <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = unemprate), color = "black", size = 0.25) + 
     labs(title = "Percentage of unemployment in\n German labor market regions in 2010", fill = "") + 
     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
 ggsave(filename = "Figs/UnemploymentRate.pdf", width = 9.65, height = 11)
 
 p_em <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = emprate), color = "black", size = 0.25) + 
     labs(title = "Percentage of employment in\n German labor market regions in 2010", fill = "") + 
     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
 ggsave(filename = "Figs/EmploymentRate.pdf", width = 9.65, height = 11)
 
 p_youth <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = youthshare), color = "black", size = 0.25) + 
     labs(title = "Youth share in\n German labor market regions in 2010", fill = "") + 
     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
 ggsave(filename = "Figs/YouthShare.pdf", width = 9.65, height = 11)
 
 p_pr <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = participationrate), color = "black", size = 0.25) + 
     labs(title = "Participation rate in\n German labor market regions in 2010", fill = "") + 
     geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
 ggsave(filename = "Figs/ParticipationRate.pdf", width = 9.65, height = 11)

##################### Clean Up #####################

rm(list = ls())
