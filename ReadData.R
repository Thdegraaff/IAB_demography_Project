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

############### Reading in the Data #############

EmpData <- read.csv(file = "Data/Employment_lmr.csv", header = TRUE, sep =",")
Instrument_20_24 <- read.csv(file = "Data/Instrument2.csv", header = TRUE, sep =",")
Instrument_18_24 <- read.csv(file = "Data/Instrument2_18_24.csv", header = TRUE, sep =",")
Instrument_15_24 <- read.csv(file = "Data/Instrument2_15_24.csv", header = TRUE, sep =",")
Instrument_25_29 <- read.csv(file = "Data/Instrument2_25_29.csv", header = TRUE, sep =",")
Instrument_20_29 <- read.csv(file = "Data/Instrument2_20_29.csv", header = TRUE, sep =",")
Population_15_17 <- read.csv(file = "Data/Population_15_17_lmr.csv", header = TRUE, sep =",")
Population_18_19 <- read.csv(file = "Data/Population_18_19_lmr.csv", header = TRUE, sep =",")
Population_18_19 <- read.csv(file = "Data/Population_18_19_lmr.csv", header = TRUE, sep =",")

############### Merge the Data ##################

EmpData <- left_join(EmpData, Population_15_17, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Population_18_19, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Instrument_20_24, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Instrument_18_24, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Instrument_15_24, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Instrument_25_29, by = c("lmr_id","year"))
EmpData <- left_join(EmpData, Instrument_20_29, by = c("lmr_id","year"))

############### Filter Empdata for years and regions ################

# We work standard with the dataset 2000-2010, because the year 2011
# seems to be a bit of an outlier.

EastGermany <- c(8, 9, 13, 93, 94, 104:141)
EmpData <- EmpData %>% filter(year <= 2010)
#EmpData <- EmpData %>% filter(!lmr_id %in% EastGermany)

############### Recoding data ###################

EmpData <- EmpData %>% 
    mutate(
        Workingpop = #pop_15_17 + 
            pop_18_19 + 
            pop_20_24 + pop_25_29 + 
            pop_30_34 + pop_35_39 + pop_40_44 + 
            pop_45_49 + pop_50_54 + pop_55_59 + 
            pop_60_64,
        unemprate = unemp/Workingpop,
        emprate = emp/Workingpop,        
        forshare = emp_for/Workingpop,
        participationrate = (unemp + emp)/Workingpop,
        youthshare = (pop_18_19 + pop_20_24)/Workingpop,
        popshare = pop/sum(pop),
        logforshare = log(forshare),
        logemprate = log(emprate),
        logunemprate = log(unemprate),
        logyouthshare = log(youthshare), 
        loginstrument = log(pop_18_24_ins2)
    )

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
    labs(title = "Percentage of employment in\n German labor market regions in 2010", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/YouthShare.pdf", width = 9.65, height = 11)

p_pr <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = participationrate), color = "black", size = 0.25) + 
    labs(title = "Participation rate in\n German labor market regions in 2010", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/ParticipationRate.pdf", width = 9.65, height = 11)

##################### Clean Up #####################

rm(list = ls())
