############### Reading necessary libraries #############

library(dplyr)
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
library(flexmix)

############### Reading in the Data #############

EmpData <- read.csv(file = "Data/Employment_lmr.csv", header = TRUE, sep =",")
Instrument2 <- read.csv(file = "Data/Instrument2.csv", header = TRUE, sep =",")

############### Merge the Data ##################

EmpData <- left_join(EmpData, Instrument2, by = c("lmr_id","year"))

############### Recoding data ###################

EmpData <- EmpData %>% 
    mutate(
        Workingpop = pop_20_24 + pop_25_29 + 
            pop_30_34 + pop_35_39 + pop_40_44 + 
            pop_45_49 + pop_50_54 + pop_55_59 + 
            pop_60_64,
        Working = emp_20_24 + emp_25_29 + 
            emp_30_34 + emp_35_39 + emp_40_44 + 
            emp_45_49 + emp_50_54 + emp_55_59 + 
            emp_60_64,
        unemprate = unemp/Workingpop,
        emprate = emp/Workingpop,        
        forshare = emp_for/Workingpop,
        participationrate = (unemp + emp)/Workingpop,
        youthshare = (pop_20_24)/Workingpop,
        popshare = pop/sum(pop),
        logforshare = log(forshare),
        logemprate = log(emprate),
        logunemprate = log(unemprate),
        logyouthshare = log(youthshare)        
    )

############### Saving Data ########################

write.csv(EmpData, file="Data/EmpData.csv", row.names=TRUE)

############### Making temporal descriptive plots ##############

ggplot(subset(EmpData, lmr_name %in% c("Berlin", "Oldenburg", "Kiel", "Frankfurt am Main")),
    aes(x=year, y=youthshare, color=lmr_name))+
    geom_point() + geom_line() + theme_economist() + labs(x = "Year", y = "Youth population share") + theme(legend.title=element_blank()) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ggsave(filename = "Figs/YouthPopulationExamples.pdf", width = 9.65, height = 11)

############### Making descriptive maps #################

RegionData <- filter(EmpData,year==2011) # Just pick one year
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
    labs(title = "Percentage of unemployment in\n German labor market regions in 2011", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/UnemploymentRate.pdf", width = 9.65, height = 11)

p_em <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = emprate), color = "black", size = 0.25) + 
    labs(title = "Percentage of employment in\n German labor market regions in 2011", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/EmploymentRate.pdf", width = 9.65, height = 11)

p_youth <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = youthshare), color = "black", size = 0.25) + 
    labs(title = "Percentage of employment in\n German labor market regions in 2011", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/YouthShare.pdf", width = 9.65, height = 11)

p_pr <- p +  geom_polygon(data=Regions, aes  (x= long, y = lat, group = group, fill = participationrate), color = "black", size = 0.25) + 
    labs(title = "Percentage of employment in\n German labor market regions in 2011", fill = "") + 
    geom_text(data=RegionData, aes(label = substr(lmr_name,1,3), x = Longitude, y = Latitude))
ggsave(filename = "Figs/ParticipationRate.pdf", width = 9.65, height = 11)