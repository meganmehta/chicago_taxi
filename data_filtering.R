#libraries to include
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(lubridate)
library(readxl)
library(rio)
library(data.table)

# initial data trim! 
#allData <- as.data.frame(fread("Taxi_Trips_-_2019.tsv"))
#selected_variables <- c("Trip Start Timestamp", "Trip Seconds", "Trip Miles", "Pickup Community Area", "Dropoff Community Area", "Company")
#selected_taxi_data <- allData[selected_variables]
#write.csv(selected_taxi_data, "trimmed_taxi_data.csv")

#import trimmed data + additional data filtering 
# remove all trips less than 0.5 miles, and more than 100 miles, and less than 60 seconds, and greater than 5 hours
# and all trips that either start or end outside of a Chicago community area. s
'''trimmedData <- read.csv("trimmed_taxi_data.csv")
temp <- subset(trimmedData, Trip.Miles > 0.5) #keep trips more than 0.5 miles 
temp <- subset(temp, Trip.Miles < 100) #keep trips less than 100 miles 
temp <- subset(temp, Trip.Seconds > 60) #keep trips more than 60 seconds 
temp <- subset(temp, Trip.Seconds < 18000) #keep trips less than 5 hours 

temp <- na.omit(temp) #community areas can be blank or NA if its not Chicago
temp <- subset(temp, Pickup.Community.Area < 77) #filter out communities 
temp <- subset(temp, Dropoff.Community.Area < 77)
write.csv(temp, "trimmed_taxi_data_2.csv") '''

#you should make the cab company names more readable (e.g. remove the s for those that have it) 
#and very likely you should convert the taxi company names into some kind of short code 
#in the data file since that text can take up a lot of space
trimmedData <- read.csv("trimmed_taxi_data_2.csv")
chicago_community_boundaries <- rgdal::readOGR("Boundaries.geojson") 

community_name <- c(chicago_community_boundaries@data$community)
community_number <- c(chicago_community_boundaries@data$area_num_1)
community_data <- data.frame(community_name, community_number) #data frame with community names + numbers 

#add name of community 
trimmedData <- merge(trimmedData, community_data, by.x="Pickup.Community.Area", by.y="community_number")
names(trimmedData)[names(trimmedData) == 'community_name'] <- 'PickupCommunity' 
trimmedData <- merge(trimmedData, community_data, by.x="Dropoff.Community.Area", by.y="community_number")
names(trimmedData)[names(trimmedData) == 'community_name'] <- 'DropoffCommunity' 

print(unique(trimmedData$Company))

#rename taxi company names with a lot of numbers 
trimmedData["Company"][trimmedData["Company"] == "1085 - 72312 N and W Cab Co"] <- "N and W Cab Co"
trimmedData["Company"][trimmedData["Company"] == "3094 - 24059 G.L.B. Cab Co"] <- "G.L.B. Cab Co"
trimmedData["Company"][trimmedData["Company"] == "6743 - 78771 Luhak Corp"] <- "Luhak Corp"
trimmedData["Company"][trimmedData["Company"] == "5062 - 34841 Sam Mestas"] <- "Sam Mestas"
trimmedData["Company"][trimmedData["Company"] == "3011 - 66308 JBL Cab Inc."] <- "JBL Cab Inc."
trimmedData["Company"][trimmedData["Company"] == "5874 - 73628 Sergey Cab Corp."] <- "Sergey Cab Corp."
trimmedData["Company"][trimmedData["Company"] == "2092 - 61288 Sbeih company"] <- "Sbeih Company"
trimmedData["Company"][trimmedData["Company"] == "6742 - 83735 Tasha ride inc"] <- "Tasha Ride Inc."
trimmedData["Company"][trimmedData["Company"] == "3620 - 52292 David K. Cab Corp."] <- "David K. Cab Corp."
trimmedData["Company"][trimmedData["Company"] == "4053 - 40193 Adwar H. Nikola"] <- "Adwar H. Nikola"
trimmedData["Company"][trimmedData["Company"] == "2733 - 74600 Benny Jona"] <- "Benny Jona"
trimmedData["Company"][trimmedData["Company"] == "5006 - 39261 Salifu Bawa"] <- "Salifu Bawa"
trimmedData["Company"][trimmedData["Company"] == "4623 - 27290 Jay Kim"] <- "Jay Kim"
trimmedData["Company"][trimmedData["Company"] == "1469 - 64126 Omar Jada"] <- "Omar Jada"
trimmedData["Company"][trimmedData["Company"] == "5074 - 54002 Ahzmi Inc"] <- "Ahzmi Inc."
trimmedData["Company"][trimmedData["Company"] == "3556 - 36214 RC Andrews Cab"] <- "RC Andrews Cab"
trimmedData["Company"][trimmedData["Company"] == "3623 - 72222 Arrington Enterprises"] <- "Arrington Enterprises"
trimmedData["Company"][trimmedData["Company"] == "3721 - Santamaria Express, Alvaro Santamaria"] <- "Alvaro Santamaria"
trimmedData["Company"][trimmedData["Company"] == "6574 - Babylon Express Inc."] <- "Babylon Enterprises"


write.csv(trimmedData, "trimmed_taxi_data_3.csv")























