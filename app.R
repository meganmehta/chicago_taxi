library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(lubridate)
library(plyr)
library(stringr)


#import all data files and combine into one data frame + additional data clean up
# taxi_files = list.files(pattern="taxi")
# allData1 <- lapply(taxi_files, read.delim)
# taxi_data <- do.call(rbind, allData1)
# taxi_data1 <- subset(taxi_data[c(-1,-4, -5)])

taxi_data1 <- read.csv("taxiaa.csv")
taxi_data1 <- subset(taxi_data1[c(-1,-4,-5)])
#taxi_data1 <- data.frame(do.call("rbind", strsplit(as.character(taxi_data1$X.X.Trip.Start.Timestamp.Trip.Seconds.Trip.Miles.Pickup.Community.Area.Dropoff.Community.Area.Company), ",", fixed = TRUE)))

#add additional columns for data sorting (ie: by startHour, day of week, month)
names(taxi_data1) <- c("dropArea", "pickupArea", "timestamp", "tripSeconds", "tripMiles","company","pickupCommunity", "dropCommunity")
#names(taxi_data1) <- c("timestamp", "tripSeconds", "tripMiles", "pickupArea", "dropArea", "company")
taxi_data1$temp <- mdy_hms(taxi_data1$timestamp) 
taxi_data1$date <- format(as.Date(taxi_data1$temp), format="%m-%d")
taxi_data1$startHour <- format((taxi_data1$temp), format = "%H")
taxi_data1$dayOfWeek <- wday(taxi_data1$temp, label=TRUE)
taxi_data1$month <- month(taxi_data1$temp, label=TRUE)
taxi_data1$tripMiles <- as.numeric(taxi_data1$tripMiles)
taxi_data1$pickupArea <- as.numeric(taxi_data1$pickupArea)
taxi_data1$dropArea <- as.numeric(taxi_data1$dropArea)
taxi_data1$km <- 1.609344 * taxi_data1$tripMiles
taxi_data1$t <- strptime(taxi_data1$timestamp, format='%m/%d/%Y %H:%M')
taxi_data1$hour1 <- strftime(taxi_data1$t, '%H')
taxi_data1$timeending <- strftime(taxi_data1$t, format = "%p")
taxi_data1$timeending[taxi_data1$startHour > 12] <- "PM"
taxi_data1$timeending[taxi_data1$startHour < 12] <- "AM"
taxi_data1$amPm <- paste(taxi_data1$hour1,taxi_data1$timeending, sep=" ")

# data binning - resource: https://www.jdatalab.com/data_science_and_data_mining/2017/01/30/data-binning-plot.html
# set up cut-off values for miles 
breaks <- c(0, 10, 20, 40, 60, 80, 100)
tags <- c("under 10 miles","between 10-20 miles","between 20-40 miles", "between 40-60 miles", 
          "between 60-80 miles", "more than 80 miles")
# bucketing values into bins
taxi_data1$binMiles <- cut(taxi_data1$tripMiles, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
#bin km 
breaks3 <- c(0, 10, 30, 45, 60, 75, 100)
tags3 <- c("under 10 km","between 10-30 km","between 30-45 km", "between 45-60 km", 
          "between 60-75 km", "more than 75km")
# bucketing values into bins
taxi_data1$binkm <- cut(taxi_data1$km, 
                           breaks=breaks3, 
                           include.lowest=TRUE, 
                           right=FALSE, 
                           labels=tags3)

# set up cut-off values for time
breaks2 <- c(0, 600, 1800, 3600, 7200, 10800, 14400)
tags2 <- c("under 10 minutes", "between 10-30 minutes", "between 30-60 minutes",
           "between 1-2 hours", "between 2-3 hours", "more than 4 hours")
# bucketing values into bins
taxi_data1$binSeconds <- cut(taxi_data1$tripSeconds, 
                           breaks=breaks2, 
                           include.lowest=TRUE, 
                           right=FALSE, 
                           labels=tags2)

#read in map data - geojson from Chicago data portal 
chicago_community_boundaries <- rgdal::readOGR("Boundaries.geojson") 

#user input selection options
companies <- sort(unique(taxi_data1$company))
taxi_companies <- c("All",companies)
time_selection <- c("24-hour", "AM/PM")
distance_selection <- c("km", "miles")
map_selection <- c("To Community Area", "From Community Area")
community_names <- c("All", unique(chicago_community_boundaries@data$community))

ui <- fluidPage(
  br(),
  br(),
  br(),
  br(),
  br(),
  titlePanel("CS 424 Spring 2022 Project 3 - Big Yellow Taxi\n\n\n"),
  fluidRow(navlistPanel(widths = c(2,10),
                        tabPanel("Main",
                          column(2, 
                              fluidRow(
                                  box(title = "Control Panel", width = 12, height = 500,
                                      selectInput("TaxiCompany", "Select the taxi company you want to visualize", taxi_companies, selected = "All"),
                                      radioButtons("distance", label="Select distance metric:", choices=distance_selection, selected="miles"),
                                      radioButtons("time", label="Select time metric:", choices=time_selection, selected="AM/PM"),
                                      radioButtons("toFrom", "Select metric:", choices=map_selection, selected="To Community Area"),
                                      selectInput("community", "Select the community you want to visualize", community_names, selected = "All"),
                                  )
                              )
                          ),
                 column(6,
                        tabsetPanel(
                          tabPanel("Percentage of Rides", plotOutput("bar6"), height="500px"),
                          tabPanel("Rides by Day of Year", plotOutput("bar1"), height="500px"),
                          tabPanel("Rides by Hour of Day", plotOutput("bar2"), height="500px"),
                          tabPanel("Rides by Day of Week", plotOutput("bar3"), height="500px"),
                          tabPanel("Rides by Month of Year", plotOutput("bar7"), height="500px"),
                          tabPanel("Rides by Binned Mileage", plotOutput("bar4"), height="500px"),
                          tabPanel("Rides by Binned Trip Time", plotOutput("bar5"), height="500px"),
                        ),
                        tabsetPanel(
                          tabPanel("Rides by Day of Year", DT::dataTableOutput("tab1"), height="500px"),
                          tabPanel("Rides by Hour of Day", DT::dataTableOutput("tab2"), height="500px"),
                          tabPanel("Rides by Day of Week", DT::dataTableOutput("tab3"), height="500px"),
                          tabPanel("Rides by Month of Year", DT::dataTableOutput("tab7"), height="500px"),
                          tabPanel("Rides by Binned Mileage", DT::dataTableOutput("tab4"), height="500px"),
                          tabPanel("Rides by Binned Trip Time", DT::dataTableOutput("tab5"), height="500px"),
                        )
                 ),
                 column(4, 
                        box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                            leafletOutput("map1", height = 1000)
                        ),
                        )
),
tabPanel("About",
         mainPanel(
           h1("About Page"),
           p("The Chicago Taxi Ride 2019 data is from the Chicago Data Portal (https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy). This dashboard was created by Megan Mehta and is the third project for CS 424 at UIC. This project
                was created in April 2022.")
         ),
         ))))
server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  #filter bar charts by community area (based on map selection) + taxi company (based on dropdown input)
  #bar chart + table showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
  output$bar1 <- renderPlot({
    if (input$TaxiCompany != "All"){ #if there is a selection for a taxi company, filter data 
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      #get the frequency/number of times a date is mentioned in the dataset 
      date_sums <- count(selected_company, "date")
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      ggplot(date_sums, aes(x=date, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Date", y = lab) + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
    else{ #if all companies 
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        lab <- paste("# of Rides to ", input$community)
        date_sums <- count(selected_community, "date")
        ggplot(date_sums, aes(x=date, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Date", y = lab) + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        lab <- paste("# of Rides from ", input$community)
        date_sums <- count(selected_community, "date")
        ggplot(date_sums, aes(x=date, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Date", y = lab) + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }
      else{
        date_sums <- count(taxi_data1, "date")
        ggplot(date_sums, aes(x=date, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Date", y = "# of Rides") + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }
    }
    
  })
  output$tab1 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      date_sums <- count(selected_company, "date")
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        date_sums <- count(selected_community, "date")
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        date_sums <- count(selected_community, "date")
      }
      else{
        date_sums <- count(taxi_data1, "date")
      }
    }
    names(date_sums)[names(date_sums) == 'freq'] <- 'Number of Rides' 
    names(date_sums)[names(date_sums) == 'date'] <- 'Date' 
    date_sums
  })
  #bar chart showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm) + switch for am/pm or 24 hour time 
  output$bar2 <- renderPlot({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      if (input$time == "AM/PM"){
        startHour_sums <- count(selected_company, "amPm")
        ggplot(startHour_sums, aes(x=amPm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
      }
      else{ #display 24 hour time
        startHour_sums <- count(selected_company, "startHour")
        ggplot(startHour_sums, aes(x=startHour, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
      }
    }
    else{ #for all taxi companies 
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        lab <- paste("# of Rides to ", input$community)
        if (input$time == "AM/PM"){
          startHour_sums <- count(selected_community, "amPm")
          ggplot(startHour_sums, aes(x=amPm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
        }
        else{ #display 24 hour time
          startHour_sums <- count(selected_community, "startHour")
          ggplot(startHour_sums, aes(x=startHour, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
        }
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        lab <- paste("# of Rides from ", input$community)
        date_sums <- count(selected_community, "date")
        if (input$time == "AM/PM"){
          startHour_sums <- count(taxi_data1, "amPm")
          ggplot(startHour_sums, aes(x=amPm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
        }
        else{ #display 24 hour time
          startHour_sums <- count(taxi_data1, "startHour")
          ggplot(startHour_sums, aes(x=startHour, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = lab) + scale_y_continuous(label=comma)
        }
      }
      else{
        if (input$time == "AM/PM"){
          startHour_sums <- count(taxi_data1, "amPm")
          ggplot(startHour_sums, aes(x=amPm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = "# of Rides") + scale_y_continuous(label=comma)
        }
        else{ #display 24 hour time
          startHour_sums <- count(taxi_data1, "startHour")
          ggplot(startHour_sums, aes(x=startHour, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Hour", y = "# of Rides") + scale_y_continuous(label=comma)
        }
      }

    }
    
  })
  output$tab2 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      if (input$time == "AM/PM"){
        startHour_sums <- count(selected_company, "amPm")
      }
      else{ #display 24 hour time
        startHour_sums <- count(selected_company, "startHour")
      }
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        if (input$time == "AM/PM"){
          startHour_sums <- count(selected_community, "amPm")
        }
        else{ #display 24 hour time
          startHour_sums <- count(selected_community, "startHour")
        }
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        if (input$time == "AM/PM"){
          startHour_sums <- count(selected_community, "amPm")
        }
        else{ #display 24 hour time
          startHour_sums <- count(selected_community, "startHour")
        }
      }
      else{
        if (input$time == "AM/PM"){
          startHour_sums <- count(taxi_data1, "amPm")
        }
        else{ #display 24 hour time
          startHour_sums <- count(taxi_data1, "startHour")
        }
      }
    }
    names(startHour_sums)[names(startHour_sums) == 'freq'] <- 'Number of Rides' 
    names(startHour_sums)[names(startHour_sums) == 'startHour'] <- 'Hour' 
    startHour_sums
  })
  #bar chart showing the distribution of the number of rides by day of week (Monday through Sunday)
  output$bar3 <- renderPlot({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      dayofWeek_sums <- count(selected_company, "dayOfWeek")
      ggplot(dayofWeek_sums, aes(x=dayOfWeek, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Day of Week", y = lab) + scale_y_continuous(label=comma)
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        lab <- paste("# of Rides to ", input$community)
        dayofWeek_sums <- count(selected_community, "dayOfWeek")
        ggplot(dayofWeek_sums, aes(x=dayOfWeek, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Day of Week", y = lab) + scale_y_continuous(label=comma)
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        lab <- paste("# of Rides from ", input$community)
        dayofWeek_sums <- count(selected_community, "dayOfWeek")
        ggplot(dayofWeek_sums, aes(x=dayOfWeek, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Day of Week", y = lab) + scale_y_continuous(label=comma)
      }
      else{
        dayofWeek_sums <- count(taxi_data1, "dayOfWeek")
        ggplot(dayofWeek_sums, aes(x=dayOfWeek, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Day of Week", y = "# of Rides") + scale_y_continuous(label=comma)
      }
    }
  })
  output$tab3 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      dayofWeek_sums <- count(selected_company, "dayOfWeek")
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        dayofWeek_sums <- count(selected_community, "dayOfWeek")
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        dayofWeek_sums <- count(selected_community, "dayOfWeek")
      }
      else{
      dayofWeek_sums <- count(taxi_data1, "dayOfWeek")
      }
    }
    names(dayofWeek_sums)[names(dayofWeek_sums) == 'freq'] <- 'Number of Rides' 
    names(dayofWeek_sums)[names(dayofWeek_sums) == 'dayOfWeek'] <- 'Day of Week' 
    dayofWeek_sums
  })

  #bar chart showing the distribution of the number of rides by binned mileage/km (with an appropriate number of bins)
  output$bar4 <- renderPlot({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      if (input$distance == "miles"){
        binnedMiles <- count(selected_company , "binMiles")
        ggplot(binnedMiles, aes(x=binMiles, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in miles)", y = lab) + scale_y_continuous(label=comma)
      }
      else{
        binnedkm <- count(selected_company , "binkm")
        ggplot(binnedkm, aes(x=binkm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in km)", y = lab) + scale_y_continuous(label=comma)
      }
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        lab <- paste("# of Rides to ", input$community)
        if (input$distance == "miles"){
          binnedMiles <- count(selected_community , "binMiles")
          ggplot(binnedMiles, aes(x=binMiles, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in miles)", y = lab) + scale_y_continuous(label=comma)
        }
        else{
          binnedkm <- count(selected_coommunity , "binkm")
          ggplot(binnedkm, aes(x=binkm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in km)", y = lab) + scale_y_continuous(label=comma)
        }
      }
      #show data from community area
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        lab <- paste("# of Rides from ", input$community)
        if (input$distance == "miles"){
          binnedMiles <- count(selected_community , "binMiles")
          ggplot(binnedMiles, aes(x=binMiles, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in miles)", y = lab) + scale_y_continuous(label=comma)
        }
        else{
          binnedkm <- count(selected_community , "binkm")
          ggplot(binnedkm, aes(x=binkm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in km)", y = lab) + scale_y_continuous(label=comma)
        }
      }
      else{ #otherwise include all data 
        if (input$distance == "miles"){
          binnedMiles <- count(taxi_data1 , "binMiles")
          ggplot(binnedMiles, aes(x=binMiles, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in miles)", y = "# of Rides") + scale_y_continuous(label=comma)
        }
        else{
          binnedkm <- count(taxi_data1 , "binkm")
          ggplot(binnedkm, aes(x=binkm, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Distance of Ride (in km)", y = "# of Rides") + scale_y_continuous(label=comma)
        }
      }
    
    }
  })
  output$tab4 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      if (input$distance == "miles"){
        binnedMiles <- count(selected_company , "binMiles")
      }
      else{
        binnedMiles <- count(selected_company , "binkm")
      }
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        if (input$distance == "miles"){
          binnedMiles <- count(selected_community , "binMiles")
        }
        else{
          binnedMiles <- count(selected_community , "binkm")
        }
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        if (input$distance == "miles"){
          binnedMiles <- count(selected_community , "binMiles")
        }
        else{
          binnedMiles <- count(selected_community , "binkm")
        }
      }
      else{
        if (input$distance == "miles"){
          binnedMiles <- count(taxi_data1 , "binMiles")
        }
        else{
          binnedMiles <- count(taxi_data1 , "binkm")
        }
      }
    }
    names(binnedMiles)[names(binnedMiles) == 'freq'] <- 'Number of Rides' 
    names(binnedMiles)[names(binnedMiles) == 'binMiles'] <- 'Mile Bins' 
    names(binnedMiles)[names(binnedMiles) == 'binkm'] <- 'KM Bins' 
    binnedMiles
  })
  #bar chart showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
  output$bar5 <- renderPlot({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      binnedTime <- count(selected_company, "binSeconds")
      ggplot(binnedTime, aes(x=binSeconds, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Time Length of Ride", y = lab) + scale_y_continuous(label=comma)
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        lab <- paste("# of Rides to ", input$community)
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        binnedTime <- count(selected_community, "binSeconds")
        ggplot(binnedTime, aes(x=binSeconds, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Time Length of Ride", y = lab) + scale_y_continuous(label=comma)
        
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        lab <- paste("# of Rides from ", input$community)
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        binnedTime <- count(selected_community, "binSeconds")
        ggplot(binnedTime, aes(x=binSeconds, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Time Length of Ride", y = lab) + scale_y_continuous(label=comma)
      }
      else{
        binnedTime <- count(taxi_data1, "binSeconds")
        ggplot(binnedTime, aes(x=binSeconds, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Time Length of Ride", y = "# of Rides") + scale_y_continuous(label=comma)
      }
      }
  })
  output$tab5 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      binnedTime <- count(selected_company, "binSeconds")
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        binnedTime <- count(selected_community, "binSeconds")
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        binnedTime <- count(selected_community, "binSeconds")
      }
      else{
        binnedTime <- count(taxi_data1, "binSeconds")
      }
    }
    names(binnedTime)[names(binnedTime) == 'freq'] <- 'Number of Rides' 
    names(binnedTime)[names(binnedTime) == 'binSeconds'] <- 'Timing Bins' 
    binnedTime
  })
  #percentage of rides going to / from each of the community areas as bar chart with the community areas listed alphabetically
  output$bar6 <- renderPlot({
    if (input$toFrom == "To Community Area"){
      binnedendArea <- count(taxi_data1, "dropArea")
      totalEnd <- sum(binnedendArea$freq)
      binnedendArea$percenta <- (binnedendArea$freq / totalEnd) * 100 
      chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedendArea, by.x="area_num_1", by.y="dropArea")
      ggplot(chicago_community_boundaries@data, aes(x=sort(community), y=percenta)) + geom_bar(stat="identity", fill="orange") + labs(x="Community Area", y = "% of Rides to Area") + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
    else{
      binnedstartArea <- count(taxi_data1, "pickupArea")
      totalStart <- sum(binnedstartArea$freq)
      binnedstartArea$percentb <- (binnedstartArea$freq / totalStart) * 100 
      chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedstartArea, by.x="area_num_1", by.y="pickupArea")
      ggplot(chicago_community_boundaries@data, aes(x=sort(community), y=percentb)) + geom_bar(stat="identity", fill="orange") + labs(x="Community Area", y = "% of Rides from Area") + scale_y_continuous(label=comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
    
  })
  output$tab7 <- DT::renderDataTable({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      month_sums <- count(selected_company, "month")
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        month_sums <- count(selected_community, "month")
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        month_sums <- count(selected_community, "month")
      }
      else{
        month_sums <- count(taxi_data1, "month")
      }
    }
    names(month_sums)[names(month_sums) == 'freq'] <- 'Number of Rides' 
    names(month_sums)[names(month_sums) == 'month'] <- 'Month' 
    month_sums
  })
  #bar plot by months 
  output$bar7 <- renderPlot({
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      lab <- paste("# of Rides given by ", input$TaxiCompany)
      month_sums <- count(selected_company, "month")
      ggplot(month_sums, aes(x=month, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Month", y = lab) + scale_y_continuous(label=comma)
    }
    else{
      if (input$toFrom == "To Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, dropCommunity == input$community)
        lab <- paste("# of Rides to ", input$community)
        month_sums <- count(selected_community, "month")
        ggplot(month_sums, aes(x=month, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Month", y = lab) + scale_y_continuous(label=comma)
      }
      else if (input$toFrom == "From Community Area" && input$community != "All"){
        selected_community = subset(taxi_data1, pickupCommunity == input$community)
        lab <- paste("# of Rides from ", input$community)
        month_sums <- count(selected_community, "month")
        ggplot(month_sums, aes(x=month, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Month", y = lab) + scale_y_continuous(label=comma)
      }
      else{
        month_sums <- count(taxi_data1, "month")
        ggplot(month_sums, aes(x=month, y=freq)) + geom_bar(stat="identity", fill="orange") + labs(x="Month", y = "# of Rides") + scale_y_continuous(label=comma)
        
      }
    }
  })

  #a map showing the Chicago Community Areas
  #resoure: https://www.theanalyticslab.nl/polygon-plotting-in-r/
  output$map1 <- renderLeaflet({
    #display numbers for every community, either for all taxi companies or filtered by taxi company
    #should display numbers from community area or to community area 
    if (input$TaxiCompany != "All"){
      selected_company <- subset(taxi_data1, company == input$TaxiCompany)
      if (input$toFrom == "To Community Area"){
        legendLabel <- paste("% of rides given by ", input$TaxiCompany, " to community area")
        binnedendArea <- count(selected_company, "dropArea")
        totalEnd <- sum(binnedendArea$freq)
        binnedendArea$percent1 <- (binnedendArea$freq / totalEnd) * 100 
        chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedendArea, by.x="area_num_1", by.y="dropArea")
      }
      else{
        legendLabel <- paste("% of rides given by ", input$TaxiCompany, " from community")
        binnedstartArea <- count(selected_company, "pickupArea")
        totalStart <- sum(binnedstartArea$freq)
        binnedstartArea$percent1 <- (binnedstartArea$freq / totalStart) * 100 
        chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedstartArea, by.x="area_num_1", by.y="pickupArea")
      }
    }
    else{
      if (input$toFrom == "To Community Area"){
        legendLabel <- "% of rides that come to community area"
        binnedendArea <- count(taxi_data1, "dropArea")
        totalEnd <- sum(binnedendArea$freq)
        binnedendArea$percent1 <- (binnedendArea$freq / totalEnd) * 100 
        chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedendArea, by.x="area_num_1", by.y="dropArea")
      }
      else{
        legendLabel <- "% of rides that come from community area"
        binnedstartArea <- count(taxi_data1, "pickupArea")
        totalStart <- sum(binnedstartArea$freq)
        binnedstartArea$percent1 <- (binnedstartArea$freq / totalStart) * 100 
        chicago_community_boundaries@data <- merge(chicago_community_boundaries@data, binnedstartArea, by.x="area_num_1", by.y="pickupArea")
      }
    }
   
    #allow filtering of map by Taxi Company 
    #allow user to select on community + update graphs
    cuts <- c(0, 1, 5, 15, 100)
    colorbins <- colorBin("YlOrRd", domain = chicago_community_boundaries@data$percent1, bins = cuts)
    tooltip <- sprintf("<strong>%s</strong><br/>%.1f%% of rides"
                       ,chicago_community_boundaries@data$community
                       ,chicago_community_boundaries@data$percent1
    ) %>% lapply(htmltools::HTML)
    
    leaflet(chicago_community_boundaries) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(stroke = TRUE, color = "white", weight="1", 
                  smoothFactor = 0.3, fillOpacity = 0.7, 
                  fillColor = ~colorbins(chicago_community_boundaries@data$percent1),
                  label = tooltip) %>%
      addLegend(pal = colorbins, 
                values = chicago_community_boundaries@data$percent1,
                labFormat = labelFormat(suffix = " %"),
                opacity = 0.7, title = legendLabel, position = "topright")
  })
}

shinyApp(ui, server)

