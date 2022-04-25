# CS 424 Project 3 - Big Yellow Taxi

This code repository contains an app.R file along with the data_filtering.R file and a folder containing the data files used in the R 
visualization. The data_filtering.R file contains the code needed to create the files referenced in the data files folder. The 
app.R file contains all of the code needed to run this project. All the data from the .csv files comes from the [Taxi Trips - 2019](https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy) dataset. 
Another .geojson file was also referenced in the app.R and data_filtering.R file which can be found here: [Boundaries - Community Areas (current)](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6) dataset. 
This data is available from the [Chicago Data Portal](https://data.cityofchicago.org/). 

To run this project locally, make sure to clone this repo on your computer in whatever location you choose. Here are some simple steps below: 

```
cd 
git clone https://github.com/meganmehta/chicago-cta.git
```
Along with cloning this repo, make sure to download [R](https://www.r-project.org/ ) (v 4.1.2) and [R Studio](https://www.rstudio.com/products/rstudio/download/ )(v 2021.09).

Once you have R and R Studio downloaded, open this project file in R Studio. You can do this by setting the working directory: Session -> Set Working Directory -> Choose Directory
![Session -> Set Working Directory -> Choose Directory](https://github.com/meganmehta/cta_rides/blob/main/documentation1.jpg)

After the working directory is set, open the data_filtering.R file within R Studio. **This file needs to be run first to ensure that all of the data files 
are present so that the app.R file can run properly.**

Before running this application, make sure that all necessary packages/libraries are downloaded. To check what libraries are currently added, type the command
`installed.packages()` in the R Studio Console. If some packages are missing, use `install.packages("package-name-here")` to install the remaining ones necessary. 
All of the needed libraries are listed at the top of the app.R file. The ggplot and leaflet packages are the most used ones in this project.

After that, you should be good to run the project. Click the 'Run App' button on the right. 
![](https://github.com/meganmehta/cta_rides/blob/main/documentation2.jpg). 

An additional window will pop up with the visualization and you're all set. 

[Here](http://shiny.evl.uic.edu:3838/g18/big_yellow_taxi/) is the link to my deployed version! If you'd like to read more about the project details, check out
the documentation [here](https://mmehta25.people.uic.edu/project3.html).
