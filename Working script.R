#To clear the global environment
rm(list=ls())
# Enabling the relevant libraries
x = c("tidyverse",'shinyjs','leaflet','magrittr','reshape2','readxl','dplyr',"maptools","classInt","OpenStreetMap","tmap","RColorBrewer", "sp", "rgeos", "tmaptools","ggmap", "shinyjs","sf", "downloader", "rgdal", "geojsonio")
lapply(x, require, character.only = TRUE)

# Data management
data_raw <- read_xls('C:/Users/Krist/Desktop/Kristian/Privat/Ansøgninger/Udveksling/University College London/Being there/Geographic Information Systems/First Assignment/income-of-tax-payers.xls')
holder = names(data_raw)
data = data_raw[,1:2]

for (i in seq(3,length(names(data_raw)),3)){
  data = cbind(data, data_raw[,i+2])
}

data = data[2:34,]
for (i in seq(3,length(names(data)))){
  names(data)[i] = 1996+i
}
names(data)[1] = "Code"
names(data)[2] = "Area"

growth_rates= data[,1:2]

for (i in seq(4,length(names(data)),1)){
  growth_rates = cbind(growth_rates, (as.numeric(data[,i])/as.numeric(data[,i-1])))
  names(growth_rates)[i-1] = 1996+i
}

growth_rates = cbind(growth_rates, ((as.numeric(data[,18])/as.numeric(data[,3]))^(1/16)))
names(growth_rates)[18] = "Avg._Growth_Rate_in_Earnings"
print(growth_rates[1:5,1:6])

# Plotting

BoroughMapSF <- read_shape("C:/Users/Krist/Desktop/Kristian/Privat/Ansøgninger/Udveksling/University College London/Being there/Geographic Information Systems/GIS/Wk2/Boundary data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp", as.sf = TRUE)

BoroughDataMap <- append_data(BoroughMapSF,growth_rates, key.shp ="GSS_CODE", key.data = "Code", ignore.duplicates = TRUE)

BoroughDataMap["Area"] <- NULL

london_osm <- read_osm(BoroughDataMap, type = "esri", zoom = NULL)
qtm(london_osm) + 
  tm_shape(BoroughDataMap) + 
  tm_polygons("Avg._Growth_Rate_in_Earnings", 
              style="jenks",
              palette="Greens",
              midpoint=NA,
              title=c("Growth Multiple"),
              alpha = 0.5,
              legend.position = c("right", "bottom")) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(legend.position = c("right","bottom"),legend.frame = T) +
  tm_layout(panel.show = TRUE, panel.labels = "Annualised Growth Multiple, Median Earnings, from 1999-2014", panel.label.size = 1.4)
