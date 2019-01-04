library('shiny')
library('shinyjs')
library('sp')
library('rgdal')
library('ggplot2')
library('tmap')
library('sf')
library('geojsonio')
library('plyr')
library('tidyverse')
library('RCurl')
library('RJSONIO')
library('rgeos')
library('maptools')
library('tmaptools')
library('GISTools')
library('spatstat')
library('raster')
library('fpc')
library('reshape2')
library('downloader')
library('plotly')
library('highcharter')
library('leaflet')
library('DT')

variables <- list('Total Population','Male Population', 'Female Population', 'Median Age','Population by Sex, 16+'
                  ,'Population by Sex, 18+','Population by Sex, 21+', 'Population by Sex, 62+', 'Population by Sex, 65+'
                  ,'Population by Mix of Races','Population, One Race','Population, Two or More Races','Races Tallied'
                  ,'Hispanic Population by Origin','Hispanic Population and Race','Non-Hispanic Population and Race', 'Relationships'
                  ,'Relationships, in Households','Households by Type'
                  ,'Tail Households and Averages','Housing Occupancy','Vacancy Rate','Housing Tenure'
                  ,'Population in Occupied Housing Units by Tenure','Average Household Size, Occupied Housing Units, by Tenure')

Census2010_county <- readOGR("County_2010Census_DP1.shp", layer = "County_2010Census_DP1")
USstates <- readOGR("gz_2010_us_040_00_20m.shp", layer = "gz_2010_us_040_00_20m")

USstates <- spTransform(USstates, CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
Census2010_county <- spTransform(Census2010_county, CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
datatable <- Census2010_county@data
datatable[8:64] <- datatable[8:64]/1000
datatable[68:173] <- datatable[68:173]/1000
datatable[176:184] <- datatable[176:184]/1000
datatable[187:191] <- datatable[187:191]/1000

coor <- coordinates(Census2010_county)
centroids <- SpatialPoints(coor, proj4string = CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
centroidsSF <- st_as_sf(centroids)

counties <- st_as_sf(Census2010_county)
USstatesSF <- st_as_sf(USstates)
namesofstates <- USstates$NAME
namesofcounties <- counties$NAMELSAD10

##################################### Setting up the state data ##########################################

stateframe <- data.frame()
for (i in seq_along(1:length(namesofstates))){
  onestate_cur <- st_as_sf(USstates[i,])
  inth_cur <- st_intersects(onestate_cur,centroidsSF)
  counties_onestate_cur <-c(unlist(inth_cur))
  stateframe <- rbind(stateframe,apply(datatable[counties_onestate_cur,8:195],2,sum))
  stateframe[i,58:60] <- apply(datatable[counties_onestate_cur,65:67],2,mean)
  stateframe[i,167:168] <- apply(datatable[counties_onestate_cur,174:175],2,mean)
  stateframe[i,178:179] <- apply(datatable[counties_onestate_cur,185:186],2,mean)
  stateframe[i,185:186] <- apply(datatable[counties_onestate_cur,192:193],2,mean)
  
}

colnames(stateframe)<-colnames(datatable[,8:195])
rownames(stateframe)<-namesofstates

################################################################################

ui <- fluidPage(
  titlePanel('Interactive Visualisation of 2010 Census Data of Demographics of States and Counties in the United States.'),
  sidebarLayout(
    sidebarPanel(
    selectInput("select_2", h3("Choose a variable of interest"), 
                choices = variables),
    checkboxInput('check1','Show County centers', value = FALSE),
    verbatimTextOutput('Description'),
    verbatimTextOutput('numberofcounties'),
    plotOutput("plot_1",width = "100%", height = "400px"),
    plotOutput("plot_2",width = "100%", height = "400px")),
  mainPanel(
    leafletOutput("mymap"),
    DT::dataTableOutput("mytable_1"),
    DT::dataTableOutput("mytable_2")),
  
  position = 'right'
  
  )
  
)

server <- function(input, output) {
  
    display <- reactive({
    if (input$check1){
      if (is.null(input$mymap_center)){
        leaflet(USstates) %>%
          fitBounds(-171.791110603, 18.91619, -66.96466, 71.3577635769) %>%
          addTiles() %>%
          addPolygons(color = 'navy',
                      opacity = 1.0,
                      weight = 1) %>%
          addMarkers(lng = coor[,1],lat = coor[,2])  
      } else {
        leaflet(USstates) %>%
          addTiles() %>%
          addPolygons(color = 'navy',
                      opacity = 1.0,
                      weight = 1) %>%
          addMarkers(lng = coor[,1],lat = coor[,2]) %>%
          setView('mymap', lat = input$mymap_center$lat, lng = input$mymap_center$lng,zoom = input$mymap_zoom)
      }
  
    } else {
      leaflet(USstates) %>%
        fitBounds(-171.791110603, 18.91619, -66.96466, 71.3577635769) %>%
        addTiles() %>%
        addPolygons(color = 'navy',
                    opacity = 1.0,
                    weight = 1)
    }
  })
  
  output$mymap <- renderLeaflet({
    display()
  })
  
  output$Description <- renderText({
    
    variable <- input$select_2
    
    ######################################## Descrption of the variable of choice ################################################
    
    if (variable == 'Total Population'){
      
      'Description:\n\nThe variable "Total Population" shows the distribution of the total population of the state/county chosen.'
    
    } else if (variable == 'Male Population'){
    
      'Description:\n\nThe variable "Male Population" shows the distribution of the total male population of the state/county chosen.'
    
    } else if (variable == 'Female Population'){
      
      'Description:\n\nThe variable "Female Population" shows the distribution of the total female population of the state/county chosen.'
    
    } else if (variable == 'Median Age'){
      
      'Description:\n\nThe variable "Median Age" shows the median age for the total, male and female population of the state/county chosen.'
      
    } else if (variable == 'Population by Sex, 16+'){
      
      'Description:\n\nThe variable "Population by Sex, 16+" shows the population in total and by sex, for persons above 16 years in the state/county chosen.'
      
    } else if (variable == 'Population by Sex, 18+'){
      
      'Description:\n\nThe variable "Population by Sex, 18+" shows the population in total and by sex, for persons above 18 years in the state/county chosen.'
      
    } else if (variable == 'Population by Sex, 21+'){
      
      'Description:\n\nThe variable "Population by Sex, 21+" shows the population in total and by sex, for persons above 21 years in the state/county chosen.'
      
    } else if (variable == 'Population by Sex, 62+'){
      
      'Description:\n\nThe variable "Population by Sex, 62+" shows the population in total and by sex, for persons above 62 years in the state/county chosen.'
      
    } else if (variable == 'Population by Sex, 65+'){
      
      'Description:\n\nThe variable "Population by Sex, 65+" shows the population in total and by sex, for persons above 65 years in the state/county chosen.'
      
    } else if (variable == 'Population by Mix of Races') {
      
      'Description:\n\nThe variable "Population by Mix of Races" shows the amount of people having one or two+ races in the state/county chosen.'
      
    } else if (variable == 'Population, One Race'){
    
      'Description:\n\nThe variable "Population, One Race" shows the distribution of the total population having one race based on their race.\n\nAbbreviations:\n\n AI: American Indian\n AN: Alaska Native\n PI: Pacific Islander'
    
    } else if (variable == 'Population, Two or More Races'){
      
      'Description:\n\nThe variable "Population, Two or More Races" shows the distribution of the total population having two or more races based on their race combination. Abbreviations:\n\n AI: American Indian\n AN: Alaska Native'
    
    } else if (variable == 'Races Tallied'){
      
      'Description:\n\nThe variable "Races Tallied" shows the total population based on the major race groups.\n\nAbbreviations:\n\n AI: American Indian \n AN: Alaska Native \n NH: Native Hawaiian\n PI: Pacific Islander'
    
    } else if (variable == 'Hispanic Population by Origin'){
      
      'Description:\n\nThe variable "Hispanic Population by Origin" shows the origin distribution of the hispanic population in the state/county chosen'
      
    } else if (variable == 'Hispanic Population and Race'){
      
      'Description:\n\nThe variable "Hispanic Population and Race" shows distribution of the hispanic population based on their race.\n\nAbbreviations:\n\n AI: American Indian \n AN: Alaska Native \n NH: Native Hawaiian\n PI: Pacific Islander'
    
    } else if (variable == 'Non-Hispanic Population and Race'){
      
      'Description:\n\nThe variable "Non-hispanic Population and Race" shows distribution of the non-hispanic population based on their race.\n\nAbbreviations:\n\n AI: American Indian \n AN: Alaska Native \n NH: Native Hawaiian\n PI: Pacific Islander'
    
    } else if (variable == 'Relationships'){
      
      'Description:\n\nThe variable "Relationships" shows which relationships the total population lives by.\n\nAbbreviations:\n\n Pop: Population\n IP: Institutionalised Population\n NP: Noninstitutionalised Population'
    
    } else if (variable == 'Relationships, in Households'){
      
      'Description:\n\nThe variable "Relationships, in Households" shows the distribution of relationships within the household relationship category.'
      
    } else if (variable == 'Households by Type'){
      
      'Description:\n\nThe variable "Households by Type" shows how the households are distributed in the state/county chosen.. "Families" and "Nonfamiliy" makes up the total amount of households. For each of the two categories is shown detailed information, which makes up each of the two categories. Missing tick marks is due the fact that one or more tick marks are the same. In this case, it is because "With Own Children, -18" follows multiple times.'
    
    } else if (variable == 'Tail Households and Averages'){
      
      'Description:\n\nThe variable "Tail Households and Averages" shows the amount of households which holds either young people or elderly, along with average household size and average family size of for all households, for the state/county chosen.'
      
    } else if (variable == 'Housing Occupancy'){
      
      'Description:\n\nThe variable "Housing Occupancy" shows how the total housing units is distributed across the avaliable categories in the state/county chosen..\n\nAbbreviations:\n\n VH: Vacant Housing'
    
    } else if (variable == 'Vacancy Rate'){
      
      'Description:\n\nThe variable "Vacancy Rate" shows the availability of homeowner homes and rental homes in the state/county chosen.'
    
    } else if (variable == 'Housing Tenure'){
      
      'Description:\n\nThe variable "Housing Tenure" shows how the housing units within the state/county chosen is owned, either by ownership or renting.'
      
    } else if (variable == 'Population in Occupied Housing Units by Tenure'){
      
      'Description:\n\nThe variable "Population in Occupied Housing Units by Tenure" shows the amount of people living in owned and rented housing units in the state/county chosen.'
      
    } else if (variable == 'Average Household Size, Occupied Housing Units, by Tenure'){
      
      'Description:\n\nThe variable "Average Household Size, Occupiced Housing Units, by Tenure" shows the average household size based on ownership type within the state/county chosen.'
      
    }
    
    })
  
  observeEvent(input$mymap_click, {
    s <- input$mymap_click

    y_coor <- s$lng
    x_coor <- s$lat
    
    combpoint <- data.frame(y_coor,x_coor)
    
    pointclick <- st_as_sf(SpatialPoints(combpoint,proj4string = CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")))
    
    posofstate <- c(unlist(st_intersects(pointclick,USstatesSF)))
    
    onestate_1 <- USstatesSF[posofstate,]
    numbofcounties <- length(c(unlist(st_intersects(onestate_1,centroidsSF))))
    
    output$numberofcounties <- renderText({
      
      paste('The number of counties within the state chosen are: ',numbofcounties)
      
    }) 
    
    nameofstate <- namesofstates[posofstate]
    proxy <- leafletProxy("mymap")
    proxy %>% clearPopups() %>%
      addPopups(s$lng, s$lat, nameofstate)
    
    output$plot_1 <- renderPlot({
      
      variable <- input$select_2
      
      onestate_1 <- st_as_sf(USstates[posofstate,])
      
      inth_1 <- st_intersects(onestate_1,centroidsSF)
      counties_onestate_1 <-c(unlist(inth_1))
      
      
      #################################### Creating the correct plot based on the variable of interest ####################################    
      if (variable == 'Total Population'){
        
        statedata<- stateframe[posofstate,2:19]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Population: ',sum(datatable[counties_onestate_1,8])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Male Population'){
        
        statedata<- stateframe[posofstate,21:38]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Population: ',sum(datatable[counties_onestate_1,27])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Female Population'){
        
        statedata<- stateframe[posofstate,40:57]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Population: ',sum(datatable[counties_onestate_1,46])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Median Age'){
        
        statedata<- stateframe[posofstate,58:60]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Both Sexes','Male','Female')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Average Median Age',
               title=paste0(nameofstate, ": ",'Average Median Age In the State'))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Sex, 16+'){
        
        statedata<- stateframe[posofstate,61:63]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 16+','Male, 16+','Female, 16+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Sex, 18+'){
        
        statedata<- stateframe[posofstate,64:66]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint, row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 18+','Male, 18+','Female, 18+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Sex, 21+'){
        
        statedata<- stateframe[posofstate,67:69]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 21+','Male, 21+','Female, 21+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Sex, 62+'){
        
        statedata<- stateframe[posofstate,70:72]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 62+','Male, 62+','Female, 62+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Sex, 65+'){
        
        statedata<- stateframe[posofstate,73:75]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 65+','Male, 65+','Female, 65+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population by Mix of Races'){
        
        statedata<- data.frame(list(stateframe[posofstate,76:77],stateframe[posofstate,95]))
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total','One Race','Two+ Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population, One Race'){
        
        statedata<- data.frame(list(stateframe[posofstate,78:88],stateframe[posofstate,90:94]))
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','Asian India','Chinese','Filipino','Japanese','Korean','Vietnamese'
                       ,'Other Asian','Native Hawaiian','Guamanian','Samoan','Other PI','Other Race')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Population, Two or More Races'){
        
        statedata<- data.frame(list(stateframe[posofstate,96:99],(stateframe[posofstate,95]-apply(stateframe[posofstate,96:99],1,sum))))
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White; AI and AN','White; Asian','White; African American','White; some Other Combi.','Other Combi.')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Races Tallied'){
        
        statedata<- stateframe[posofstate,100:105]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or other PI','Some Other Race')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Hispanic Population by Origin'){
        
        statedata<- stateframe[posofstate,107:112]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Hispanic (any race)','Mexican','Puerto Rican','Cuban','Other Hispanic','Not Hispanic')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Hispanic Population and Race'){
        
        statedata<- stateframe[posofstate,115:121]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Hispanic Population: ',sum(datatable[counties_onestate_1,121])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Non-Hispanic Population and Race'){
        
        statedata<- stateframe[posofstate,123:129]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Non-Hispanic Population: ',sum(datatable[counties_onestate_1,129])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Relationships'){
        
        statedata<- data.frame(list(stateframe[posofstate,131],stateframe[posofstate,144:149]))
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('In Households','Institutionalised Pop.','IP, Male','IP, Female','Noninstitutionalised Pop.','NP, Male','NP, Female')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Population: ',sum(datatable[counties_onestate_1,137])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Relationships, in Households'){
        
        statedata<- stateframe[posofstate,132:142]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Householder','Spouse','Child','Own Child, -18','Other Relatives','Other relatives, -18','Other Relatives, 65+'
                       ,'Nonrelatives','Nonrelatives, -18','Nonrelatives, 65+','Nonrelatives, UP')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Population in Households: ',sum(datatable[counties_onestate_1,138])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Households by Type'){
        # Consider if the missing labels can be handled better
        statedata<- stateframe[posofstate,151:164]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Families','With Own Children, -18','Husband-wife','With Own Children, -18','Single Male',
                       'With Own Children, -18','Single Female','With Own Children, -18','Nonfamiliy'
                       ,'Singles','Singles, Male','Single Males, 65+','Singles, Female','Single Females, 65+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Households',sum(datatable[counties_onestate_1,157])))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Tail Households and Averages'){
        # Consider if there should be two y-axis because of the scale difference in the values
        statedata<- stateframe[posofstate,165:168]
        varofint_1 <- apply(statedata[,1:2],2,sum)
        varofint_2 <- round(apply(statedata[,3:4],2,mean),2)
        varofint_3 <- merge.list(varofint_1,varofint_2)
        textpos <- seq(1:length(varofint_3))
        varofint_df <- data.frame(x = textpos, y = varofint_3,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Households, Individuals -18','Households, Individuals 65+','Average Household Size', 'Average Family Size')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount / Average Sizes',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 15, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint_3)*1.20)
        
      } else if (variable == 'Housing Occupancy'){
        
        statedata<- stateframe[posofstate,170:177]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Occupied Housing Units','Vacant Housing Units','VH, For rent', 'VH, Rented - Not Occupied','VH, For Sale Only'
                       ,'VH, Sold - Not Occupied', 'VH, For SRorO use','VH, All Other Vacants')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Housing Units',sum(datatable[counties_onestate_1,176])))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      } else if (variable == 'Vacancy Rate'){
        
        statedata<- stateframe[posofstate,178:179]
        varofint <- round(apply(statedata,2,mean),3)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Homeowner','Rental')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Average Percentage',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      }  else if (variable == 'Housing Tenure'){
        
        statedata<- stateframe[posofstate,181:182]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofstate, ": ",variable),
               subtitle=paste('Total Occupied Housing Units',sum(datatable[counties_onestate_1,187])))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.22)
        
      } else if (variable == 'Population in Occupied Housing Units by Tenure'){
        
        statedata<- stateframe[posofstate,183:184]
        varofint <- apply(statedata,2,sum)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.25)
        
      } else if (variable == 'Average Household Size, Occupied Housing Units, by Tenure'){
        
        statedata<- stateframe[posofstate,185:186]
        varofint <- round(apply(statedata,2,mean),3)
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Average Sizes',
               title=paste0(nameofstate, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.20)
        
      }
      
    })
    
  })
  
  observeEvent(input$mymap_marker_click, {
    c <- input$mymap_marker_click

    y_coor <- c$lng
    x_coor <- c$lat

    combpoint <- data.frame(y_coor,x_coor)
    
    pointclick <- st_as_sf(SpatialPoints(combpoint,proj4string = CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")))
    
    posofcounty <- c(unlist(st_intersects(pointclick,counties)))
    posofcountyinstate <- c(unlist(st_intersects(pointclick,USstatesSF)))
    
    nameofcounty <- namesofcounties[posofcounty]
    
    proxy <- leafletProxy("mymap")
    proxy %>% clearPopups() %>%
      addPopups(round(c$lng,2), round(c$lat,2), nameofcounty)
    
    output$plot_2 <- renderPlot({
      
      variable <- input$select_2
      #################################### Creating the correct plot based on the variable of interest ####################################    
      if (variable == 'Total Population'){
        
        varofint <- c(unlist(datatable[posofcounty,9:26]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Population: ',datatable[posofcounty,8]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Male Population'){
        
        varofint <- c(unlist(datatable[posofcounty,28:45]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Population: ',datatable[posofcounty,27]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Female Population'){
        
        varofint <- c(unlist(datatable[posofcounty,47:64]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','pop')
        labelplot <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                       ,'70-74','75-79','80-84','85<')
        
        ggplot(varofint_df,aes(x=pos,y=pop,label = pop))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Population: ',datatable[posofcounty,46]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Median Age'){
        # Not very good
        varofint <- c(unlist(datatable[posofcounty,65:67]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Both Sexes','Male','Female')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Median Age',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 16+'){
        
        varofint <- c(unlist(datatable[posofcounty,68:70]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 16+','Male, 16+','Female, 16+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 18+'){
        
        varofint <- c(unlist(datatable[posofcounty,71:73]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 18+','Male, 18+','Female, 18+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 21+'){
        
        varofint <- c(unlist(datatable[posofcounty,74:76]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 21+','Male, 21+','Female, 21+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 62+'){
        
        varofint <- c(unlist(datatable[posofcounty,77:79]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 62+','Male, 62+','Female, 62+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 65+'){
        
        varofint <- c(unlist(datatable[posofcounty,80:82]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 65+','Male, 65+','Female, 65+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Sex, 65+'){
        
        varofint <- c(unlist(datatable[posofcounty,80:82]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total, 65+','Male, 65+','Female, 65+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population by Mix of Races'){
        
        varofint <- c(unlist(datatable[posofcounty,83:84]),unlist(datatable[posofcounty,102]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Total','One Race','Two+ Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population, One Race'){
        
        varofint <- c(unlist(datatable[posofcounty,85:95]),unlist(datatable[posofcounty,97:101]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','Asian India','Chinese','Filipino','Japanese','Korean','Vietnamese'
                       ,'Other Asian','Native Hawaiian','Guamanian','Samoan','Other Pacific Islander','Other Race')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population, Two or More Races'){
        
        varofint <- c(unlist(datatable[posofcounty,103:106]),(unlist(datatable[posofcounty,102])-sum(unlist(datatable[posofcounty,103:106]))))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White; AI and AN','White; Asian','White; African American','White; some Other Combi.','Other Combi.')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Races Tallied'){
        
        varofint <- c(unlist(datatable[posofcounty,107:112]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or other PI','Some Other Race')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Hispanic Population by Origin'){
        
        varofint <- c(unlist(datatable[posofcounty,114:119]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Hispanic (any race)','Mexican','Puerto Rican','Cuban','Other Hispanic','Not Hispanic')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Hispanic Population and Race'){
        
        varofint <- c(unlist(datatable[posofcounty,122:128]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Hispanic Population: ',datatable[posofcounty,121]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Non-Hispanic Population and Race'){
        
        varofint <- c(unlist(datatable[posofcounty,130:136]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Non-Hispanic Population: ',datatable[posofcounty,129]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Relationships'){
        
        varofint <- c(unlist(datatable[posofcounty,138]),unlist(datatable[posofcounty,151:156]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('In Households','Institutionalized Pop.','Male','Female','Noninstitutionalized Pop.','Male','Female')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Population: ',datatable[posofcounty,137]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Relationships, in Households'){
        
        varofint <- c(unlist(datatable[posofcounty,139:149]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Householder','Spouse','Child','Own Child, -18','Other Relatives','Other relatives, -18','Other Relatives, 65+'
                       ,'Nonrelatives','Nonrelatives, -18','Nonrelatives, 65+','Nonrelatives, Unmarried Partner')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Population in Households: ',datatable[posofcounty,138]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Households by Type'){
        # Consider if the missing labels can be handled better
        varofint <- c(unlist(datatable[posofcounty,158:171]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Families','With Own Children, -18','Husband-wife','With Own Children, -18','Single Male Householder',
                       'With Own Children, -18','Single Female Householder','With Own Children, -18','Nonfamiliy Households'
                       ,'Singles','Singles, Male','Single Males, 65+','Singles, Female','Single Females, 65+')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Households',datatable[posofcounty,157]))+
          theme(axis.text.x=element_text(angle = 50, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Tail Households and Averages'){
        
        varofint <- c(unlist(datatable[posofcounty,172:175]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Households, Individuals -18','Households, Individuals 65+','Average Household Size', 'Average Family Size')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount / Average Sizes',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 15, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Housing Occupancy'){
        
        varofint <- c(unlist(datatable[posofcounty,177:184]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Occupied Housing Units','Vacant Housing Units','VH, For rent', 'VH, Rented - Not Occupied','VH, For Sale Only'
                       ,'VH, Sold - Not Occupied', 'VH, For SRorO use','VH, All Other Vacants')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Housing Units',datatable[posofcounty,176]))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Vacancy Rate'){
        
        varofint <- c(unlist(datatable[posofcounty,185:186]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Homeowner','Rental')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Percentage',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      }  else if (variable == 'Housing Tenure'){
        
        varofint <- c(unlist(datatable[posofcounty,188:189]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Amount',
               title=paste0(nameofcounty, ": ",variable),
               subtitle=paste('Total Occupied Housing Units',datatable[posofcounty,187]))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5)
                ,plot.subtitle = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Population in Occupied Housing Units by Tenure'){
        
        varofint <- c(unlist(datatable[posofcounty,190:191]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = '1000 Persons',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      } else if (variable == 'Average Household Size, Occupied Housing Units, by Tenure'){
        
        varofint <- c(unlist(datatable[posofcounty,192:193]))
        textpos <- seq(1:length(varofint))
        varofint_df <- data.frame(x = textpos, y = varofint,row.names = textpos)
        colnames(varofint_df) <- c('pos','age')
        labelplot <- c('Owner-occupied','Renter-occupied')
        
        ggplot(varofint_df,aes(x=pos,y=age,label = age))+
          geom_bar(stat = 'identity')+
          geom_text(vjust = 0.3,hjust = -0.1,angle = 90)+ 
          scale_x_discrete(name ="Groups", limits=labelplot)+
          labs(y = 'Size',
               title=paste0(nameofcounty, ": ",variable))+
          theme(axis.text.x=element_text(angle = 30, hjust = 1)
                ,plot.title = element_text(hjust=0.5))+
          ylim(0,max(varofint)*1.15)
        
      }
      
    })
    
    output$mytable_2 <- DT::renderDataTable({
      
      variable <- input$select_2
      
      onestate_1 <- st_as_sf(USstates[posofcountyinstate,])
      
      inth_1 <- st_intersects(onestate_1,centroidsSF)
      counties_onestate_1 <-c(unlist(inth_1))
      
      #################################### Creating the correct descriptive statistic table based on the variable of interest ####################################
      #################################### County data ##########################################################    
      if (variable == 'Total Population'){
        
        holder <- data.frame(datatable[counties_onestate_1,9:26])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                                ,'70-74','75-79','80-84','85<')
        desstats <- t(desstats)
        
      } else if (variable == 'Male Population'){
        
        holder <- data.frame(datatable[counties_onestate_1,28:45])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                                ,'70-74','75-79','80-84','85<')
        desstats <- t(desstats)
        
      } else if (variable == 'Female Population'){
        
        holder <- data.frame(datatable[counties_onestate_1,47:64])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                                ,'70-74','75-79','80-84','85<')
        desstats <- t(desstats)
        
      } else if (variable == 'Median Age'){
        
        holder <- data.frame(datatable[counties_onestate_1,65:67])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),1))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Both Sexes','Male','Female')
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Sex, 16+'){
        
        holder <- data.frame(datatable[counties_onestate_1,68:70])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total, 16+','Male, 16+','Female, 16+')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Sex, 18+'){
        
        holder <- data.frame(datatable[counties_onestate_1,71:73])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total, 18+','Male, 18+','Female, 18+')
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Sex, 21+'){
        
        holder <- data.frame(datatable[counties_onestate_1,74:76])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total, 21+','Male, 21+','Female, 21+')
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Sex, 62+'){
        
        holder <- data.frame(datatable[counties_onestate_1,77:79])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total, 62+','Male, 62+','Female, 62+')
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Sex, 65+'){
        
        holder <- data.frame(datatable[counties_onestate_1,80:82])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total, 65+','Male, 65+','Female, 65+')
        desstats <- t(desstats)
        
      } else if (variable == 'Population by Mix of Races'){
        
        holder <- data.frame(list(datatable[counties_onestate_1,83:84],datatable[counties_onestate_1,102]))
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Total','One Race','Two+ Races')
        desstats <- t(desstats)
        
      } else if (variable == 'Population, One Race'){
        
        holder <- data.frame(list(datatable[counties_onestate_1,85:95],datatable[counties_onestate_1,97:101]))
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('White','African American','AI and AN','Asian','Asian India','Chinese','Filipino','Japanese','Korean','Vietnamese'
                                ,'Other Asian','Native Hawaiian','Guamanian','Samoan','Other Pacific Islander','Other Race')
        desstats <- t(desstats)
        
      } else if (variable == 'Population, Two or More Races'){
        
        holder <- data.frame(list(datatable[counties_onestate_1,103:106],(datatable[counties_onestate_1,102]-apply(datatable[counties_onestate_1,103:106],1,sum))))
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('White; AI and AN','White; Asian','White; African American','White; some Other Combi.','Other Combi.')
        desstats <- t(desstats)
        
      } else if (variable == 'Races Tallied'){
        
        holder <- data.frame(datatable[counties_onestate_1,107:112])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or other PI','Some Other Race')
        desstats <- t(desstats)
        
      } else if (variable == 'Hispanic Population by Origin'){
        
        holder <- data.frame(datatable[counties_onestate_1,114:119])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Hispanic (any race)','Mexican','Puerto Rican','Cuban','Other Hispanic','Not Hispanic')
        desstats <- t(desstats)
        
      } else if (variable == 'Hispanic Population and Race'){
        
        holder <- data.frame(datatable[counties_onestate_1,122:128])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        desstats <- t(desstats)
        
      } else if (variable == 'Non-Hispanic Population and Race'){
        
        holder <- data.frame(datatable[counties_onestate_1,130:136])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
        desstats <- t(desstats)
        
      } else if (variable == 'Relationships'){
        
        holder <- data.frame(list(datatable[counties_onestate_1,138],datatable[counties_onestate_1,151:156]))
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('In Households','Institutionalized Pop.','Male','Female','Noninstitutionalized Pop.','Male','Female')
        desstats <- t(desstats)
        
      } else if (variable == 'Relationships, in Households'){
        
        holder <- data.frame(datatable[counties_onestate_1,139:149])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Householder','Spouse','Child','Own Child, -18','Other Relatives','Other relatives, -18','Other Relatives, 65+'
                                ,'Nonrelatives','Nonrelatives, -18','Nonrelatives, 65+','Nonrelatives, Unmarried Partner')
        desstats <- t(desstats)
        
      } else if (variable == 'Households by Type'){
        
        holder <- data.frame(datatable[counties_onestate_1,158:171])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Families','With Own Children, -18','Husband-wife','With Own Children, -18','Single Male Householder',
                                'With Own Children, -18','Single Female Householder','With Own Children, -18','Nonfamiliy Households'
                                ,'Singles','Singles, Male','Single Males, 65+','Singles, Female','Single Females, 65+')
        desstats <- t(desstats)
        
      } else if (variable == 'Tail Households and Averages'){
        
        holder <- data.frame(datatable[counties_onestate_1,172:175])
        
        # The unelegant code block below is necessary because we need to have different decimals on the four variables (or at least
        # for the two pairs)
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        
        desstat_1 <- data.frame()
        desstat_1 <- rbind(desstat_1,round(apply(holder[1:2],2,mean),3))
        desstat_1 <- cbind(desstat_1,round(apply(holder[3],2,mean),3))
        desstat_1 <- cbind(desstat_1,round(apply(holder[4],2,mean),3))
        
        colnames(desstat_1) <- colnames(desstats)
        
        desstats <- rbind(desstats,desstat_1)
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Households, Individuals -18','Households, Individuals 65+','Average Household Size', 'Average Family Size')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Housing Occupancy'){
        
        holder <- data.frame(datatable[counties_onestate_1,177:184])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Occupied Housing Units','Vacant Housing Units','VH, For rent', 'VH, Rented - Not Occupied','VH, For Sale Only'
                                ,'VH, Sold - Not Occupied', 'VH, For SRorO use','VH, All Other Vacants')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Vacancy Rate'){
        
        holder <- data.frame(datatable[counties_onestate_1,185:186])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Homeowner','Rental')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Housing Tenure'){
        
        holder <- data.frame(datatable[counties_onestate_1,188:189])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Owner-occupied','Renter-occupied')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Population in Occupied Housing Units by Tenure'){
        
        holder <- data.frame(datatable[counties_onestate_1,190:191])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Owner-occupied','Renter-occupied')
        
        desstats <- t(desstats)
        
      } else if (variable == 'Average Household Size, Occupied Housing Units, by Tenure'){
        
        holder <- data.frame(datatable[counties_onestate_1,192:193])
        
        desstats <- data.frame()
        desstats <- rbind(desstats,apply(holder,2,median))
        desstats <- rbind(desstats,round(apply(holder,2,mean),3))
        desstats <- rbind(desstats,apply(holder,2,max))
        desstats <- rbind(desstats,apply(holder,2,min))
        rownames(desstats) <- c('Median','Mean','Max','Min')
        colnames(desstats) <- c('Owner-occupied','Renter-occupied')
        
        desstats <- t(desstats)
        
      }
      
      DT::datatable(desstats, options = list(lengthMenu = c(10, 15, 20), pageLength = 20),caption = 'Descriptive Statistics Table For All Counties Within the State of the Chosen County')
    })
    
})

  output$mytable_1 <- DT::renderDataTable({
    
    variable <- input$select_2
    #################################### Creating the correct descriptive statistic table based on the variable of interest ####################################
    #################################### State data #######################################################################
    if (variable == 'Total Population'){
      
      holder <- data.frame(stateframe[,2:19])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                              ,'70-74','75-79','80-84','85<')
      desstats <- t(desstats)
      
    } else if (variable == 'Male Population'){
      
      holder <- data.frame(stateframe[,21:38])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                              ,'70-74','75-79','80-84','85<')
      desstats <- t(desstats)
      
    } else if (variable == 'Female Population'){
      
      holder <- data.frame(stateframe[,40:57])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69'
                              ,'70-74','75-79','80-84','85<')
      desstats <- t(desstats)
      
    } else if (variable == 'Median Age'){
      
      holder <- data.frame(stateframe[,58:60])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),1))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Both Sexes','Male','Female')
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Sex, 16+'){
      
      holder <- data.frame(stateframe[,61:63])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total, 16+','Male, 16+','Female, 16+')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Sex, 18+'){
      
      holder <- data.frame(stateframe[,64:66])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total, 18+','Male, 18+','Female, 18+')
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Sex, 21+'){
      
      holder <- data.frame(stateframe[,67:69])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total, 21+','Male, 21+','Female, 21+')
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Sex, 62+'){
      
      holder <- data.frame(stateframe[,70:72])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total, 62+','Male, 62+','Female, 62+')
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Sex, 65+'){
      
      holder <- data.frame(stateframe[,73:75])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total, 65+','Male, 65+','Female, 65+')
      desstats <- t(desstats)
      
    } else if (variable == 'Population by Mix of Races'){
      
      holder <- data.frame(list(stateframe[,76:77],stateframe[,95]))
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Total','One Race','Two+ Races')
      desstats <- t(desstats)
      
    } else if (variable == 'Population, One Race'){
      
      holder <- data.frame(list(stateframe[,78:88],stateframe[,90:94]))
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('White','African American','AI and AN','Asian','Asian India','Chinese','Filipino','Japanese','Korean','Vietnamese'
                              ,'Other Asian','Native Hawaiian','Guamanian','Samoan','Other Pacific Islander','Other Race')
      desstats <- t(desstats)
      
    } else if (variable == 'Population, Two or More Races'){
      
      holder <- data.frame(list(stateframe[,96:99],(stateframe[,95]-apply(stateframe[,96:99],1,sum))))
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('White; AI and AN','White; Asian','White; African American','White; some Other Combi.','Other Combi.')
      desstats <- t(desstats)
      
    } else if (variable == 'Races Tallied'){
      
      holder <- data.frame(stateframe[,100:105])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or other PI','Some Other Race')
      desstats <- t(desstats)
      
    } else if (variable == 'Hispanic Population by Origin'){
      
      holder <- data.frame(stateframe[,107:112])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Hispanic (any race)','Mexican','Puerto Rican','Cuban','Other Hispanic','Not Hispanic')
      desstats <- t(desstats)
      
    } else if (variable == 'Hispanic Population and Race'){
      
      holder <- data.frame(stateframe[,115:121])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
      desstats <- t(desstats)
      
    } else if (variable == 'Non-Hispanic Population and Race'){
      
      holder <- data.frame(stateframe[,123:129])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('White','African American','AI and AN','Asian','NH or Other PI','Some Other Race','Two or More Races')
      desstats <- t(desstats)
      
    } else if (variable == 'Relationships'){
      
      holder <- data.frame(list(stateframe[,131],stateframe[,144:149]))
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('In Households','Institutionalized Pop.','Male','Female','Noninstitutionalized Pop.','Male','Female')
      desstats <- t(desstats)
      
    } else if (variable == 'Relationships, in Households'){
      
      holder <- data.frame(stateframe[,132:142])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Householder','Spouse','Child','Own Child, -18','Other Relatives','Other relatives, -18','Other Relatives, 65+'
                              ,'Nonrelatives','Nonrelatives, -18','Nonrelatives, 65+','Nonrelatives, Unmarried Partner')
      desstats <- t(desstats)
      
    } else if (variable == 'Households by Type'){
      
      holder <- data.frame(stateframe[,151:164])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Families','With Own Children, -18','Husband-wife','With Own Children, -18','Single Male Householder',
                              'With Own Children, -18','Single Female Householder','With Own Children, -18','Nonfamiliy Households'
                              ,'Singles','Singles, Male','Single Males, 65+','Singles, Female','Single Females, 65+')
      desstats <- t(desstats)
      
    } else if (variable == 'Tail Households and Averages'){
      
      holder <- data.frame(stateframe[,165:168])
      
      # The unelegant code block below is necessary because we need to have different decimals on the four variables (or at least
      # for the two pairs)
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      
      desstat_1 <- data.frame()
      desstat_1 <- rbind(desstat_1,round(apply(holder[1:2],2,mean),3))
      desstat_1 <- cbind(desstat_1,round(apply(holder[3],2,mean),3))
      desstat_1 <- cbind(desstat_1,round(apply(holder[4],2,mean),3))
      
      colnames(desstat_1) <- colnames(desstats)
      
      desstats <- rbind(desstats,desstat_1)
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Households, Individuals -18','Households, Individuals 65+','Average Household Size', 'Average Family Size')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Housing Occupancy'){
      
      holder <- data.frame(stateframe[,170:177])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Occupied Housing Units','Vacant Housing Units','VH, For rent', 'VH, Rented - Not Occupied','VH, For Sale Only'
                              ,'VH, Sold - Not Occupied', 'VH, For SRorO use','VH, All Other Vacants')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Vacancy Rate'){
      
      holder <- data.frame(stateframe[,178:179])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Homeowner','Rental')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Housing Tenure'){
      
      holder <- data.frame(stateframe[,181:182])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Owner-occupied','Renter-occupied')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Population in Occupied Housing Units by Tenure'){
      
      holder <- data.frame(stateframe[,183:184])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Owner-occupied','Renter-occupied')
      
      desstats <- t(desstats)
      
    } else if (variable == 'Average Household Size, Occupied Housing Units, by Tenure'){
      
      holder <- data.frame(stateframe[,185:186])
      
      desstats <- data.frame()
      desstats <- rbind(desstats,apply(holder,2,median))
      desstats <- rbind(desstats,round(apply(holder,2,mean),3))
      desstats <- rbind(desstats,apply(holder,2,max))
      desstats <- rbind(desstats,apply(holder,2,min))
      rownames(desstats) <- c('Median','Mean','Max','Min')
      colnames(desstats) <- c('Owner-occupied','Renter-occupied')
      
      desstats <- t(desstats)
      
    }
    
    DT::datatable(desstats, options = list(lengthMenu = c(10, 15, 20), pageLength = 20),caption = 'Descriptive Statistics Table For All States')
  })
  
}

shinyApp(ui = ui, server = server)
