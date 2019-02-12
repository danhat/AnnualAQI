#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

# get data from annual aqi files, and merge into one data frame
aqi_file = list.files(pattern = "*county*")
temp_data <- lapply(aqi_file, read.csv)
temp_data2 <- do.call(rbind, temp_data)
aqi_data <- subset(temp_data2, Days.with.AQI > -1)

# values for selectInputs
years <- c(2018:1980)
states2 <- subset(aqi_data[, 1], aqi_data$Year == 2018)
states <- unique(states2)
counties <- subset(aqi_data[, 2], aqi_data$Year == 2018 & aqi_data$State == "Illinois")

# data for showing location on a map
aqs <- read.csv("aqs_sites.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "Annual AQI Data"),

  dashboardSidebar(
    disable = FALSE, collapsed = FALSE,
    selectInput("Year", "Select a year", years, selected = 2018),
    selectInput("State", "Select a state", states, selected = states[15]),
    selectInput("County", "Select a county", counties, selected = counties[4]),

    h3("
      Annual AQI App by Danielle Hatten."),
    h5("Using data from United States Environmental Protection Agency."),
    h5("Libraries used: ggplot2, DT, leaflet")
    
  ),

  dashboardBody(

    # change dashboard colors
    tags$head(
      tags$style(" 

        .skin-blue .main-header .logo {
          background-color: #0D579B;
        }
        
        .skin-blue .main-header .logo:hover {
          background-color: #0D579B;
        }
        
        .skin-blue .main-header .navbar {
          background-color: #0D579B;
        }        
        
        .skin-blue .main-sidebar {
          background-color: #0D518E;
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #ff0000;
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          background-color: #00ff00;
          color: #000000;
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #ff69b4;
        }

        .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #0D579B;
        }
        
        .content-wrapper {
          background-color: white !important;
        }  

        #final_text {
          text-align: center;
          text-font: Verdana;
        }

        
                 
    ")), # end tags$head
    
    
    fluidRow( 
      column(4, offset = 0,
        box(title = "AQI Level Throughout the Year", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("table1", height = 300))
      ),
      column(4,
        box(title = "AQI Level Throughout the Year", solidHeader = TRUE, status = "primary", width = 8, plotOutput("pie1", height = 300))
      ),
      column(4,
        box(title = "AQI Level Throughout the Year", solidHeader = TRUE, status = "primary", width = 8, plotOutput("bar1", height = 300))
      ),
      column(4,
        box(title = "Days with Pollutant as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("line1", height = 300))
      )
    
    ),
    
    fluidRow( 
      column(4,            
        box(title = "Days with Pollutant as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("table2", height = 300))
      ),
      column(4,
        box(title = "Days with Pollutant as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("bar2", height = 300))
      ),
      column(4,
        box(title = "Days with Pullutant as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("line2", height = 300))
      ),
      column(4,
        box(title = "Location", solidHeader = TRUE, status = "primary", width = 8, leafletOutput("leaf", height = 300))
      )
    ),      
        
    fluidRow(
      # pollutants pie charts
      column(4,
        box(title = "CO as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("pieCO", height = 300))
      ),
      column(4,
        box(title = "NO2 as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("pieNO2", height = 300))
      ),
      column(4,
        box(title = "Ozone as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("pieOzone", height = 300))
      ),
      column(4,
        box(title = "SO2 as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("pieSO2", height = 300))
      ),
      column(4,
        box(title = "PM2.5 as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("piePM25", height = 300))
      ),
      column(4,
        box(title = "PM10 as the Main Pollutant", solidHeader = TRUE, status = "primary", width = 8, plotOutput("piePM10", height = 300))
      )      
    )
    
    
  ) # end dashboardBody
) # end dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  # increase font size
  theme_set(theme_grey(base_size = 18))
  
  # reactive function to update counties list
  counties_reactive <- reactive({subset(states[, 2], aqi_data$State == input$State & aqi_data$Year == 2018)})
  
  # selected data for a selected selected county, state, and year
  selected_data <- reactive({subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = Days.with.AQI:Days.PM10)})
  
  # get data for line graphs
  line1_data <- reactive({subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = Max.AQI:Median.AQI)})
  line2_data <- reactive({subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = Days.with.AQI:Days.PM10)})
  
  # get data for showing location on a map
  map_values <- reactive({subset(aqs, (aqs$State.Name) == (input$State) & (aqs$County.Name) == (input$County), select = Latitude:Longitude)})

  # update counties list
  #output$County <- renderUI({
    #selectInput("County2", "Select a county", counties_reactive())
  #})

  # read in a table AQI of days of selected year
  output$table1 <- DT::renderDataTable(
    DT::datatable({
      sd <- subset(selected_data(), select = Good.Days:Hazardous.Days)
      
      data.frame(
        Levels = c("Good", "Moderate", "Unhealthy for Sesitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
        Days = c(sd[1,1], sd[1,2], sd[1,3], sd[1,4], sd[1,5], sd[1,6])
      ) 
    },
    options <- list(searching = FALSE, pageLength = 6, lengthChange = FALSE), 
    rownames = FALSE
    )
  )
  
  # read in table for pollutants 
  output$table2 <- DT::renderDataTable(
    DT::datatable({
      sd <- subset(selected_data(), select = Days.CO:Days.PM10)
      data.frame(
        Pollutant = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),
        Days = c(sd[1,1], sd[1,2], sd[1,3], sd[1,4], sd[1,5], sd[1,6])
      )
    },
    options <- list(searching = FALSE, pageLength = 6, lengthChange = FALSE), 
    rownames = FALSE
    )
  )
  
  # read in a bar chart for AQI of days for selected year
  output$bar1 <- renderPlot({
    Level <- c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous")
    ccd <- selected_data()
    Days <- c(ccd["Good.Days"], ccd["Moderate.Days"], ccd["Unhealthy.for.Sensitive.Groups.Days"], ccd["Unhealthy.Days"], ccd["Very.Unhealthy.Days"], ccd["Hazardous.Days"]) 
    df <- data.frame(Level,Days)
    
    ggplot(df, aes(x = Level, y = Days)) + geom_bar(width = 1, stat = "identity", fill = "#0D579B")
  })
  
  
  # read in a bar chart for pollutants
  output$bar2 <- renderPlot({
    Pollutant <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10")
    ccd <- selected_data()
    Days <- c(ccd["Days.CO"], ccd["Days.NO2"], ccd["Days.Ozone"], ccd["Days.SO2"], ccd["Days.PM2.5"], ccd["Days.PM10"]) 
    df <- data.frame(Pollutant,Days)
    
    ggplot(df, aes(x = Pollutant, y = Days)) + geom_bar(width = 1, stat = "identity", fill = "#0D579B")

  })
  
  # read in a pie chart for AQI of days for selected year
  output$pie1 <- renderPlot({
    Level <- c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous")
    ccd <- selected_data()
    Days <- c(ccd["Good.Days"], ccd["Moderate.Days"], ccd["Unhealthy.for.Sensitive.Groups.Days"], ccd["Unhealthy.Days"], ccd["Very.Unhealthy.Days"], ccd["Hazardous.Days"]) 
    
    df <- data.frame(Level, Days)
    ggplot(df, aes(x = "", y = Days, fill = Level)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  # read in a pie chart for the main pollutants (CO, NO2, Ozone, SO2, PM2.5, PM10)
  output$pieCO <- renderPlot({
    category <- c("CO as Main Pollutant", "CO Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.CO"], sum(sd["Days.NO2"], sd["Days.Ozone"], sd["Days.SO2"], sd["Days.PM2.5"], sd["Days.PM10"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  output$pieNO2 <- renderPlot({
    category <- c("CO as Main Pollutant", "CO Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.NO2"], sum(sd["Days.CO"], sd["Days.Ozone"], sd["Days.SO2"], sd["Days.PM2.5"], sd["Days.PM10"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  output$pieOzone <- renderPlot({
    category <- c("CO as Main Pollutant", "CO Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.Ozone"], sum(sd["Days.NO2"], sd["Days.CO"], sd["Days.SO2"], sd["Days.PM2.5"], sd["Days.PM10"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  output$pieSO2 <- renderPlot({
    category <- c("CO as Main Pollutant", "CO Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.SO2"], sum(sd["Days.NO2"], sd["Days.Ozone"], sd["Days.CO"], sd["Days.PM2.5"], sd["Days.PM10"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  output$piePM25 <- renderPlot({
    category <- c("PM2.5 as Main Pollutant", "PM2.5 Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.PM2.5"], sum(sd["Days.NO2"], sd["Days.Ozone"], sd["Days.SO2"], sd["Days.CO"], sd["Days.PM10"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  output$piePM10 <- renderPlot({
    category <- c("PM10 as Main Pollutant", "PM10 Not Main Pollutant")
    #sd <- selected_data()
    sd <- subset(selected_data(), select = Days.CO:Days.PM10)
    Days <- c(sd["Days.PM10"], sum(sd["Days.NO2"], sd["Days.Ozone"], sd["Days.SO2"], sd["Days.PM2.5"], sd["Days.CO"])) 
    
    df <- data.frame(category,Days)
    ggplot(df, aes(x = "", y = Days, fill = category)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
  })
  
  # read in pie chart for max, 90th percentile, and median AQI over the years
  output$line1 <- renderPlot({
    data <- line1_data()
    lines <- data.frame(
      Year = c(1980:2018),
      Max = data[, "Max.AQI"],
      X90th = data[, "X90th.Percentile.AQI"],
      Median = data[, "Median.AQI"]
    )
    
    ggplot(lines, aes(Year)) + geom_line(aes(y = Max, color = "Max")) + geom_line(aes(y = X90th, color = "X90th")) + geom_line(aes(y = Median, color = "Median")) 
  })
  
  #read in line graph for main pollutant over the year
  output$line2 <- renderPlot({
    data <- line2_data()
    lines <- data.frame(
      Year = c(1980:2018),
      CO = data[, "Days.CO"],
      NO2 = data[, "Days.NO2"],
      Ozone = data[, "Days.Ozone"],
      SO2 = data[, "Days.SO2"],
      PM25 = data[, "Days.PM2.5"],
      PM10 = data[, "Days.PM10"]
    )
    
    ggplot(lines, aes(Year)) + geom_line(aes(y = CO, color = "CO")) + geom_line(aes(y = NO2, color = "NO2")) + geom_line(aes(y = Ozone, color = "Ozone")) + geom_line(aes(y = SO2, color = "SO2")) + geom_line(aes(y = PM25, color = "PM25")) + geom_line(aes(y = PM10, color = "PM10")) 
  })

  # map to show the location of the selected area
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map_data <- map_values()
    map <- setView(map, lng = map_data[1, 2], lat = map_data[1, 1], zoom = 9)
    map <- addMarkers(map, lng = map_data[1, 2], lat = map_data[1, 1], popup = "Here")
    map
  })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

