#
# Author: Danielle Hatten
# Title: Just Breathe
# August 2019
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(shinythemes)
library(ggplot2)
library(leaflet)
library(data.table)
#library(htmltools)
library(waffle)
library(ggiraph)

packageVersion("waffle")

library(jpeg)
library(Rcpp) # for rasterGrob()
library(grid)

library(viridis)
library(hrbrthemes)


# get data from annual aqi files, and merge into one data frame
aqi_file = list.files(pattern = "annual_aqi_by_county*", recursive = TRUE)
temp_data <- lapply(aqi_file, read.csv)
aqi_data <- do.call(rbind, temp_data)
aqi_data <- data.table(aqi_data)



#aqi_data$State <- factor(aqi_data$State)

# values for year selectInput
years <- c(2019:1980)

# states available for the user to select
states <- data.table(unique(aqi_data[, 1]))
#levels(droplevels(states))



# data for showing location on a map
aqs2 <- read.csv("./data/aqs_sites.csv")
aqs <- subset(aqs2, !is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0, select = c("Longitude", "Latitude", "Land.Use", "Location.Setting", "Address", "State.Name", "County.Name"))
aqs <- data.table(aqs)


last_selected_state <- reactiveVal(NULL)
last_selected_county <- reactiveVal(NULL)


# silver, purple, orange, blue, green, gray
mycolors <- c("#A9A9A9", "#7220B5", "#FF8930", "#0C9F0F", "#2763DB", "#444444")

# mustard, grey, orange, blue, white, black
mycolors2 <- c("#ffd700", "#bcbcbc", "#ffa500", "#254290", "#ffffff", "#000000")

# green, blue, grey, mustard, orange, red
# good, moderate, unhealty for s, unhealthy, v unhealthy, hazardous
aqi_cat_colors <- c("#0b731c", "#254290", "#bcbcbc", "#ffd700", "#ffa500", "#9a0909")
# R: 11 G: 115 B: 28
# R: 37 G: 66 B: 144

# black, dark green-blue, green-blue (color-blind friendly)
cb_colors <- c("#212945", "#4b5c9b", "#a9b3d6")

# options for pollutant focusing
pollutants <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10")

# options for changing the units
units <- c("Metric", "Imperial")


#######################################################################################################################################################################
######################################################################### dashboard ###################################################################################
ui <- dashboardPage(

  dashboardHeader(title = "Just Breathe"),

  dashboardSidebar(
    disable = FALSE, collapsed = FALSE,
    selectInput("Year", "Select a Year:", years, selected = 2019),
		#selectInput("State", "Select a State: ", states),
		uiOutput("State"),
    uiOutput("County"),
    textOutput("aqi_days"),
    sidebarMenu(
      menuItem("Air Quality Index", tabName = "aqi_tab"),
      menuItem("Pollutants", tabName = "pollutants_tab"),
      menuItem("Comparison", tabName = "comparison_tab"),
		  menuItem("About", tabName = "about")
		)

  ),

  dashboardBody(
		tags$head(
      # modify the theme using css code
			tags$link(rel = "stylesheet", type = "text/css", href = "light_theme.css"),
			tags$link(type ="text/javascript", href = "map.js")
    ),

   
    tabItems(
      # tab #1
      tabItem("aqi_tab",
        fluidRow(
          column(4, offset = 0,
            box(title = "Locations", solidHeader = TRUE, status = "primary", width = NULL, leafletOutput("aqi_map", height = 500))
			      #box(title = "AQI", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("aqi_scatter", height = 500))
          ),
          column(4, offset = 0,
            box(title = "AQI", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("aqi_waffle", height = 500))
          ),
          column(4, offset = 0,
            box(title = "AQI", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("aqi_bubble_plot", height = 500))       
          )
        )

      ),
      # tab #2
      tabItem("pollutants_tab",

        fluidRow(
          column(4, offset = 0,
            #wellPanel(textOutput("aqi_days")),
            wellPanel(
              selectInput("Pollutant", "Select a Pollutant to Focus on", pollutants, selected = pollutants[1]),
              box(title = "Locations", solidHeader = TRUE, status = "primary", width = NULL, leafletOutput("map", height = 500))
            )
		      ),
          column(8,
            wellPanel(
              fluidRow(
              ),
              fluidRow(
                column(6, offset = 0,
                  box(title = "Pollutants", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("pollutants_waffle", height = 500))
                ),
                column(6, offset = 0,
                  box(title = "Days with Pollutant as Main Pollutant 1980-2019", solidHeader = TRUE, status = "primary", width = NULL, girafeOutput("top_pollutant_line", height = 500))
                )
              )
            ) # end well panel
          ) # end column
        ) # end outer row


      ), # end tabItem

      tabItem("comparison_tab",
        fluidRow(
          column(8,
            wellPanel(
              fluidRow(
                column(6, offset = 0,
                  selectInput("state1", "Select a 1st Location for Comparison", states, selected = states[1]),
                  selectInput("state2", "Select a 2nd Location for Comparison", states, selected = states[2])
                ),
                column(6, offset = 0,
                  uiOutput("county1"),
                  uiOutput("county2")
                )
              ),
              fluidRow(
                column(6, offset = 0,
                  box(title = "AQI", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("aqi_stacked", height = 400))
                ),
                column(6, offset = 0,
                  box(title = "Pollutants", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("pollutants_line", height = 400))
                )
              )
            ) # end well panel
          ) # end column 8
        ) # end row
      ),  # end tab #3

      # tab #4
      tabItem("about"

      ) # end tabItem

    ) # end tabItems

  ) # end dashboardBody

) # end dashboardPage


################################################################################################################################################################################
################################################################################ outputs #######################################################################################

server <- function(input, output) {
	theme_set(theme_grey(base_size = 18))


	get_counties_with_aqi_days <- reactive({
		unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$State, select = c("County", "Days.with.AQI")))
  })


  observeEvent(input$State, {
    last_selected_state(input$State)
  })

  
  # update state list based on which states have availiable data for selected year
  output$State <- renderUI({
    all_states <- data.table(unique(aqi_data[, 1]), stringsAsFactors = FALSE)
    all_states[] <- lapply(all_states, as.character)
    states_w_data <- unique(subset(aqi_data, aqi_data$Year == input$Year, select = State))

    pickerInput(
      inputId = "State",
      label = "Select :",
      choices = all_states$State,
      choicesOpt = list(
        disabled = !rownames(all_states) %in% rownames(states_w_data)
      ),
			selected = last_selected_state()
    )
  })


  observeEvent(input$County, {
    last_selected_county(input$County)
  })

	# change county list based on selected state
	output$County <- renderUI({
    counties <- unique(subset(aqi_data[, 2], aqi_data$State == input$State))
    counties[] <- lapply(counties, as.character)
    
    counties_w_data <- unique(subset(aqi_data, aqi_data$State == input$State & aqi_data$Year == input$Year, select = County))


    #if (rownames(counties_w_data) %in% rownames(last_selected_county)) {
      pickerInput(
        inputId = "County",
        label = "Select: ",
        choices = counties$County,
        choicesOpt = list(
          disabled = !rownames(counties) %in% rownames(counties_w_data)
        ),
				selected = last_selected_county()
      )
    #}
    
    
  })


	# change county list based on first location selected for comparison
	output$county1 <- renderUI({
		counties1 <- unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$state1))
		selectInput("county1", " ", counties1, selected = counties1[1])
	})

	# change county list based on second location selected for comparison
	output$county2 <- renderUI({
		counties2 <- unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$state2))
		selectInput("county2", " ", counties2, selected = counties2[1])
	})




	# show number of days where aqi data was collected for selected location
	output$aqi_days <- renderText({
		x <- subset(aqi_data[, "Days.with.AQI"], aqi_data$Year == input$Year & aqi_data$State == input$State & aqi_data$County == input$County)
		t <- paste("Number of Days with AQI Data Collected: ", x)
		t <- paste(x, "/365 Days with Collected Data", sep = "")
		t
	})


	locations_for_state <- reactive({
		d <- aqs[!duplicated(aqs[, c("State.Name", "County.Name")]),]
		d <- subset(d, d$State.Name == input$State)
	})


	numberOfCounties <- reactive({
		length(subset(aqi_data[[2]], aqi_data$Year == input$Year & aqi_data$State == input$State))
	})

	# a map showing the locations of the top 100 counties that have an issue with AQI 
	# or that pollutant based on the annual data, in terms of the largest percentage of bad days for that pollutant 
	aqi_map_data <- reactive({
		n <- numberOfCounties()
		percentages <- subset(aqi_data, aqi_data$Year == input$Year & aqi_data$State == input$State, select = State:Hazardous.Days)
		percentages[, 5:10] <- percentages[, 5:10] / c(percentages[, 4], percentages[, 4], percentages[, 4], percentages[, 4], percentages[, 4], percentages[, 4]) * 100

		# order them by which has the most aqi days in decreasing order
		percentages$Bad <- percentages$Hazardous.Days + percentages$Unhealthy.Days + percentages$Very.Unhealthy.Days + percentages$Unhealthy.for.Sensitive.Groups.Days
		d <- percentages[order(percentages$Bad, decreasing = TRUE),]
		d[1:n,]
	})

	output$aqi_map <- renderLeaflet({
		selected_state <- zoom_state()
		view_lng <- selected_state[1, 1]
		view_lat <- selected_state[1, 2]

		#green, lighter green, yellow, orange, red, darker red

		heat_colors <- c("#00ff00", "#b3ffb3", "#ffff00", "#ff8000", "#ff0000", "#b30000")

		map <- leaflet() %>%
			addTiles() %>%
			addWMSTiles(
				"http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
				layers = "nexrad-n0r-900913",
				options = WMSTileOptions(format = "image/png", transparent = TRUE),
				attribution = "Weather data 2012 IEM Nexrad"
			) %>%
		#setView(lng = as.numeric(view_lng), lat = as.numeric(view_lat), zoom = 5) %>%
		addLegend(
				title = "<center>% of Days w/<br>Unhealthy Levels</center>",
				position = "topright",
				colors = heat_colors,
				labels = c("0-10", "11-20", "21-40", "41-65", "66-85", "86-100"),
				opacity = 1
			)


		# m %>% clearBounds()  # world view

		d <- aqi_map_data()
		df <- data.frame(
			State = d[, "State"],
			County = d[, "County"],
			Days = d[, "Bad"]
		)

		n <- numberOfCounties()

		i = 1
		while (i <= as.numeric(n)) {
			vals <- subset(aqs, as.character(aqs$State.Name) == as.character(df[i, "State"]) & as.character(aqs$County.Name) == as.character(df[i, "County"]), select = Latitude:Longitude)

			if (df[i, 3] <= 10) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[1], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else if (df[i, 3] <= 20) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[2], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else if (df[i, 3] <= 40) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[3], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else if (df[i, 3] <= 65) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[4], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else if (df[i, 3] <= 85) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[5], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else if (df[i, 3] <= 100) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[6], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
			}
			else {

			}

			i = i + 1
		}

		map
	})

	# aqi state data (for year? all years?)
	aqi_state_data <- reactive({
		subset(aqi_data, aqi_data$State == input$State, select = c("Year",
																															 "County",
																															 "Good.Days",
																															 "Moderate.Days",
																															 "Unhealthy.for.Sensitive.Groups.Days",
																															 "Unhealthy.Days",
																															 "Very.Unhealthy.Days",
																															 "Hazardous.Days"))
	})


	output$aqi_scatter <- renderPlot({
		d <- aqi_state_data()

		ggplot(d) +
      geom_point(aes(x = Year, y = Good.Days, color = 'Good'), size = 3) +
      geom_point(aes(x = Year, y = Moderate.Days, color = 'Moderate'), size = 3) +
      geom_point(aes(x = Year, y = Unhealthy.for.Sensitive.Groups.Days, color = 'Unhealthy for\nSensitive Groups'), size = 3) +
      geom_point(aes(x = Year, y = Unhealthy.Days, color = 'Unhealthy'), size = 3) +
      geom_point(aes(x = Year, y = Very.Unhealthy.Days, color = 'Very Unhealthy'), size = 3) +
      geom_point(aes(x = Year, y = Hazardous.Days, color = 'Hazardous'), size = 3) +
      scale_color_manual(values = aqi_cat_colors[]) +
      labs(x = "Year", y = "Number of Days", colour = NULL) +
      theme(legend.position = "bottom", legend.direction = "horizontal") #+
      #guides(colour = guide_legend(override.aes = list(size = 5)))

	})


	pollutant_map_data <- reactive({
		n <- numberOfCounties()
		pollutant <- paste("Days.", input$Pollutant, sep = "")
		percentages <- subset(aqi_data, aqi_data$Year == input$Year & aqi_data$State == input$State)
		percentages$Days <- percentages[[pollutant]]
		d <- percentages[order(percentages$Days, decreasing = TRUE),]
		d[1:n, c(1, 2, 20)]
	})




	output$map <- renderLeaflet({

		selected_state <- zoom_state()
		view_lng <- selected_state[1, 1]
		view_lat <- selected_state[1, 2]

		#green, lighter green, yellow, orange, red, darker red

    heat_colors <- c("#00ff00", "#b3ffb3", "#ffff00", "#ff8000", "#ff0000", "#b30000")
    # r    g    b
    # 0    255  0     green
    # 179  255  179
    # 255  255  0
    # 255  128  0
    # 255  0    0
		# 179  0    0


		map <- leaflet() %>%
			addTiles() %>%
			addWMSTiles(
				"http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
				layers = "nexrad-n0r-900913",
				options = WMSTileOptions(format = "image/png", transparent = TRUE),
				attribution = "Weather data 2012 IEM Nexrad"
			) %>%
		#setView(lng = as.numeric(view_lng), lat = as.numeric(view_lat), zoom = 5) %>%
		addLegend(
				title = "<center>% of Days w/<br>Unhealthy Levels</center>",
				position = "topright",
				colors = heat_colors,
				labels = c("0-10", "11-20", "21-40", "41-65", "66-85", "86-100"),
				opacity = 1
			)


		# m %>% clearBounds()  # world view

		if (input$Pollutant == "AQI") {



		}
		else {

			d <- pollutant_map_data()
			df <- data.frame(
				State = d[, "State"],
				County = d[, "County"],
				Days = d[, "Days"]
			)

			n <- numberOfCounties()

			i = 1
			while (i <= as.numeric(n)) {
				vals <- subset(aqs, as.character(aqs$State.Name) == as.character(df[i, "State"]) & as.character(aqs$County.Name) == as.character(df[i, "County"]), select = Latitude:Longitude)
				pollutant = input$Pollutant

				if (df[i, 3] <= 10) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[1], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else if (df[i, 3] <= 20) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[2], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else if (df[i, 3] <= 40) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[3], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else if (df[i, 3] <= 65) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[4], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else if (df[i, 3] <= 85) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[5], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else if (df[i, 3] <= 100) {
					map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[6], label = paste(paste(df$County[i], "County", sep = " "), df$State[i], sep = ", "))
				}
				else {

				}

				i = i + 1
			}

			map

		}

	})

	zoom_state <- reactive({
		subset(aqs, aqs$State.Name == input$State)[1,]
	})


	observe({
		selected_state <- zoom_state()
		view_lng <- selected_state[1, 1]
		view_lat <- selected_state[1, 2]

		leafletProxy('map') %>%
			setView(lng = as.numeric(view_lng), lat = as.numeric(view_lat), zoom = 5)

	})

	# zoom in on county on marker click
	observeEvent(input$map_click, {
		click <- input$map_click
		view_lng <- click$lng
		view_lat <- click$lat

		leafletProxy('map') %>%
			setView(lng = view_lng, lat = view_lat, zoom = 12)


		#location <- subset(aqs, round(aqs$Longitude, 3) == round(view_lng, 3) & round(aqs$Latitude, 3) == round(view_lat, 3), select = State.Name:County.Name)[1, ]
		#updateSelectInput("State", selected = as.character(location[[1]]))

	})

	user_selected_pollutant <- reactive({
		input$Pollutant
	})
	
	
	user_selected_pollutant_data <- reactive({
	  #pol <- user_selected_pol()
    pol <- paste("Days.", input$Pollutant, sep = "")
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c("Year", pol))
	})


	user_selected_aqi_data <- reactive({
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select(c("Year",
	                                                                                                     "Good.Days",
	                                                                                                     "Moderate.Days",
	                                                                                                     "Unhealthy.for.Sensitive.Groups.Days",
	                                                                                                     "Unhealthy.Days",
	                                                                                                     "Very.Unhealthy.Days",
	                                                                                                     "Hazardous.Days")))
	})
	
	
	# selected pollutant over the years
	output$top_pollutant_line <- renderGirafe({
    if (input$Pollutant == "AQI") {
	    # bar plot
	    d <- user_selected_aqi_data()
	    
	    dt <- data.table(
	      d[, "Year"],
	      d[, "Good.Days"],
	      d[, "Moderate.Days"],
	      d[, "Unhealthy.for.Sensitive.Groups.Days"],
	      d[, "Unhealthy.Days"],
	      d[, "Very.Unhealthy.Days"],
	      d[, "Hazardous.Days"]
	    )
	  }
	  else {
	  d <- user_selected_pollutant_data()
	  
	  dt <- data.table(
	    d[, "Year"],
	    d[, 2]
	  )
	  
	  #dt[, eval(selected_pol)]
	  
	  #selected_pol <- user_selected_pol()
    selected_pol <- paste("Days.", input$Pollutant, sep = "")
	  line_color <- set_line_color(selected_pol)
	  
	  gg <- ggplot(dt, aes(Year)) +
	    geom_line(color = line_color, size = 1, aes_string(y = selected_pol)) +
	    geom_point(aes_string(y = selected_pol)) +
	    labs(x = "Year", y = "Number of Days")
	  #scale_color_manual(values = mycolors[]) +
	  #theme(legend.position = "bottom", legend.direction = "horizontal")
	  
	  gg1 <- gg + geom_point_interactive(aes_string(y = selected_pol, tooltip = dt[[selected_pol]]), size = 2)
	  girafe(ggobj = gg1)
	  }
	  
	})
	
	
	
	
	# selected data for a selected selected county, state, and year
	# w/o aqi.days
	selected_area_pollutants_data <- reactive({
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c("Days.CO", "Days.NO2", "Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10"))
	})
	
	
	output$pollutants_waffle <- renderPlot({
	  d <- data.frame(selected_area_pollutants_data())
	  names(d) <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10")
	  
	  waffle(d[1, ], 
	         rows = 14,
	         xlab = "\n1 square = 1 day",
	         colors = mycolors[],
	         size = 0.33, 
	         legend_pos = "bottom")
  })


  selected_area_aqi_data <- reactive({
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c("Good.Days", "Moderate.Days", "Unhealthy.for.Sensitive.Groups.Days", "Unhealthy.Days", "Very.Unhealthy.Days", "Hazardous.Days"))
	})
	
	
	output$aqi_waffle <- renderPlot({
    #d <- data.frame(selected_area_aqi_data())
    #if (is.null(d)) {
			paste("no data")
    #}

    #else {
     # names(d) <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")

      #waffle(d[1,],
       #    rows = 14,
        #   xlab = "\n1 square = 1 day",
         #  colors = aqi_cat_colors[],
          # size = 0.33,
           #legend_pos = "bottom")

    #}
  })

	
	selected_aqi_for_year <- reactive({
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c("Year", "Days.with.AQI", "Good.Days", "Moderate.Days", "Unhealthy.for.Sensitive.Groups.Days", "Unhealthy.Days", "Very.Unhealthy.Days", "Hazardous.Days"))
  })
	
	
	output$aqi_line <- renderPlot({
	  d <- selected_aqi_for_year()
    d$Bad <- (d$Unhealthy.for.Sensitive.Groups.Days + d$Unhealthy.Days + d$Very.Unhealthy.Days + d$Hazardous.Days) / d$Days.with.AQI * 100
	  
	  lines <- data.table(
	    Year = d$Year,
	    Bad = d$Bad
    )

    img <- jpeg::readJPEG("background.jpg")
    sectioned_background <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
    
    ggplot(lines, aes(Year)) +
      annotation_custom(sectioned_background, - Inf, Inf, - Inf, Inf) +
      geom_line(size = 1, aes(y = Bad), color = "white") +
      geom_point(aes(y = Bad), size = 3, color = "white") +
      labs(x = "Year", y = "% of Days with Hazardous\nor Unhealthy Levels", colour = "Pollutant") +
      scale_y_continuous(labels = c(0:10) * 10, breaks = c(0:10) * 10, limits = c(0, 100), expand = c(0,0))
  })


  output$aqi_bubble_plot <- renderPlot({
    d <- selected_aqi_for_year()
    d$Bad <- (d$Unhealthy.for.Sensitive.Groups.Days +
              d$Unhealthy.Days +
              d$Very.Unhealthy.Days +
              d$Hazardous.Days) /
							d$Days.with.AQI * 100

    lines <- data.table(
      Year = d$Year,
      Bad = d$Bad,
			TotalDays = d$Days.with.AQI
    )


    ggplot(lines, aes(x = Year, y = Bad)) +
      geom_point(aes(color = Bad, size = TotalDays), alpha = 0.5) +
      scale_color_gradient(low = "#0000db", high = "#e60000", name = "% of Days w/\nUnhealthy Levels", limits = c(0, 100)) +
      scale_size(range = c(2, 15), name = "Days w/\nCollected Data") +
      theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "vertical", legend.justification = "center", legend.title.align = 0.5) +
      #coord_equal(expand = TRUE) + 
      scale_y_continuous(labels = c(0:10) * 10, breaks = c(0:10) * 10, limits = c(0, 100))

  })


	all_pollutant_data <- reactive({
		subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c("Year", "Days.with.AQI", "Days.CO", "Days.NO2", "Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10"))
	})

	#read in line graph for main pollutant over the year
	output$pollutant_line <- renderPlot({
		data <- all_pollutant_data()
		lines <- data.table(
			data[, "Year"],
			data[, "Days.CO"],
			data[, "Days.NO2"],
			data[, "Days.Ozone"],
			data[, "Days.SO2"],
			data[, "Days.PM2.5"],
			data[, "Days.PM10"]
		)

		ggplot(lines, aes(Year)) +
		geom_line(size = 1, aes(y = Days.CO, color = "CO")) +
		geom_line(size = 1, aes(y = Days.NO2, color = "NO2")) +
		geom_line(size = 1, aes(y = Days.Ozone, color = "Ozone")) +
		geom_line(size = 1, aes(y = Days.SO2, color = "SO2")) +
		geom_line(size = 1, aes(y = Days.PM25, color = "PM25")) +
		geom_line(size = 1, aes(y = Days.PM10, color = "PM10")) +
		labs(x = "Year", y = "Number of Days", colour = "Pollutant") +
		scale_color_manual(values = mycolors[]) +
		theme(legend.position = "bottom", legend.direction = "horizontal")

	})


	
	



	comparison_data <- reactive({
		d <- subset(aqi_data, aqi_data$State == input$state1 & aqi_data$County == input$county1 & aqi_data$Year == input$Year)
		d2 <- rbind(d, subset(aqi_data, aqi_data$State == input$state2 & aqi_data$County == input$county2 & aqi_data$Year == input$Year))
		d2

	})


	output$aqi_stacked <- renderPlot({
		#d <- comparison_data()

    #data_ <- data.frame(matrix(vector(), 0, 9, dimnames = list(c(), c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous", "Variable", "Fill"))), stringsAsFactors = F)

		# change to percentages
		#dt <- data.table(
		#	Category = c("Good", "Moderate", "Unhealthy for Senstitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
		#	Loc1 = c(d[1, "Good.Days"], d[1, "Moderate.Days"], d[1, "Unhealthy.for.Sensitive.Groups.Days"], d[1, "Unhealthy.Days"], d[1, "Very.Unhealthy.Days"], d[1, "Hazardous.Days"]),
		#	Loc2 = c(d[2, "Good.Days"], d[2, "Moderate.Days"], d[2, "Unhealthy.for.Sensitive.Groups.Days"], d[2, "Unhealthy.Days"], d[2, "Very.Unhealthy.Days"], d[2, "Hazardous.Days"])
    #)
		
		

		#dt.melt <- melt(dt, id.vars = "Category")

		#ggplot(dt.melt, aes(x = Category, y = value, fill = variable)) +
    #geom_bar(stat = "identity") +
    #labs(y = "Value", x = "Category") 

	})


	p_location1_data <- reactive({
		data.table(subset(aqi_data, aqi_data$State == input$state1 & aqi_data$County == input$county1))
	})

	p_location2_data <- reactive({
		data.table(subset(aqi_data, aqi_data$State == input$state2 & aqi_data$County == input$county2))
	})


	output$pollutants_line <- renderPlot({
		d1 <- p_location1_data()
		d2 <- p_location2_data()

		# change to percentages
    dt <- data.table(
			Year = c(1980:2019),
			d1[, "Days.CO"],
			d1[, "Days.NO2"],
			d1[, "Days.Ozone"],
			d1[, "Days.SO2"],
			d1[, "Days.PM2.5"],
			d1[, "Days.PM10"],
			d2[, "Days.CO"],
			d2[, "Days.NO2"],
			d2[, "Days.Ozone"],
			d2[, "Days.SO2"],
			d2[, "Days.PM2.5"],
			d2[, "Days.PM10"]
		)

		names(dt) <- c("Year", "CO_1", "NO2_1", "Ozone_1", "SO2_1", "PM2.5_1", "PM10_1", "CO_2", "NO2_2", "Ozone_2", "SO2_2", "PM2.5_2", "PM10_2")

		ggplot(dt, aes(Year)) +
		geom_line(size = 1, aes(y = CO_1, colour = "CO")) +
		geom_line(size = 1, aes(y = NO2_1, colour = "NO2")) +
		geom_line(size = 1, aes(y = Ozone_1, colour = "Ozone")) +
		geom_line(size = 1, aes(y = SO2_1, colour = "SO2")) +
		geom_line(size = 1, aes(y = PM2.5_1, colour = "PM2.5")) +
		geom_line(size = 1, aes(y = PM10_1, colour = "PM10")) +
		geom_line(size = 1, linetype = "dashed", aes(y = CO_2, colour = "CO")) +
		geom_line(size = 1, linetype = "dashed", aes(y = NO2_2, colour = "NO2")) +
		geom_line(size = 1, linetype = "dashed", aes(y = Ozone_2, colour = "Ozone")) +
		geom_line(size = 1, linetype = "dashed", aes(y = SO2_2, colour = "SO2")) +
		geom_line(size = 1, linetype = "dashed", aes(y = PM2.5_2, colour = "PM2.5")) +
		geom_line(size = 1, linetype = "dashed", aes(y = PM10_2, colour = "PM10")) +
		#scale_color_manual(values = mycolors[]) +
		labs(x = "Year", y = "Value", colour = "Pollutant\n") +
		theme(legend.position = "bottom", legend.direction = "horizontal") #+
		#scale_linetype_manual(values = c("solid", "dashed"), labels = c(input$county1, input$county2))

	})

}


################################################################################################################################################################################
################################################################################ functions #####################################################################################



set_line_color <- function(pollutant) {
  if(pollutant == "Days.CO")
    retval <- mycolors[1]
  else if (pollutant == "Days.NO2")
    retval <- mycolors[2]
  else if (pollutant == "Days.Ozone")
    retval <- mycolors[3]
  else if (pollutant == "Days.SO2")
    retval <- mycolors[4]
  else if (pollutant == "Days.PM2.5")
    retval <- mycolors[5]
  else if (pollutant == "Days.PM10")
    retval <- mycolors[6]

  return(retval)
}



# Run the application
shinyApp(ui = ui, server = server)
