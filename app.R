#
# Author: Danielle Hatten
# Title: Just Breathe
# August 2019
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(data.table)
library(forcats)


# get data from annual aqi files, and merge into one data frame
aqi_file = list.files(pattern = "annual_aqi_by_county*", recursive = TRUE)
temp_data <- lapply(aqi_file, read.csv)
aqi_data <- do.call(rbind, temp_data)
aqi_data <- data.table(aqi_data)

# values for year selectInput
years <- c(2019:1980)

# states available for the user to select
states <- unique(aqi_data[, 1])

# data for showing location on a map
aqs2 <- read.csv("./data/aqs_sites.csv")
aqs <- subset(aqs2, !is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0, select = c("Longitude", "Latitude", "Land.Use", "Location.Setting", "Address", "State.Name", "County.Name"))
aqs <- data.table(aqs)

# mmonitoring sites
aqs_temp <- read.csv("./data/aqs_monitors.csv")
aqs_monitors <- subset(aqs_temp, grepl(pattern = "Carbon monoxide", aqs_temp$Parameter.Name) | grepl(pattern = "Nitrogen dioxide", aqs_temp$Parameter.Name) | grepl(pattern = "Ozone", aqs_temp$Parameter.Name) | grepl(pattern = "Sulfur dioxide", aqs_temp$Parameter.Name) | grepl(pattern = "PM2.5", aqs_temp$Parameter.Name) | grepl(pattern = "PM10", aqs_temp$Parameter.Name), select = c("Parameter.Name", "Latitude", "Longitude", "Address", "State.Name", "County.Name"))
aqs_monitors <- data.table(aqs_monitors)

# options to change the map background
map_backgrounds <- c("Orignal", "Satellite", "Terrain")

# silver, purple, orange, blue, green, gray
mycolors <- c("#A9A9A9", "#7220B5", "#FF8930", "#0C9F0F", "#2763DB", "#444444")

# mustard, grey, orange, blue, white, black
mycolors2 <- c("#ffd700", "#bcbcbc", "#ffa500", "#254290", "#ffffff", "#000000")

# green, blue, grey, mustard, orange, red
# good, moderate, unhealty for s, unhealthy, v unhealthy, hazardous
aqi_cat_colors <- c("#0b731c", "#254290", "#bcbcbc", "#ffd700", "#ffa500", "#9a0909")

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
    selectInput("Year", "Select a Year:", years, selected = 2018),
    selectInput("State", "Select a State:", states, selected = "Illinois"),
    uiOutput("County"),
    selectInput("map_background", "Choose a Map Background: ", map_backgrounds, selected = map_backgrounds[1]),
		sidebarMenu(
		  menuItem("Annually", tabName = "annually"),
		  menuItem("Hourly", tabName = "hourly"),
		  menuItem("About", tabName = "about")
		)

  ),

  dashboardBody(
		tags$head(
      # modify the theme using css code
			tags$link(rel = "stylesheet", type = "text/css", href = "light_theme.css")
		),

    # tab #1
    tabItems(
      tabItem("annually",

        fluidRow(
          column(4, offset = 0,
            wellPanel(textOutput("aqi_days")),
            box(title = "Top Pollutants Throughout the Year", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("top_pollutant_pie", height = 400))
          ),
          column(8,
            wellPanel(
              fluidRow(
                column(4, offset = 0,
                  selectInput("selected_pollutant", "Select a Pollutant to Focus on:", pollutants, selected = pollutants[1])
                )
              ),
              fluidRow(
                column(6, offset = 0,
                  box(title = "Locations of Monitors", solidHeader = TRUE, status = "primary", width = NULL, leafletOutput("map", height = 400))
                ),
                column(6, offset = 0,
                  box(title = "Days with Pullutant as the Main Pollutant 1980-2019", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("top_pollutant_line", height = 400))
                )
              )
            ) # end well panel
          ) # end column
        ), # end row



        ##### aqi plots #####
        fluidRow(
          column(4, offset = 0,
            box(title = "AQI Level Throughout the Year", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("aqi_pie_plot", height = 400))
          ),
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

      ), # end tabItem

      # tab #2
      tabItem("hourly"

      ), # end tabItem

      # tab #3
      tabItem("about"

      ) # end tabItem

    ) # end tabItems

  ) # end dashboardBody

) # end dashboardPage


################################################################################################################################################################################
################################################################################ outputs #######################################################################################

server <- function(input, output) {
	theme_set(theme_grey(base_size = 18))


	user_selected_pol <- reactive({
		pollutant_to_days_format(input$selected_pollutant)
	})


	get_counties_with_aqi_days <- reactive({
		unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$State, select = c("County", "Days.with.AQI")))
	})

	update_background <- reactive({
		input$map_background
	})

	# change county list based on selected state
	output$County <- renderUI({
		counties <- unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$State))
		selectInput("County", "Select a County", counties, selected = counties[1])
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
		t
	})



	selected_aqi_for_year <- reactive({
		subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c("Days.with.AQI", "Good.Days", "Moderate.Days", "Unhealthy.for.Sensitive.Groups.Days", "Unhealthy.Days", "Very.Unhealthy.Days", "Hazardous.Days"))
	})

	# read in a bar chart for AQI of days for selected year
	output$aqi_bar <- renderPlot({
		Level <- c("Good", "Moderate", "Unhealthy\nfor\nSensitive", "Unhealthy", "Very\nUnhealthy", "Hazardous")
		d <- selected_aqi_for_year()
		Days <- c(d[1, "Good.Days"], d[1, "Moderate.Days"], d[1, "Unhealthy.for.Sensitive.Groups.Days"], d[1, "Unhealthy.Days"], d[1, "Very.Unhealthy.Days"], d[1, "Hazardous.Days"])
		dt <- data.table(Level, Days)

		#ggplot(df, aes(x = fct_inorder(Level), y = Days, fill = Level)) +
		#geom_bar(width = 0.8, stat = "identity", fill = "#2a3457") +
		#labs(x = "Level", y = "Number of Days")

		ggplot(dt, aes(x = fct_inorder(Level), y = Days, fill = Level)) +
		geom_bar(width = 0.8, stat = "identity") +
		scale_fill_manual(values = aqi_cat_colors, name = "AQI Level") +
		#geom_text(show.legend = FALSE) +
		theme(legend.position = "none") +
		labs(x = "Level", y = "Number of Days")

	})

	# read in a pie chart for AQI of days for selected year
	output$aqi_pie_plot <- renderPlot({

		d <- selected_aqi_for_year()
		total_days <- d[1, "Days.with.AQI"]

		dt <- data.table(
			Level <- c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
			Days <- c(d[1, "Good.Days"] / total_days * 100.0, d[1, "Moderate.Days"] / total_days * 100.0, d[1, "Unhealthy.for.Sensitive.Groups.Days"] / total_days * 100.0, d[1, "Unhealthy.Days"] / total_days * 100.0, d[1, "Very.Unhealthy.Days"] / total_days * 100.0, d[1, "Hazardous.Days"] / total_days * 100.0)
		)

		ggplot(dt, aes("", Days, fill = Level)) +
		geom_bar(width = 1, stat = "identity") +
		coord_polar("y", start = 0) +
		geom_text(aes(label = paste0(round(as.numeric(as.character(Days))), "%")), position = position_stack(vjust = 0.5)) +
		labs(x = NULL, y = NULL, fill = NULL, title = NULL, colour = "Category") +
		#guides(fill = guide_legend(reverse = TRUE)) +
		#scale_color_discrete(breaks=c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous")) +
		scale_fill_manual(values = aqi_cat_colors) +
		theme_classic() +
		theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

	})


	# selected data for a selected selected county, state, and year
	selected_area_pollutants_data <- reactive({
		subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c("Days.with.AQI", "Days.CO", "Days.NO2", "Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10"))
	})

	# read in a bar chart for pollutants
	output$aqi_bar_plot <- renderPlot({
		Pollutant <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10")
		selected <- selected_area_pollutants_data()
		Days <- c(selected[1, "Days.CO"], selected[1, "Days.NO2"], selected[1, "Days.Ozone"], selected[1, "Days.SO2"], selected[1, "Days.PM2.5"], selected[1, "Days.PM10"])
		d <- data.table(Pollutant, Days)

		ggplot(d, aes(x = fct_inorder(Pollutant), y = Days)) +
		geom_bar(width = 0.8, stat = "identity", fill = "#2a3457") +
		labs(x = "Pollutant", y = "Number of Days")

	})

	output$top_pollutant_pie <- renderPlot({
		d <- selected_area_pollutants_data()
		total_days <- as.numeric(d[1, "Days.with.AQI"])

		dt <- data.table(
			Pollutant <- c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),
			Days <- c(d[1, "Days.CO"] / total_days * 100.0, d[1, "Days.NO2"] / total_days * 100.0, d[1, "Days.Ozone"] / total_days * 100.0, d[1, "Days.SO2"] / total_days * 100.0, d[1, "Days.PM2.5"] / total_days * 100.0, d[1, "Days.PM10"] / total_days * 100.0)
		)

		ggplot(dt, aes("", Days, fill = Pollutant)) +
		geom_bar(width = 1, stat = "identity") +
		coord_polar("y", start = 0) +
		geom_text(aes(label = paste0(round(as.numeric(as.character(Days))), "%")), position = position_stack(vjust = 0.5)) +
		labs(x = NULL, y = NULL, fill = NULL, title = NULL, colour = "Pollutant") +
		guides(fill = guide_legend(reverse = TRUE)) +
		scale_fill_manual(values = mycolors) +
		theme_classic() +
		theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

	})


	# get data for line graphs
	aqi_percentile_data <- reactive({
		subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c("Year", "Max.AQI", "X90th.Percentile.AQI", "Median.AQI"))
	})

	# read in data for max, 90th percentile, and median AQI over the years
	output$percentile_line <- renderPlot({
		data <- aqi_percentile_data()

		lines <- data.table(
			data[, "Year"], #c(1980:2018),
			data[, "Max.AQI"],
			data[, "X90th.Percentile.AQI"],
			data[, "Median.AQI"]
		)
		# ggtitle for total days
		ggplot(lines, aes(Year)) +
		geom_line(size = 1, linetype = "solid", aes(y = Max.AQI, color = "Max.AQI")) +
		geom_line(size = 1, linetype = "solid", aes(y = X90th.Percentile.AQI, color = "X90th.Percentile.AQI")) +
		geom_line(size = 1, linetype = "solid", aes(y = Median.AQI, color = "Median.AQI")) +
		labs(x = "Year", y = "Number of Days", colour = "Category") +
		scale_color_manual(values = cb_colors[1:3]) +
		theme(legend.position = "bottom", legend.direction = "horizontal")

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


	user_selected_pollutant_data <- reactive({
		pol <- user_selected_pol()
		subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c("Year", pol))
	})

	# selected pollutant over the years
	output$top_pollutant_line <- renderPlot({
		#convert selected pollutatnt to correct form
		d <- user_selected_pollutant_data()

		dt <- data.table(
			d[, "Year"],
			d[, 2]
		)

		selected_pol <- user_selected_pol()
		line_color <- set_line_color(selected_pol)


		ggplot(dt, aes(Year)) +
		geom_line(color = line_color, size = 1, aes_string(y = selected_pol)) +
		geom_point(aes_string(y = selected_pol)) +
		labs(x = "Year", y = "Number of Days")
		#scale_color_manual(values = mycolors[]) +
		#theme(legend.position = "bottom", legend.direction = "horizontal")

	})

	# sites
	monitors_data <- reactive({
		pollutant <- pollutant_to_aqs_form(input$selected_pollutant)
		temp <- subset(aqs_monitors, aqs_monitors$State.Name == input$State & aqs_monitors$County.Name == input$County & grepl(pattern = pollutant, aqs_monitors$Parameter.Name))
		temp
	})

	# get data for showing location on a map
	map_values <- reactive({
		subset(aqs, (aqs$State.Name) == (input$State) & (aqs$County.Name) == (input$County), select = Latitude:Longitude)
	})

	# map to show the location of the selected area
	output$map <- renderLeaflet({
		d <- monitors_data()

		chosen_background <- update_background()

		map <- leaflet()
		map <- addTiles(map)

		if (chosen_background == "Original") {
			map <- remove()
			map <- leaflet()
			map <- addTiles(map) %>%
				addWMSTiles(
					"http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
					layers = "nexrad-n0r-900913",
					options = WMSTileOptions(format = "image/png", transparent = TRUE),
					attribution = "Weather data 2012 IEM Nexrad"
				)
		}
		else if (chosen_background == "Terrain") {
			map <- remove()
			map <- leaflet()
			map <- addTiles(map) %>%
				addProviderTiles(providers$Stamen.Terrain)
		}
		else if (chosen_background == "Satellite") {
			map <- remove()
			map <- leaflet()
			map <- addTiles(map) %>%
				addProviderTiles(providers$Esri.WorldImagery)
		}

		#map <- leaflet()
		#map <- addTiles(map)


		i = 1
		n <- length(d$Latitude)
		while (i <= n) {
			map <- addMarkers(map, lng = as.numeric(d[i, "Longitude"]), lat = as.numeric(d[i, "Latitude"]), popup = d[i, "Address"])
			i = i + 1
		}
		map
	})


	comparison_data <- reactive({
		d <- subset(aqi_data, aqi_data$State == input$state1 & aqi_data$County == input$county1 & aqi_data$Year == input$Year)
		d2 <- rbind(d, subset(aqi_data, aqi_data$State == input$state2 & aqi_data$County == input$county2 & aqi_data$Year == input$Year))
		d2

	})


	output$aqi_stacked <- renderPlot({
		d <- comparison_data()

    data_ <- data.frame(matrix(vector(), 0, 9, dimnames = list(c(), c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous", "Variable", "Fill"))), stringsAsFactors = F)

		# change to percentages
		dt <- data.table(
			Category = c("Good", "Moderate", "Unhealthy for Senstitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
			Loc1 = c(d[1, "Good.Days"], d[1, "Moderate.Days"], d[1, "Unhealthy.for.Sensitive.Groups.Days"], d[1, "Unhealthy.Days"], d[1, "Very.Unhealthy.Days"], d[1, "Hazardous.Days"]),
			Loc2 = c(d[2, "Good.Days"], d[2, "Moderate.Days"], d[2, "Unhealthy.for.Sensitive.Groups.Days"], d[2, "Unhealthy.Days"], d[2, "Very.Unhealthy.Days"], d[2, "Hazardous.Days"])
    )
		
		

		dt.melt <- melt(dt, id.vars = "Category")

		ggplot(dt.melt, aes(x = Category, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(y = "Value", x = "Category") 

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

# function to convert pollutant from input form to aqs form, argument: pollutant input
pollutant_to_aqs_form <- function(input_pol) {
  if(input_pol == "CO")
    retval <- "Carbon monoxide"
  else if (input_pol == "NO2")
    retval <- "Nitrogen dioxide"
  else if (input_pol == "Ozone")
    retval <- "Ozone"
  else if (input_pol == "SO2")
    retval <- "sulfur dioxide"
  else if (input_pol == "PM2.5")
    retval <- "PM2.5"
  else if (input_pol == "PM10")
    retval <- "PM10"

  return(retval)

}

pollutant_to_days_format <- function(input_pol) {
  if(input_pol == "CO")
    retval <- "Days.CO"
  else if (input_pol == "NO2")
    retval <- "Days.NO2"
  else if (input_pol == "Ozone")
    retval <- "Days.Ozone"
  else if (input_pol == "SO2")
    retval <- "Days.SO2"
  else if (input_pol == "PM2.5")
    retval <- "Days.PM2.5"
  else if (input_pol == "PM10")
    retval <- "Days.PM10"

  return(retval)
}


# get pollutant index in table
get_pollutant_index <- function(pollutant, table) {
  if(pollutant == "CO")
    retval <- table[, 3]
  else if (pollutant == "NO2")
    retval <- table[, 4]
  else if (pollutant == "Ozone")
    retval <- table[, 5]
  else if (pollutant == "SO2")
    retval <- table[, 6]
  else if (pollutant == "PM2.5")
    retval <- table[, 7]
  else if (pollutant == "PM10")
    retval <- table[, 8]

  return(retval)
}

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
