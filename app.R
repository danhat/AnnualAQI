#
# Author: Danielle Hatten
# Title: Just Breathe
# March 2020
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(data.table)
library(waffle)
library(ggiraph)
library(data.table)

packageVersion('waffle')

library(jpeg)
library(Rcpp) # for rasterGrob()
library(grid)

library(viridis)
library(hrbrthemes)

source('getting_data.R')


# values for year selectInput
years <- c(2020:2000)

# states available for the user to select
states <- data.table(unique(aqi_data[, 1]))
#levels(droplevels(states))


last_selected_state <- reactiveVal(NULL)
last_selected_county <- reactiveVal(NULL)


## silver, purple, orange, blue, green, gray
mycolors <- c('#A9A9A9', '#7220B5', '#FF8930', '#0C9F0F', '#2763DB', '#444444')

# mustard, grey, orange, blue, white, black
##mycolors2 <- c('#ffd700', '#bcbcbc', '#ffa500', '#254290', '#ffffff', '#000000')

# green, blue, grey, mustard, orange, red
# good, moderate, unhealty for s, unhealthy, v unhealthy, hazardous
aqi_cat_colors <- c('#0b731c', '#254290', '#bcbcbc', '#ffd700', '#ffa500', '#9a0909')
# R: 11 G: 115 B: 28
# R: 37 G: 66 B: 144

# black, dark green-blue, green-blue (color-blind friendly)
##cb_colors <- c('#212945', '#4b5c9b', '#a9b3d6')


pollutants <- c('CO', 'NO2', 'Ozone', 'SO2', 'PM2.5', 'PM10')
category <- c('AQI', 'Pollutants')


# options for changing the units
units <- c('Metric', 'Imperial')


#######################################################################################################################################################################
######################################################################### dashboard ###################################################################################
ui <- dashboardPage(

  dashboardHeader(title = 'Just Breathe'),

  dashboardSidebar(
    disable = FALSE, collapsed = FALSE,
    selectInput('Year', 'Select a Year:', years, selected = 2019),
		uiOutput('State'),
    uiOutput('County'),
    textOutput('aqi_days'),
    sidebarMenu(
      menuItem('Air Quality Index', tabName = 'aqi_tab'),
      menuItem('Predictions', tabName = 'predictions_tab'),
		  menuItem('About', tabName = 'about_tab')
		)

  ),

  dashboardBody(
		tags$head(
      # modify the theme using css code
			tags$link(rel = 'stylesheet', type = 'text/css', href = 'light_theme.css'),
			tags$link(type ='text/javascript', href = 'map.js')
    ),

   
    tabItems(
      # tab #1
      tabItem('aqi_tab',
        fluidRow(
          column(4, offset = 0,
            box(title = 'Locations', solidHeader = TRUE, status = 'primary', width = NULL, leafletOutput('aqi_map', height = 500))
          ),
          column(4, offset = 0,
            wellPanel(
              selectInput('Category', 'AQI or Pollutants?', category, selected = category[1]),
              box(title = 'AQI', solidHeader = TRUE, status = 'primary', width = NULL, plotOutput('aqi_waffle', height = 500))
            )
          ),
          column(4, offset = 0,
            wellPanel(
              fluidRow(
                selectInput('Pollutant', 'Select a Pollutant to Focus on', pollutants, selected = pollutants[1])
              ),
              fluidRow(
                column(12, offset = 0,
                  box(title = 'Days with Pollutant as Main Pollutant 1980-2019', solidHeader = TRUE, status = 'primary', width = NULL, girafeOutput('top_pollutant_line', height = 500))
                )
              )
            ) # end well panel      
          )
        ),
        
        fluidRow(
          column(8,
            
          ) # end column
        ) # end outer row
        
        

      ),
      # tab #2
      tabItem('predictions_tab',

        


      ), # end tabItem


      # tab #4
      tabItem('about_tab'

      ) # end tabItem

    ) # end tabItems

  ) # end dashboardBody

) # end dashboardPage


################################################################################################################################################################################
################################################################################ outputs #######################################################################################

server <- function(input, output) {
	theme_set(theme_grey(base_size = 18))
  
  
  
  observeEvent(input$State, {
    last_selected_state(input$State)
  })
  
  
  # update state list based on which states have available data for selected year
  output$State <- renderUI({
    all_states <- data.table(unique(aqi_data[, 1]), stringsAsFactors = FALSE)
    all_states[] <- lapply(all_states, as.character)
    states_w_data <- unique(subset(aqi_data, aqi_data$Year == input$Year, select = State))
    
    pickerInput(
      inputId = 'State',
      label = 'Select :',
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
      inputId = 'County',
      label = 'Select: ',
      choices = counties$County,
      choicesOpt = list(
        disabled = !rownames(counties) %in% rownames(counties_w_data)
      ),
      selected = last_selected_county()
    )
    #}
    
    
  })


  
  
	get_counties_with_aqi_days <- reactive({
		unique(subset(aqi_data[, 2], aqi_data$Year == input$Year & aqi_data$State == input$State, select = c('County', 'Days.with.AQI')))
  })





	# show number of days where aqi data was collected for selected location
	output$aqi_days <- renderText({
		x <- subset(aqi_data[, 'Days.with.AQI'], aqi_data$Year == input$Year & aqi_data$State == input$State & aqi_data$County == input$County)
		t <- paste('Number of Days with AQI Data Collected: ', x)
		t <- paste(x, '/365 Days with Collected Data', sep = '')
		t
	})
	
	

	##locations_for_state <- reactive({
	##	d <- aqs[!duplicated(aqs[, c('State.Name', 'County.Name')]),]
	##	d <- subset(d, d$State.Name == input$State)
	##})


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

		heat_colors <- c('#00ff00', '#b3ffb3', '#ffff00', '#ff8000', '#ff0000', '#b30000')

		map <- leaflet() %>%
			addTiles() %>%
			addWMSTiles(
				'http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi',
				layers = 'nexrad-n0r-900913',
				options = WMSTileOptions(format = 'image/png', transparent = TRUE),
				attribution = 'Weather data 2012 IEM Nexrad'
			) %>%
		#setView(lng = as.numeric(view_lng), lat = as.numeric(view_lat), zoom = 5) %>%
		addLegend(
				title = '<center>% of Days w/<br>Unhealthy Levels</center>',
				position = 'topright',
				colors = heat_colors,
				labels = c('0-10', '11-20', '21-40', '41-65', '66-85', '86-100'),
				opacity = 1
			)


		# m %>% clearBounds()  # world view

		d <- aqi_map_data()
		df <- data.frame(
			State = d[, 'State'],
			County = d[, 'County'],
			Days = d[, 'Bad']
		)

		n <- numberOfCounties()

		i = 1
		while (i <= as.numeric(n)) {
			vals <- subset(aqs, as.character(aqs$State.Name) == as.character(df[i, 'State']) & as.character(aqs$County.Name) == as.character(df[i, 'County']), select = Latitude:Longitude)

			if (df[i, 3] <= 10) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[1], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else if (df[i, 3] <= 20) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[2], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else if (df[i, 3] <= 40) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[3], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else if (df[i, 3] <= 65) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[4], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else if (df[i, 3] <= 85) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[5], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else if (df[i, 3] <= 100) {
				map <- addCircleMarkers(map, lng = as.numeric(vals[1, 2]), lat = as.numeric(vals[1, 1]), stroke = FALSE, fillOpacity = 0.7, color = heat_colors[6], label = paste(paste(df$County[i], 'County', sep = ' '), df$State[i], sep = ', '))
			}
			else {

			}

			i = i + 1
		}

		map
	})
	

	
	
	
	
	output$aqi_pie <- renderPlot({
	  
	  
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
		#updateSelectInput('State', selected = as.character(location[[1]]))

	})
	
	

  selected_area_aqi_data <- reactive({
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c('Good.Days', 'Moderate.Days', 'Unhealthy.for.Sensitive.Groups.Days', 'Unhealthy.Days', 'Very.Unhealthy.Days', 'Hazardous.Days'))
	})
	
  # selected data for a selected selected county, state, and year
  # w/o aqi.days
  selected_area_pollutants_data <- reactive({
    subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County) & (aqi_data$Year) == (input$Year), select = c('Days.CO', 'Days.NO2', 'Days.Ozone', 'Days.SO2', 'Days.PM2.5', 'Days.PM10'))
  })
	
	output$aqi_waffle <- renderPlot({
	  
	  if (input$Category == 'AQI') {
	    d <- data.frame(selected_area_aqi_data())
	    if (is.null(d)) {
	      paste('no data')
	    }
	    
	    else {
	      names(d) <- c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Unhealthy', 'Very Unhealthy', 'Hazardous')
	      
	      waffle(d[1,],
          rows = 14,
          xlab = '\n1 square = 1 day',
          colors = aqi_cat_colors[],
          size = 0.33,
          legend_pos = 'bottom')
	      
	    }
	  }
	  
	  else {
	    d <- data.frame(selected_area_pollutants_data())
	    names(d) <- c('CO', 'NO2', 'Ozone', 'SO2', 'PM2.5', 'PM10')
	    
	    waffle(d[1,],
        rows = 14,
        xlab = '\n1 square = 1 day as main pollutant',
        colors = mycolors[],
        size = 0.33,
        legend_pos = 'bottom')
	  }
	  
	  
    
  })





  ################### pollutants ##########################3
	
	##user_selected_pollutant <- reactive({
	  ##input$Pollutant
	##})
	
	
	user_selected_pollutant_data <- reactive({
	  #pol <- user_selected_pol()
	  pol <- paste('Days.', input$Pollutant, sep = '')
	  subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select = c('Year', pol))
	})
	
	

	##user_selected_category <- reactive({
	  ##input$Category
	##})
	
	


  


  
  
  
  user_selected_aqi_data <- reactive({
    subset(aqi_data, (aqi_data$State) == (input$State) & (aqi_data$County) == (input$County), select(c('Year',
                                                                                                       'Good.Days',
                                                                                                       'Moderate.Days',
                                                                                                       'Unhealthy.for.Sensitive.Groups.Days',
                                                                                                       'Unhealthy.Days',
                                                                                                       'Very.Unhealthy.Days',
                                                                                                       'Hazardous.Days')))
  })
  
  
  
  # selected pollutant over the years
  output$top_pollutant_line <- renderGirafe({
    if (input$Pollutant == 'AQI') {
      # bar plot
      d <- user_selected_aqi_data()
      
      dt <- data.table(
        d[, 'Year'],
        d[, 'Good.Days'],
        d[, 'Moderate.Days'],
        d[, 'Unhealthy.for.Sensitive.Groups.Days'],
        d[, 'Unhealthy.Days'],
        d[, 'Very.Unhealthy.Days'],
        d[, 'Hazardous.Days']
      )
    }
    else {
      d <- user_selected_pollutant_data()
      
      dt <- data.table(
        d[, 'Year'],
        d[, 2]
      )
      
      #dt[, eval(selected_pol)]
      
      #selected_pol <- user_selected_pol()
      selected_pol <- paste('Days.', input$Pollutant, sep = '')
      line_color <- set_line_color(selected_pol)
      
      gg <- ggplot(dt, aes(Year)) +
        geom_line(color = line_color, size = 1, aes_string(y = selected_pol)) +
        geom_point(aes_string(y = selected_pol)) +
        labs(x = 'Year', y = 'Number of Days')
      #scale_color_manual(values = mycolors[]) +
      #theme(legend.position = 'bottom', legend.direction = 'horizontal')
      
      gg1 <- gg + geom_point_interactive(aes_string(y = selected_pol, tooltip = dt[[selected_pol]]), size = 2)
      girafe(ggobj = gg1)
    }
    
  })

  



}


################################################################################################################################################################################
################################################################################ functions #####################################################################################



set_line_color <- function(pollutant) {
  if(pollutant == 'Days.CO')
    retval <- mycolors[1]
  else if (pollutant == 'Days.NO2')
    retval <- mycolors[2]
  else if (pollutant == 'Days.Ozone')
    retval <- mycolors[3]
  else if (pollutant == 'Days.SO2')
    retval <- mycolors[4]
  else if (pollutant == 'Days.PM2.5')
    retval <- mycolors[5]
  else if (pollutant == 'Days.PM10')
    retval <- mycolors[6]

  return(retval)
}



# Run the application
shinyApp(ui = ui, server = server)
