

update_data <- function() {
  # get data from annual aqi files, and merge into one data frame
  aqi_data <- data.table()
  
  year_max <- as.numeric(format(Sys.Date(), "%Y"))
  year <- 2000
  
  while (year <= year_max) {
    url <- paste('https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_', year, '.zip', sep = '')
    file <- paste('annual_aqi_by_county_', year, '.csv', sep = '')
    temp <- tempfile()
    download.file(url, temp, mode = 'wb')
    csv_file <- unz(temp, file)
    d <- read.csv(csv_file, sep = ',', skip = 0, header = T)
    aqi_data <- rbind(aqi_data, data.table(d))
    unlink(temp)
    d <- NULL
    year <- year + 1
  }

  # data for showing location on a map
  temp <- tempfile()
  download.file('https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip', temp, mode = 'wb')
  file = 'aqs_sites.csv'
  csv_file <- unz(temp, file)

  aqs <- read.csv(csv_file, sep = ',', skip = 0, header = T)
  aqs <- subset(aqs, !is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0, select = c('Longitude', 'Latitude', 'Land.Use', 'Location.Setting', 'Address', 'State.Name', 'County.Name'))
  aqs <- data.table(aqs)
  unlink(temp)
}




