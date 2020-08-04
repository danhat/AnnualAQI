





get_aqi_data <- function() {
  # get data from annual aqi files, and merge into one data frame
  aqi_file = list.files(pattern = "annual_aqi_by_county*", recursive = TRUE)
  temp_data <- lapply(aqi_file, read.csv)
  aqi_data <- do.call(rbind, temp_data)
  aqi_data <- data.table(aqi_data)
  return(aqi_data)
}


get_aqs_data <- function() {
  aqs2 <- read.csv("./data/aqs_sites.csv")
  aqs <- subset(aqs2, !is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0, select = c("Longitude", "Latitude", "Land.Use", "Location.Setting", "Address", "State.Name", "County.Name"))
  aqs <- data.table(aqs)
  return(aqs)
}


update_file <- function() {
  # get data from annual aqi files, and merge into one data frame
  aqi_data <- { }
  year = 1980
  while (year < 2020) {
    url = paste('https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_', year, sep = '')
    url = paste(url, '.zip', sep = '')
    file = paste('data/annual_aqi_by_county_', year, sep = '')
    file = paste(file, '.csv', sep = '')
    temp <- tempfile()
    download.file(url, temp, mode = 'wb')
    unzip(temp, file)
    #d <- read.csv(file, sep = ',', skip = 0, header = T)
    #aqi_data <- rbind(aqi_data, d)
    unlink(temp)

    year = year + 1
  }

  #aqi_data <- data.table(aqi_data)


  # data for showing location on a map
  temp <- tempfile()
  download.file('https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip', temp, mode = 'wb')
  file = 'data/aqs_sites.csv'
  unzip(temp, file)
  #aqs <- read.csv(file, sep = ',', skip = 0, header = T)
  #aqs <- subset(aqs, !is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0, select = c("Longitude", "Latitude", "Land.Use", "Location.Setting", "Address", "State.Name", "County.Name"))
  #aqs <- data.table(aqs)
}




