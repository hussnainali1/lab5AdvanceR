#' fetchWeatherData
#'
#' @param queryCity A String
#'
#' @return A DataFrame
#' @export fetchWeatherData
#'
#' @examples
#' fetchWeatherData("pakistan")
#'
fetchWeatherData <- function(queryCity) {
  stopifnot(is.character(queryCity))
  baseUrl <- "https://api.weatherapi.com/v1/"
  apiKey <- "148193efcfce480cb9591848222809"
  url <- paste(baseUrl,"forecast.json?key=",apiKey, "&q=",queryCity, sep="")
  res <- future::future(httr::GET(url))
  responseValue <- future::value(res)
  contentData <- httr::content(responseValue, "text")
  dat <- jsonlite::fromJSON(contentData, flatten = TRUE)$forecast$forecastday$hour
  dataframeObj <- as.data.frame(dat)[ ,2:3]
  return(dataframeObj)
}


#' fetchCurrentWeather
#'
#' @param queryCity A String
#'
#' @return A DataFrame
#' @export fetchCurrentWeather
#'
#' @examples
#' fetchCurrentWeather("Pakistan")
#'
fetchCurrentWeather <- function(queryCity) {

  stopifnot(is.character(queryCity))
  baseUrl <- "https://api.weatherapi.com/v1/"
  apiKey <- "148193efcfce480cb9591848222809"
  url <- paste(baseUrl,"current.json?key=",apiKey, "&q=",queryCity, sep="")
  res <- future::future(httr::GET(url))
  responseValue <- future::value(res)
  contentData <- httr::content(responseValue, "text")
  dat <- jsonlite::fromJSON(contentData, flatten = TRUE)$current$temp_c
  dataframeObj <- as.data.frame(dat)
  colnames(dataframeObj) <- c("Current Temperature In Degree")
  return(dataframeObj)
}

