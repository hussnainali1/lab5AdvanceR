library(testthat)
library(httr)
library(jsonlite)
library(future)

test_that("Input check doesnot works for fetchWeatherData()", {
  expect_error(fetchWeatherData(12))
})

test_that("Input check doesnot works for fetchCurrentWeather()", {
  expect_error(fetchCurrentWeather("12"))
})


test_that("Output check doesnot works for fetchWeatherData()", {
  result <- fetchWeatherData("sweden")
  expect_true(is.data.frame(result))
  expect_true(is.data.frame(result))
  expect_equal(colnames(result)[1],"time")

})

test_that("Output check doesnot works for fetchCurrentWeather()", {
  currentWeather <- fetchCurrentWeather("sweden")
  expect_true(is.data.frame(currentWeather))
  expect_true(is.data.frame(currentWeather))
  expect_equal(colnames(currentWeather)[1],"Current Temperature In Degree")

})

test_that("Load check doesnot works for fetchWeatherData()", {

  a <- 10
  repeat {
    print(fetchWeatherData("pakistan"))
    a <- a - 1
    if (a < 1) {
      testthat::expect_true(is.data.frame(fetchWeatherData("pakistan")))
      break;
    }
  }
})

test_that("Load check doesnot works for fetchCurrentWeather()", {

  a <- 10
  repeat {
    print(fetchCurrentWeather("pakistan"))
    a <- a - 1
    if (a < 1) {
      testthat::expect_true(is.data.frame(fetchCurrentWeather("pakistan")))
      break;
    }
  }
})


