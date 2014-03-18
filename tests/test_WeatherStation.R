library(testthat)

test_that("Create WeatherStation correctly", {
  ws <- WeatherStation(c(1.2, 2.3), c(2.3, 4.6), c(2, 5))
  expect_that(nrow(getDf(ws)), equals(2))
})

test_that("Create WeatherStation wrongly", {
  expect_that(WeatherStation(c(1.2, 2.3), c(4.6), c(2, 5)), throws_error())
})

test_that("WeatherStation subset", {
  ws <- WeatherStation(c(1.2, 2.3), c(2.3, 4.6), c(2, 5))
  ss <- subset(ws, 3)
  expect_that(nrow(getDf(ss)), equals(1))
})

