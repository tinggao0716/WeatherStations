#######################################
# Test cases for S4 class WeatherStation
#
# Author: Ting Gao (ting2)
#######################################
library(testthat)

createTestDf <- function(){
  data.frame(
             City= c("A", "BC"),
             State=c("CA", "IL"),
             Country=c("Santa Clara", "Evanston"),
             Id=c("af4", "safsAS"),
             Lat=c(12.3, 2.241),
             Lon=c(34.5, 8.1),
             Distance=c(2,5),
             stringsAsFactors=FALSE)
}

test_that("Create empty WeatherStation", {
          ws<-WeatherStation()
          expect_that(nrow(getDf(ws)), equals(0))
             })

test_that("Create WeatherStation correctly", {
          df <- createTestDf()
          ws<-WeatherStation(df)
          expect_that(nrow(getDf(ws)), equals(2))
             })

test_that("Create WeatherStation wrongly", {
          df <- createTestDf()
          df$Lon <- c("sadfa", "jf093")
          expect_that(WeatherStation(df), throws_error())
             })

test_that("WeatherStation subset", {
          df <- createTestDf()
          ws<-WeatherStation(df)
          ss <- subset(ws, 3)
          expect_that(nrow(getDf(ss)), equals(1))
             })

test_that("Save/Load WeatherStation", {
          df <- createTestDf()
          ws<-WeatherStation(df)
          filename <- "test.rds"
          saveDf(ws, file=filename)
          ws_loaded <- loadDf(filename)

          expect_that(getDf(ws), equals(getDf(ws_loaded)))
             })

