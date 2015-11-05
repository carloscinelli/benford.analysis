library(benford.analysis)

context("Extracting Digits")

test_that("Exctracting digits from made up data",
{
  data <- c(213,65468,878942,132456,6878,32132,45665,41656,138744,23465)
  
  digits1 <- extract.digits(data, 1)$data.digits
  expect_that(digits1, 
              equals(c(2, 6, 8, 1, 6, 3, 4, 4, 1, 2)))
  
  digits2 <- extract.digits(data)$data.digits
  expect_that(digits2, 
              equals(c(21, 65, 87, 13, 68, 32, 45, 41, 13, 23)))
  
  digits3 <- extract.digits(data, 3)$data.digits
  expect_that(digits3,
              equals(c(213, 654, 878, 132, 687, 321, 456, 416, 138, 234)))
  
  digits4 <- extract.digits(data, 4)$data.digits
  expect_that(digits4,
              equals(c(2130, 6546, 8789, 1324, 6878, 3213, 4566, 4165, 1387, 2346)))
  
  data <- as.integer(data)
  digits3 <- extract.digits(data, 3)$data.digits
  expect_that(digits3,
              equals(c(213, 654, 878, 132, 687, 321, 456, 416, 138, 234)))
  
  
  set.seed(1)
  x <- rnorm(10)
  
  rd1 <- extract.digits(x)
  expect_that(nrow(rd1), equals(length(x[x>0])))
  
  rd2 <- extract.digits(x, sign="negative")
  expect_that(nrow(rd2), equals(length(x[x<0])))
  
  rd3 <- extract.digits(x, second.order=TRUE, discrete=FALSE)
  expect_that(nrow(rd3), equals(length(x[x>0])-1))
  
  discrete <- c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
  
  d1 <- extract.digits(discrete,1, second.order=TRUE, discrete=TRUE)
  expect_that(d1$data.digits, equals(rep(1,7)))
  
  d2 <- extract.digits(discrete,2, second.order=TRUE, discrete=TRUE)
  expect_that(d2$data.digits, equals(rep(10,7)))
  
  d3 <- extract.digits(discrete,4, second.order=TRUE, discrete=TRUE, round=12)
  expect_that(d3$data.digits, equals(rep(1000, 7)))
  
  d4 <- extract.digits(discrete,4, second.order=TRUE, discrete=TRUE, round=16)
  expect_that(d4$data.digits, equals(c(1000,999,1000,999,999,999,1000)))
  
  expect_that(extract.digits("asbdas"), throws_error("Data must be a numeric vector"))
  expect_warning(extract.digits(data, number.of.digits = 5, second.order = TRUE), 
              "There might be some floating point precision issues on the Second Order distribution")
  
})
