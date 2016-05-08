library(benford.analysis)

context("Generating Benford Digits Probabilities")

test_that("p.this.digit.at.n",{
  test <- structure(list(`n=1` = c(NA, 0.30103, 0.17609, 0.12494, 0.09691,0.07918, 0.06695, 0.05799, 0.05115, 0.04576), 
                         `n=2` = c(0.11968,0.11389, 0.10882, 0.10433, 0.10031, 0.09668, 0.09337, 0.09035, 0.08757, 0.085), 
                         `n=3` = c(0.10178, 0.10138, 0.10097, 0.10057,0.10018, 0.09979, 0.0994, 0.09902, 0.09864, 0.09827), 
                         `n=4` = c(0.10018,0.10014, 0.1001, 0.10006, 0.10002, 0.09998, 0.09994, 0.0999,0.09986, 0.09982)), 
                    .Names = c("n=1", "n=2", "n=3", "n=4"), 
                    row.names = c("d=0","d=1", "d=2", "d=3", "d=4", "d=5", "d=6", "d=7", "d=8", "d=9"), 
                    class = "data.frame")
  matrix <- as.data.frame(round(sapply(1:4, function(x) sapply(0:9,p.this.digit.at.n,n=x)),5))
  names(matrix) <- paste0("n=",1:4)
  rownames(matrix) <- paste0("d=",0:9)
  expect_that(matrix, equals(test))
  
  expect_that(p.this.digit.at.n("a",1), throws_error("d must be numeric or integer"))
  expect_that(p.this.digit.at.n(0,1), equals(NA))
  expect_that(p.this.digit.at.n(2,"b"), throws_error("n must be numeric or integer"))
  expect_that(p.this.digit.at.n(2123,0), throws_error("d must have only 1 digit. This function evaluates 1 digit at position n"))
  expect_that(p.this.digit.at.n(1,0),throws_error("n must be greater than 1"))
  expect_that(p.this.digit.at.n(-3,3), equals(p.this.digit.at.n(3,3)))
  expect_that(p.this.digit.at.n(1,9), equals(0.1))
}
)

test_that("p.these.digits",{
  expect_that(p.these.digits(1), equals(0.30103))
  expect_that(p.these.digits(11), equals(0.03778856, tolerance=1e-07))
  expect_that(p.these.digits(999999), equals(4.342947e-07))
  expect_that(p.these.digits("123"), throws_error("d must be numeric or integer"))
  expect_that(p.these.digits(-123),equals(p.these.digits(123)))
}
)

  
