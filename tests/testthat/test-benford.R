context("Benford Functions")
library(benford.analysis)
test_that("Corporate Payment 1 digit, only >=10",
          { data(corporate.payment,envir=environment())
            cp <- corporate.payment$Amount[corporate.payment$Amount>=10]
            expect_that(length(cp), equals(177763))
            bfd1 <- benford(cp,1)
            expect_that(bfd1$info$n.second.order,equals(64578))
            expect_that(MAD(bfd1), equals(0.01464, tolerance=1e-03))
          }
          )
   
test_that("Corporate Payment 2 digits, only >=10",
          { data(corporate.payment,envir=environment())
            cp <- corporate.payment$Amount[corporate.payment$Amount>=10]
            expect_that(length(cp), equals(177763))
            bfd2 <- benford(cp,2)
            expect_that(bfd2$info$n.second.order,equals(64578))
            expect_that(MAD(bfd2), equals(0.00243, tolerance=1e-03))
            expect_that(head(suspectsTable(bfd2, "abs.excess.summation"))$digits,
                        equals(c(15, 26, 10, 14, 11, 50)))
            d.test <- data.frame(number=c(50.00,1153.35,1083.45,150.00,988.35,1159.35),
                                 duplicates = c(6022,2264,1185,1056,1018,976))
            d.data <- as.data.frame(head(duplicatesTable(bfd2)))
            expect_that(d.data, equals(d.test))
          }
          )
      
test_that("Census 2009 data, 2 digits, only >=10",
          {data(census.2009,envir=environment())
           pop <- census.2009$pop.2009[census.2009$pop.2009>=10]
           expect_that(length(pop), equals(19482))
           bfd.census <- benford(pop, 2)
           expect_that(dfactor(bfd.census), equals(0.74, tolerance=1e-03))
           expect_that(round(chisq(bfd.census)$statistic),is_equivalent_to(108))
          }
           )

test_that("Negative numbers, simulated log-normal *(-1)",
         {set.seed(1)
          data <- rlnorm(10000, 10, 10)
          data <- data*(-1)
          bfd <- benford(data, sign="negative")
          test<-structure(list(statistic = c("Mean Mantissa", "Var Mantissa", 
                                       "Ex. Kurtosis Mantissa", "Skewness Mantissa"), 
                         values = structure(c(0.496354817370674,0.0819078569969828, -1.18345152468828, 0.0122274722811928), 
                                            .Names = c("mean.mantissa", "var.mantissa", "ek.mantissa", "sk.mantissa"))), 
                    .Names = c("statistic","values"), row.names = c(NA, -4L), 
                    class = c("data.table", "data.frame"))
          mant <- mantissa(bfd)
          expect_that(test, equals(mant))
         }
)
          
          
