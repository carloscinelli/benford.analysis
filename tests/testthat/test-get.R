library(benford.analysis)
context("Get functions")

test_that("Get functions are working",
          {
            data("sino.forest")
            bfd <- benford(sino.forest$value)
            suspects <- getSuspects(bfd, sino.forest)
            test_suspects <- structure(list(value = c(1238185, 1252023, 128124, 8756, 1223352, 
                                                      125238, 87670, 12156, 1261300, 1213495, 12200, 12996000, 1292000, 
                                                      12744, 1297, 12, 12156, 8717000, 877000, 1294, 128122, 1211, 
                                                      127681, 87670, 87670, 87670, 87670000, 1264000, 1213495, 125000, 
                                                      87000, 12200, 12317, 8756, 8756, 1264, 1264, 123233000, 1255453, 
                                                      87670000, 87670000, 1223352000, 87670, 121544, 8715, 122793, 
                                                      1229842000, 12755000, 8793000, 127000, 122855000, 123, 12401, 
                                                      12, 1261524, 1207281, 1270954)), 
                                       .Names = "value", row.names = c(NA, -57L), 
                              class = c("data.table", "data.frame"))
            expect_that(suspects, equals(test_suspects))
            expect_that(getSuspects("a"), throws_error("bfd must be a 'Benford' object."))
            expect_that(getSuspects(bfd, sino.forest, "diff"), 
                        throws_error("By must be one of these: 'abs.excess.summation','difference','squared.diff','absolute.diff'"))
            expect_that(getSuspects(bfd, "a", "diff"), 
                        throws_error("Data must be a data frame."))
            
            
            duplicates <- getDuplicates(bfd, sino.forest)
            test_duplicates <- structure(list(value = c(1670, 87670, 1670, 87670, 87670, 87670, 
                                                        1670, 1670, 87670)), 
                                         .Names = "value", 
                                         row.names = c(NA, -9L), 
                                         class = c("data.table", "data.frame"))
            expect_that(duplicates, equals(test_duplicates))
            expect_that(getDuplicates(mtcars), throws_error("bfd must be a 'Benford' object."))
            expect_that(getDuplicates(bfd, 1), throws_error("Data must be a data frame."))
            
            digits <- getDigits(bfd, sino.forest, 10)
            test_digits <- structure(list(value = c(10609, 1054257, 103991, 10602, 1054257, 
                                                    1032009, 10942, 106865, 10214, 10703, 1047, 10058, 10905, 1068, 
                                                    1e+07, 10.12, 1055002, 1055002, 103854, 101195, 1031, 1e+07, 
                                                    1e+07, 108327000, 1060000, 107400000, 1007, 10328, 1074516, 1053191, 
                                                    1026, 10377, 100, 101, 107, 1040916)), 
                                     .Names = "value", row.names = c(NA, -36L), class = c("data.table", "data.frame"))
            expect_that(digits, equals(test_digits))
            
            data <- getData(bfd)
            expect_that(data, equals(bfd$data))
            expect_that(getData(1), throws_error("Object must be of class Benford"))
            
            bfd_test <- getBfd(bfd)
            expect_that(bfd_test, equals(bfd$bfd))
            expect_that(getBfd(1), throws_error("Object must be of class Benford"))
            
            mant <- marc(bfd)
            test_mant <- structure(list(statistic = structure(0.00216678894007925, .Names = "L2"), 
                                        parameter = structure(2, .Names = "df"), p.value = structure(0.187728020755151, .Names = "L2"), 
                                        method = "Mantissa Arc Test", data.name = "sino.forest$value"), 
                                   .Names = c("statistic", "parameter", "p.value", "method", "data.name"), 
                                   class = "htest")
            expect_that(mant, equals(test_mant))
            expect_that(marc(1), throws_error("Object must be of class Benford"))
            
            expect_that(suspectsTable(bfd, "diff"), 
                        throws_error("By must be one of these: 'abs.excess.summation','difference','squared.diff','absolute.diff'"))
          })
