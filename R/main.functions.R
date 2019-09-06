#### Package benford.analysis ####

##' Benford Analysis for data validation and forensic analytics
##' 
##' The Benford Analysis package provides tools that make it easier to
##' validate data using Benford's Law. The main purpose of the package is
##' to identify suspicious data that need further verification.
##' 
##' More information can be found on its help documentation.
##' 
##' The main function is \code{\link{benford}}. It generates a \code{Benford} S3 object.
##' 
##' The package defines S3 methods for plotting and printing Benford type objects.
##' 
##' After running \code{benford} you can easily get the "suspicious" data by using the 
##' functions: \code{\link{suspectsTable}}, \code{\link{getSuspects}}, \code{\link{duplicatesTable}} and
##' \code{\link{getDuplicates}}. See help documentation and examples for further details.
##' 
##' The package also includes 6 real datasets for illustration purposes.
##' 
##' @examples
##' data(corporate.payment) #gets data
##' cp <- benford(corporate.payment$Amount, 2, sign="both") #generates benford object
##' cp #prints 
##' plot(cp) #plots
##' 
##' head(suspectsTable(cp),10) #prints the digits by decreasing order of discrepancies
##' 
##' #gets observations of the 2 most suspicious groups
##' suspects <- getSuspects(cp, corporate.payment, how.many=2) 
##' 
##' duplicatesTable(cp) #prints the duplicates by decreasing order
##' 
##' #gets the observations of the 2 values with most duplicates
##' duplicates <- getDuplicates(cp, corporate.payment,how.many=2) 
##' 
##' MAD(cp) #gets the Mean Absolute Deviation
##' 
##' chisq(cp) #gets the Chi-squared test
##' 
##' #gets observations starting with 50 or 99
##' digits_50_and_99 <- getDigits(cp, corporate.payment, digits=c(50, 99)) 
##' 
##' @references Alexander, J. (2009). Remarks on the use of Benford's Law. Working Paper, Case Western Reserve University, Department of Mathematics and Cognitive Science.
##' \cr\cr Berger, A. and Hill, T. (2011). A basic theory of Benford's Law. Probability Surveys, 8, 1-126.
##' \cr\cr Hill, T. (1995). A statistical derivation of the significant-digit law. Statistical Science, 10(4), 354-363.
##' \cr\cr Nigrini, M. J. (2012). Benford's Law: Application for Forensic Accounting, Auditing and Fraud Detection. Wiley and Sons: New Jersey.
##' \cr\cr Nigrini, M. J. (2011). Forensic Analytics: Methods and Techniques for Forensic Accounting Investigations.Wiley and Sons: New Jersey.
##' \cr\cr 
##' 
##' @docType package
##' @aliases benford.analysis-package
##' @name benford.analysis
##' 
NULL

##' @title Benford Analysis of a dataset
##' @description This function validates a dataset using Benford's Law.
##' Its main purposes are to find out where the dataset deviates from Benford's Law and 
##' to identify suspicious data that need further verification. 
##' 
##' For a more complete example, see the package help at \link{benford.analysis}.
##' @param data a numeric vector.
##' @param number.of.digits how many first digits to analyze.
##' @param sign  The default value for sign is "positive" and it analyzes only data greater than zero. 
##' There are also the options "negative" and "both" that will analyze only negative values or both positive and negative values of the data,
##' respectively. For large datasets with both positive and negative numbers, 
##' it is usually recommended to perform a separate analysis for each group,
##' for the incentives to manipulate the numbers are usually different.
##' @param discrete most real data - like population numbers or accounting data - are discrete, so 
##' the default is TRUE. This parameter sets rounding to the differences of the ordered data to avoid floating point number
##' errors in the second order distribution, that usually occurs when data is discrete
##' and the ordered numbers are very close to each other. If your data is continuous
##' (like a simulated lognormal) you should run with discrete = FALSE. 
##' @param round it defines the number of digits that the rounding will use if discrete = TRUE.
##' @param data.name the name of your data to show. If \code{NULL} (the default), the name of the passed object will be used.
##' @return An object of class Benford containing the results of the analysis. It is a list of 
##' eight objects, namely:
##' 
##' \item{info}{general information, including \itemize{
##' \item data.name: the name of the data used.
##' \item n: the number of observations used.
##' \item n.second.order: the number of observations used for second order analysis.
##' \item number.of.digits: the number of first digits analyzed.
##' }}
##' 
##' \item{data}{a data frame with: \itemize{
##' \item lines.used: the original lines of the dataset.
##' \item data.used: the data used.
##' \item data.mantissa: the log data's mantissa.
##' \item data.digits: the first digits of the data.
##' }}
##' 
##' \item{s.o.data}{a data frame with: \itemize{
##' \item data.second.order: the differences of the ordered data.
##' \item data.second.order.digits: the first digits of the second order analysis.
##' }}
##' 
##' \item{bfd}{a data frame with: \itemize{
##' \item digits: the groups of digits analyzed.
##' \item data.dist: the distribution of the first digits of the data.
##' \item data.second.order.dist: the distribution of the first digits of the second order analysis.
##' \item benford.dist: the theoretical benford distribution.
##' \item data.second.order.dist.freq: the frequency distribution of the first digits of the second order analysis.
##' \item data.dist.freq: the frequency distribution of the first digits of the data.
##' \item benford.dist.freq: the theoretical benford frequency distribution.
##' \item benford.so.dist.freq: the theoretical benford frequency distribution of the second order analysis.
##' \item data.summation: the summation of the data values grouped by first digits.
##' \item abs.excess.summation: the absolute excess summation of the data values grouped by first digits.
##' \item difference: the difference between the data and benford frequencies.
##' \item squared.diff: the chi-squared difference between data and benford frequencies.
##' \item absolute.diff: the absolute difference between data and benford frequencies.
##' \item z.statistic: the z-statistic difference between data and benford relative frequencies.
##' }}
##' 
##' \item{mantissa}{a data frame with: \itemize{
##' \item mean.mantissa: the mean of the mantissa.
##' \item var.mantissa: the variance of the mantissa.
##' \item ek.mantissa: the excess kurtosis of the mantissa.
##' \item sk.mantissa: the skewness of the mantissa.
##' }}
##' 
##' \item{MAD}{the mean absolute deviation.}
##' \item{distortion.factor}{the distortion factor} 
##' 
##' \item{stats}{list of "htest" class statistics: \itemize{
##' \item chisq: Pearson's Chi-squared test.
##' \item mantissa.arc.test: Mantissa Arc Test.
##' \item ks.test: Kolmogorov-Smirnov Test.
##' }}
##' @examples 
##' data(corporate.payment) #loads data
##' bfd.cp <- benford(corporate.payment$Amount) #generates benford object
##' bfd.cp #prints
##' plot(bfd.cp) #plots
##' 
##' @export


benford <-
function(data, number.of.digits = 2, 
         sign = "positive", 
         discrete = TRUE, round = 3, 
         data.name = NULL)
{
  
  if (!is.numeric(data)) stop("Data must be a numeric vector")
  
  if (length(data) == 1) stop("Data comprised of only one observation, no meaningful analysis is possible")
  
  if (is.null(data.name)) {
    data.name <- as.character(deparse(substitute(data)))
  }
  
  benford.digits <- generate.benford.digits(number.of.digits)
    
  benford.dist <- generate.benford.distribution(benford.digits)
  
  empirical.distribution <- generate.empirical.distribution(data, number.of.digits, sign = sign, second.order = FALSE, benford.digits = benford.digits)
  
  n <- length(empirical.distribution$data)
  
  second.order <- generate.empirical.distribution(data, number.of.digits, sign = sign, second.order = TRUE, benford.digits = benford.digits, discrete = discrete, round = round)
  
  n.second.order <- length(second.order$data)
  
  last.two.digits <- generate.empirical.distribution(data, sign = sign, last.two.dgts = TRUE, benford.digits = benford.digits)
  
  benford.dist.freq <- benford.dist*n
  
  ## calculating useful summaries and differences:
  difference <- empirical.distribution$dist.freq - benford.dist.freq
  squared.diff <- ((empirical.distribution$dist.freq - benford.dist.freq)^2)/benford.dist.freq
  absolute.diff <- abs(empirical.distribution$dist.freq - benford.dist.freq)
  z.stat <- z.stat.bfd(benford.dist, empirical.distribution$dist, n)
  chisq.bfd <- chisq.test.bfd(squared.diff, data.name)
  mean.abs.dev <- sum(abs(empirical.distribution$dist - benford.dist)/(length(benford.dist)))
  
  if (number.of.digits > 3) {
    MAD.conformity <- NA
  } else {
    digits.used <- c("First Digit", "First-Two Digits", "First-Three Digits")[number.of.digits]  
    MAD.conformity <- MAD.conformity(MAD = mean.abs.dev, digits.used)$conformity
  }
  
  ## Summation
  summation <- generate.summation(benford.digits,empirical.distribution$data, empirical.distribution$data.digits)
  abs.excess.summation <- abs(summation - mean(summation))
  
  ### Mantissa
  mantissa <- extract.mantissa(empirical.distribution$data)
  mean.mantissa <- mean(mantissa)
  var.mantissa <- var(mantissa)
  ek.mantissa <- excess.kurtosis(mantissa)
  sk.mantissa <- skewness(mantissa)
  
  ### Mantissa Arc Test
  mat.bfd <- mantissa.arc.test(mantissa, data.name)
  
  ### Distortion Factor
  distortion.factor <- DF(empirical.distribution$data)  
  
  ### Kolmogorov-Smirnov test
  ks.test <- ks.test.bfd(benford.dist, empirical.distribution$dist, n, data.name)
  
  ## recovering the lines of the numbers
  if (sign == "positive") lines <- which(data > 0 & !is.na(data))
  if (sign == "negative") lines <- which(data < 0 & !is.na(data))
  if (sign == "both")     lines <- which(data != 0 & !is.na(data))
  #lines <- which(data %in% empirical.distribution$data)
  
  ## output
  output <- list(info = list(data.name = data.name,
                             n = n,
                             n.second.order = n.second.order,
                             number.of.digits = number.of.digits),
                 
                 data = data.table(lines.used = lines,
                                   data.used = empirical.distribution$data,
                                   data.mantissa = mantissa,
                                   data.digits = empirical.distribution$data.digits),
                 
                 s.o.data = data.table(second.order = second.order$data,
                                       data.second.order.digits = second.order$data.digits),
                 
                 last.two.digits = data.table(last.two.digits = 0:99,
                                              data.dist.freq = last.two.digits$dist.freq),
                 
                 bfd = data.table(digits = benford.digits,
                                  data.dist = empirical.distribution$dist,
                                  data.second.order.dist = second.order$dist,
                                  benford.dist = benford.dist,
                                  data.second.order.dist.freq = second.order$dist.freq,
                                  data.dist.freq = empirical.distribution$dist.freq,
                                  benford.dist.freq = benford.dist.freq,
                                  benford.so.dist.freq = benford.dist*n.second.order,
                                  data.summation = summation,
                                  abs.excess.summation = abs.excess.summation,
                                  difference = difference,
                                  squared.diff = squared.diff,
                                  absolute.diff = absolute.diff,
                                  z.statistic = z.stat),
                 
                 mantissa = data.table(statistic = c("Mean Mantissa", 
                                                     "Var Mantissa", 
                                                     "Ex. Kurtosis Mantissa",
                                                     "Skewness Mantissa"),
                                       values = c(mean.mantissa = mean.mantissa,
                                                  var.mantissa = var.mantissa,
                                                  ek.mantissa = ek.mantissa,
                                                  sk.mantissa = sk.mantissa)),
                 MAD = mean.abs.dev,
                 
                 MAD.conformity = MAD.conformity,
                 
                 distortion.factor = distortion.factor,
                 
                 stats = list(chisq = chisq.bfd,
                              mantissa.arc.test = mat.bfd,
                              ks.test = ks.test)
  )
  
  class(output) <- "Benford"
  
  return(output)
  
}


##' @title Print method for Benford Analysis
##' @description The \code{print} method for "Benford" objects.
##' @usage 
##' 
##' \method{print}{Benford}(x, how.many=5, ...)
##' @param  x a "Benford" object.
##' @param how.many a number that defines how many of the biggest absolute differences to show.
##' @param ... arguments to be passed to generic print functions.
##' @return Prints the Benford object.
##' @export


print.Benford <-
function(x, how.many = 5,...)
{

  if(class(x)!="Benford") stop("Class(x) must be 'Benford'")
  cat("\nBenford object:\n",
      "\nData:", x[["info"]]$data.name,
      "\nNumber of observations used =", x[["info"]]$n,
      "\nNumber of obs. for second order =", x[["info"]]$n.second.order,
      "\nFirst digits analysed =", x[["info"]]$number.of.digits)    
  
  cat("\n\nMantissa: \n")
  cat("\n")
  pretty_print <- x$mantissa
  pretty_print$statistic <- gsub("Mantissa|\\s", "", pretty_print$statistic)
  pretty_print <- setNames(pretty_print, c("Statistic", "Value"))
  print.data.frame(pretty_print, row.names = FALSE, digits = 2)
  cat("\n")
  
  cat("\nThe", how.many, "largest deviations: \n")
  cat("\n")
  print.data.frame(round(head(x[["bfd"]][order(absolute.diff, decreasing=TRUE)][,list(digits, absolute.diff)], how.many), 2))
  cat("\n")
  
  cat("Stats:\n")
  print(x[["stats"]]$chisq)
  print(x[["stats"]]$mantissa.arc.test)
  print(x[["stats"]]$ks.test)
  cat("Mean Absolute Deviation (MAD):",x[["MAD"]])
  if (!is.na(x[["MAD.conformity"]])) 
    cat("\nMAD Conformity - Nigrini (2012):", x[["MAD.conformity"]])
  cat("\nDistortion Factor:", x[["distortion.factor"]])
  cat("\n\nRemember: Real data will never conform perfectly to Benford's Law. You should not focus on p-values!")
  values <- NULL
  absolute.diff <- NULL
  digits <- NULL
}


##' @title Summary method for Benford Analysis
##' @description The \code{summary} method for "Benford" objects.
##' 
##' 
##' @param x a "Benford" object
##' @param what options: "first digits", "summation", "last two digits", "second order".
##' @param sort.by (not implemented)
##' @param freq (not implemented)
##' @return Summary of the Benford object.
##' @export

summary.Benford <- function(x, what = c("first digits", "summation", "last two digits", "second order"), sort.by = "abs diff", freq = TRUE){
  
  if (class(x) != "Benford") stop("Class(x) must be 'Benford'")
  
  if(class(x)!="Benford") stop("Class(x) must be 'Benford'")
  cat("\nBenford object:\n",
      "\nData:", x[["info"]]$data.name,
      "\nNumber of observations used (positives, negatives or both?) =", x[["info"]]$n,
      "\nFirst digits analysed =", x[["info"]]$number.of.digits)   
  
  what <- tolower(what)
  
  for(i in 1:length(what)){
    switch(what[i], 
           "first digits" = {
             cat("First Digits Analysis:\n\n")
             print.first.digit.analysis(x)
             cat("\n\n")
           },
           "summation" = {
             cat("Summation Analysis:\n\n")
             print.summation.analysis(x)
             cat("\n\n")
           },
           "last two digits" = {
             cat("Last-Two Digits Analysis:\n\n")
             print.last.two.digits.analysis(x)
             cat("\n\n")
           },
           "second order" = {
             cat("Second Order Analysis:\n\n")
             print.second.order.analysis(x)
             cat("\n\n")
           }
    )
  }
  
  cat("Remember: Real data will never conform perfectly to Benford's Law. You should not focus on p-values!")
}


print.first.digit.analysis <- function(x, freq = TRUE, ...)
{
  
  out <- list()
  statistics <- getBfd(x)
  first.digits <- statistics[, c("digits", "data.dist.freq", "benford.dist.freq", "absolute.diff", "squared.diff", "z.statistic")]
  names(first.digits) <- c("Digits", "Count", "Benford's Law", "Absolute Diff.", "Squared Diff.", "Z-statistic")
  
  print(first.digits, topn = 4)
  
  cat("---\n")
  
  cat("\nMean Absolute Deviation (MAD): ", MAD(x))
  if (!is.na(x[["MAD.conformity"]]))
    cat(" - Conclusion:", x[["MAD.conformity"]], "\n")
  
  cat(paste0(chisq(x)$methods, ": X-squared = ", round(chisq(x)$statistic, 7), " on ", chisq(x)$parameter, " DF, ", "p-value: ", format.pval(chisq(x)$p.value), "\n"))
  cat(paste0(ks(x)$method, ": D = ", round(ks(x)$statistic, 7), ", critical value = ", round(ks(x)$parameter[1], 7), " for alpha = ", 0.05, "\n"))
  
  out$data <- first.digits
  out$MAD <- MAD(x)
  if (!is.na(x[["MAD.conformity"]]))
    out$MAD.Conformity <- x[["MAD.conformity"]]
  out$chisq <- chisq(x)
  out$ks <- ks(x)
  invisible(out)
}

print.last.two.digits.analysis <- function(x, freq = TRUE, ...)
{
  
  out <- list()
  last2digits <- x$last.two.digits
  last2digits$bf <- mean(last2digits$data.dist.freq)
  absolute.diff <- abs(last2digits$data.dist.freq - last2digits$bf)
  squared.diff <- ((last2digits$data.dist.freq - last2digits$bf)^2)/last2digits$bf
  ep <- last2digits$bf/sum(last2digits$bf)
  ap <- last2digits$data.dist.freq/sum(last2digits$data.dist.freq)
  z.stat <- z.stat.bfd(ep, ap, sum(last2digits$data.dist.freq))
  last2digits$absolute.diff <- absolute.diff
  last2digits$squared.diff <- squared.diff
  last2digits$z.stat <- z.stat
  names(last2digits) <- c("Last-Two Digits", "Count", "Benford's Law", "Absolute Diff.", "Squared Diff.", "Z-statistic")
  
  print(last2digits, topn = 4)
  cat("---\n")
  
  mean.abs.dev <- sum(abs(ep - ap)/(nrow(last2digits)))
  chisq.bfd <- chisq.test.bfd(squared.diff, "")
  ks.bfd <- ks.test.bfd(ep, ap, sum(last2digits$Count), "")
  
  cat("\nMean Absolute Deviation (MAD): ", mean.abs.dev, "\n")
  cat(paste0("Pearson's Chi-squared test", ": X-squared = ", round(chisq.bfd$statistic, 7), " on ", chisq.bfd$parameter, " DF, ", "p-value: ", format.pval(chisq.bfd$p.value), "\n"))
  cat(paste0("Kolmogorov-Smirnov test", ": D = ", round(ks.bfd$statistic, 7), ", critical value = ", round(ks.bfd$parameter[1], 7), " for alpha = ", 0.05, "\n"))
  
  out$data <- last2digits
  out$MAD <- mean.abs.dev
  out$chisq <- chisq.bfd
  out$ks <- ks.bfd
  invisible(out)
}


print.second.order.analysis <- function(x, ...)
{
  
  out <- list()
  statistics <- getBfd(x)
  second.order <- statistics[, c("digits", "data.second.order.dist.freq", "benford.so.dist.freq")]
  
  absolute.diff <- abs(second.order$data.second.order.dist.freq - second.order$benford.so.dist.freq)
  squared.diff <- ((second.order$data.second.order.dist.freq - second.order$benford.so.dist.freq)^2)/second.order$benford.so.dist.freq
  ep <- second.order$benford.so.dist.freq/sum(second.order$benford.so.dist.freq)
  ap <- second.order$data.second.order.dist.freq/sum(second.order$data.second.order.dist.freq)
  z.stat <- z.stat.bfd(ep, ap, sum(second.order$data.second.order.dist.freq))
  second.order$absolute.diff <- absolute.diff
  second.order$squared.diff <- squared.diff
  second.order$z.stat <- z.stat
  names(second.order) <- c("Digits", "Count", "Benford's Law", "Absolute Diff.", "Squared Diff.", "Z-statistic")
  
  print(second.order, topn = 4)
  cat("---\n")
  
  mean.abs.dev <- sum(abs(ep - ap)/(nrow(second.order)))
  chisq.bfd <- chisq.test.bfd(squared.diff, "")
  ks.bfd <- ks.test.bfd(ep, ap, sum(second.order$Count), "")
  
  cat("\nMean Absolute Deviation (MAD): ", mean.abs.dev, "\n")
  cat(paste0("Pearson's Chi-squared test", ": X-squared = ", round(chisq.bfd$statistic, 7), " on ", chisq.bfd$parameter, " DF, ", "p-value: ", format.pval(chisq.bfd$p.value), "\n"))
  cat(paste0("Kolmogorov-Smirnov test", ": D = ", round(ks.bfd$statistic, 7), ", critical value = ", round(ks.bfd$parameter[1], 7), " for alpha = ", 0.05, "\n"))
  
}


print.summation.analysis <- function(x, ...)
{
  
  statistics <- getBfd(x)
  bl <- mean(statistics$data.summation)
  summation.analysis <- data.table(statistics[, c("digits", "data.summation", "abs.excess.summation")], bl)
  summation.analysis <- summation.analysis[, c(1,2,4,3)]
  summation.analysis <- summation.analysis[order(summation.analysis$abs.excess.summation, decreasing = T), ]
  names(summation.analysis) <- c("Digits", "Summation", "Benford's Law", "Abs. Excess Summation")
  
  cat("The largest deviations:\n\n")
  print(head(summation.analysis))
  
}
