
# Tests -------------------------------------------------------------------

DF <- function(data){
  data <- data[data >= 10]
  collapsed <- 10 * (data) / 10^trunc(log10(data))
  DF <- (mean(collapsed) - 39.0865) / (39.0685)
  DF * 100
}

mantissa.arc.test <- function(data, data.name){ #data must be the mantissa of the log10
  x.coord <- cos(2*pi*data)
  y.coord <- sin(2*pi*data)
  
  L2 <- (mean(x.coord))^2 + (mean(y.coord))^2
  names(L2) <- "L2"
  
  p.value <- exp(-(L2)*length(data))
  mantissa.df <- 2
  names(mantissa.df) <- "df"
  
  mat.bfd <- list(statistic = L2,
                  parameter = mantissa.df,
                  p.value = p.value,
                  method = "Mantissa Arc Test",
                  data.name = data.name)
  
  class(mat.bfd) <- "htest"
  
  return(mat.bfd)
}


chisq.test.bfd <- function(squared.diff, data.name){
  
  chisq <- sum(squared.diff)
  names(chisq) <- "X-squared"
  
  df <- length(squared.diff) - 1
  names(df) <- "df"
  chisq.p.value <- pchisq(chisq, df, lower.tail = FALSE)
  
  
  chisq.bfd <- list(statistic = chisq,
                    parameter = df,
                    p.value = chisq.p.value,
                    methods = "Pearson's Chi-squared test",
                    data.name = data.name)
  
  class(chisq.bfd) <- "htest"
  
  return(chisq.bfd)
}

ks.test.bfd <- function(expected.prop, actual.prop, n.records, data.name){
  cs.ep <- cumsum(expected.prop)
  cs.ap <- cumsum(actual.prop)
  D <- max(abs(cs.ep - cs.ap))
  names(D) <- "D"
  cv <- 1.36/sqrt(n.records)
  names(cv) <- "critical value"
  alpha <- 0.05
  names(alpha) <- "alpha"
  
  ks.bfd <- list(statistic = D,
                 method = "Kolmogorov-Smirnov test",
                 parameter = c(cv, alpha),
                 data.name = data.name)
  
  class(ks.bfd) <- "htest"

  return(ks.bfd)
}

z.stat.bfd <- function(expected.prop, actual.prop, n.records){
  (abs(actual.prop - expected.prop) - 1/(2*n.records))/sqrt(expected.prop*(1-expected.prop)/n.records)
}


##' MAD conformity to Benford's Law using the MAD
##' 
##' This function checks the MAD against the conformity criteria proposed by Nigrini (2012).
##' 
##' @param MAD The mean absolute deviation, as computed in the function \code{\link{benford}}
##' @param digits.used How many digits used in the analysis.
##' 
##' @return A list with the MAD, digits.used and the conformity level.
##' 
##' @references Nigrini, M. J. (2012). Benford's Law: Application for Forensic Accounting, Auditing and Fraud Detection. Wiley and Sons: New Jersey.
MAD.conformity <- 	function(MAD = NULL,
                            digits.used = c("First Digit",
                                            "Second Digit",
                                            "First-Two Digits",
                                            "First-Three Digits")){
  Conformity.Levels <- c("Close conformity", 
                         "Acceptable conformity", 
                         "Marginally acceptable conformity", 
                         "Nonconformity")
  mad.intervals <- switch(digits.used,
                          "First Digit" = c(0.000, 0.006, 0.012, 0.015),
                          "Second Digit" = c(0.000, 0.008, 0.010, 0.011),
                          "First-Two Digits" = c(0.000, 0.0012, 0.0018, 0.0022),
                          "First-Three Digits" = c(0.000, 0.00036, 0.00044, 0.00050)
  )
  
  conformity <- Conformity.Levels[findInterval(MAD, mad.intervals)]
  
  out <- list(MAD = MAD, digits.used = digits.used, conformity = conformity)
  
  return(out)
}




# Extraction functions ----------------------------------------------------





#' @title Extracts the leading digits from the data
#' @description It extracts the leading digits from the data.
#' 
#'This function is used by the main function of the package \code{\link{benford}} to extract the 
#'leading digits of the data.
#' @usage
#' 
#' extract.digits(data, number.of.digits = 2, 
#'                sign="positive", second.order = FALSE, discrete=TRUE, round=3)
#' @param data a numeric vector. 
#' @param number.of.digits how many first digits to analyze .
#' @param sign  The default value for sign is "positive" and it analyzes only data greater than zero. 
#' There are also the options "negative" and "both" that will analyze only negative values or both positive and negative values of the data,
#' respectively. For large datasets with both positive and negative numbers, 
#' it is usually recommended to perform a separate analysis for each group,
#' for the incentives to manipulate the numbers are usually different.
#' @param second.order If TRUE, the function will extract the first digits of the second order distribution.
#' @param discrete Most real data - like population numbers or accounting data - are discrete, so 
#' the default is TRUE. This paramater sets rounding to the differences of the ordered data to avoid floating point number
#' errors in the second order distribution, that usually occurs when data is discrete
#' and the ordered numbers are very close to each other. If your data is continuous
#' (like a simulated lognormal) you should run with discrete = FALSE. 
#' @param round it defines the number of digits that the rounding will use if discrete = TRUE and second.order = TRUE.
#' @return A data.frame with the data and the first digits.
#' @export
extract.digits <- function(data, number.of.digits = 2, sign="positive", second.order = FALSE, discrete = TRUE, round = 3) {
  
  if (!is.numeric(data)) stop("Data must be a numeric vector")
  
  ## cleaning data for analysis - only > 0 and either only positive or only negative
  if (sign == "positive")  positives <- data[data > 0 & !is.na(data)]
  if (sign == "negative")  positives <- data[data < 0 & !is.na(data)]*(-1)
  if (sign == "both")      positives <- abs(data[data != 0 & !is.na(data)]) 
  
  if (second.order) {
    
    if (number.of.digits > 4) {
      warning("There might be some floating point precision issues on the Second Order distribution")
    }
    
    n <- length(positives)
    first <- sort(positives)[1:(n - 1)]
    second <- sort(positives)[2:n]
    
    positives <-  if (discrete) {
      round(second - first, number.of.digits + round)
    } else {
      round(second - first, 8)
    } 
    
    positives <- positives[positives > 0]
  }
  
  results <- data.frame(data = positives,
                        data.digits = trunc((10^((floor(log10(positives))*-1) + 
                                                   number.of.digits - 1))*positives))
  return(results)
}

#' @title Trunc decimal values.
#' @description It trunc decimal values from the data.
#' 
#'This function is used by the main function of the package \code{\link{benford}}
#' to trunc the values in its ndec argument to the specified number of decimal places. 
#'
#' @param x a numeric vector. 
#' @param n.dec integer indicating the number of decimal places (trunc) to be used.
#' @return A numeric vector with the data with deciml values truncated.
#' @export

truncDec <- function(x, n.dec = 2){
  int <- format(floor(x), scientific = FALSE)
  int <- gsub("[ ]*", "",  int)
  x.as.str <- format(x, scientific = FALSE)
  dec <- gsub("[ ]*", "",  x.as.str)
  isDec <- grepl("\\.", dec)
  dec <- sub("^[0-9]*\\.", "", dec)
  truncateDec <- substr(dec, 1, n.dec)
  truncatedNumber <- vector("character", length = length(dec))
  truncatedNumber[isDec] <- paste(int[isDec], ".", truncateDec[isDec], sep = "")
  truncatedNumber[!isDec] <- as.character(int[!isDec])
  truncatedNumber <- as.numeric(truncatedNumber)
  return(truncatedNumber)
} 


#' @title Extracts the last two digits from the data
#' @description It extracts the last two digits from the data.
#' 
#'This function is used by the main function of the package \code{\link{benford}} to extract the 
#'ast two digits of the data.
#'
#' @param data a numeric vector. 
#' @param ndec it specifies the number of decimals to be used (default 2).
#' @param sign The default value for sign is "positive" and it analyzes only data greater than zero. 
#' There are also the options "negative" and "both" that will analyze only negative values or both positive and negative values of the data,
#' respectively. For large datasets with both positive and negative numbers, 
#' it is usually recommended to perform a separate analysis for each group,
#' for the incentives to manipulate the numbers are usually different.
#' @return A data.frame with the data and the last digits.
#' @export

last.two.digits <- function(data, sign="positive", ndec = 2) {
  
  if (!is.numeric(data)) stop("Data must be a numeric vector")
  
  ## cleaning data for analysis - only > 0 and either only positive or only negative
  if (sign == "positive")  positives <- data[data > 0 & !is.na(data)]
  if (sign == "negative")  positives <- data[data < 0 & !is.na(data)]*(-1)
  if (sign == "both")      positives <- abs(data[data != 0 & !is.na(data)]) 
  
  remainder <- (positives - floor(positives))
  if (all(remainder == 0)){
    warning("Data appears to be integers, so the argument 'ndec' is set to zero.")
    ndec <- 0
  } 
  
  truncated.values <- truncDec(positives, ndec)
  values.as.str <- format(truncated.values, scientific = FALSE)
  values.as.str <- gsub("[ ]*", "", values.as.str)
  if (ndec == 0){
    nchar.values <- nchar(truncated.values)
    ltd <- substr(truncated.values, nchar.values - 1, nchar.values)
    ltd <- as.numeric(ltd)
  }else{
    nozeros <- grepl("\\.", values.as.str)
    values.as.str.nz <- values.as.str[nozeros]
    dgts.after.dot <- sub("^[0-9]*\\.", "", values.as.str.nz)
    ltd.dgts.after.dot <- substr(dgts.after.dot, nchar(dgts.after.dot) - 1, nchar(dgts.after.dot))
    which.dgts.mult.10 <- nchar(ltd.dgts.after.dot) == 1
    dgts.mult.10 <- as.character(as.numeric(ltd.dgts.after.dot[which.dgts.mult.10])*10)
    ltd.dgts.after.dot[which.dgts.mult.10] <- dgts.mult.10
    values.as.str[!nozeros] <- 0
    values.as.str[nozeros] <- ltd.dgts.after.dot
    ltd <- as.numeric(values.as.str)
  }
  
  results <- data.frame(data = truncated.values,
                        data.digits = ltd)
  return(results)
}


extract.mantissa <- function(positives) {
  log <- log10(positives)
  log[log < 0] <- log[log < 0] + as.integer(log[log < 0])*(-1) + 1
  mantissa <- log - trunc(log)
  return(mantissa)
}


# Digits probability ------------------------------------------------------



#' @title Probability of a digit sequence
#' @description It calculates the probability of a digit sequence "d".
#' @usage
#' 
#' p.these.digits(d)
#' @param d a digit sequence, like 1234 ou 999999.
#' @return The probability of the sequence d.
#' @examples
#' p.these.digits(1) # 0.30103
#' p.these.digits(11) # 0.03778856
#' p.these.digits(999999) # 4.342947e-07
#' @export 
p.these.digits <- function(d){
  if (!is.numeric(d)) stop("d must be numeric or integer")
  d <- trunc(d)
  d[d < 0] <- d[d < 0]*(-1)
  prob <- log10(1 + 1/d)
  return(prob)
}







#' @title Probability of a digit at the nth position
#' @description It calculates the probability of digit "d" at the "n"th position.
#' @usage
#' 
#' p.this.digit.at.n(d,n)
#' @param d a digit from 0 to 9 (except at position n=1, where d cannot be 0, it will give you NA).
#' @param n the nth position.
#' @return The probability of d at position n.
#' @examples
#' p.this.digit.at.n(1,1) # 0.30103
#' p.this.digit.at.n(1,2) # 0.1138901
#' p.this.digit.at.n(9,3) # 0.09826716
#' matrix <- as.data.frame(round(sapply(1:4, function(x) sapply(0:9,p.this.digit.at.n,n=x)),5))
#' names(matrix) <- paste0("n=",1:4)
#' rownames(matrix) <- paste0("d=",0:9)
#' matrix # a table with the probabilities of digits 0 to 9 in positions 1 to 4.
#' @export 
p.this.digit.at.n <- function(d,n){
  if (d < 0) d <- d*(-1)
  if (d == 0 & n == 1) return(NA)
  n1 <- strsplit(as.character(d), "")[[1]]
  n1 <- length(n1)
  if (n1 > 1) stop("d must have only 1 digit. This function evaluates 1 digit at position n")
  if (!is.numeric(d)) stop("d must be numeric or integer")
  if (!is.numeric(n)) stop("n must be numeric or integer")
  if (n < 1) stop("n must be greater than 1")
  if (n == 1) return(log10(1 + 1/d))
  if (n >= 9) return(0.1)
  k <- 10^(n - 2)
  j <- (10^(n - 1)) - 1
  i <- k:j
  sum <- sum(log10(1 + 1/(10*i + d)))
  return(sum)
}



# Generating functions ----------------------------------------------------



generate.benford.digits <- function(number.of.digits) {
  number.of.digits <- as.integer(number.of.digits)
  begin <- 10^(number.of.digits - 1)
  ends <- 10^(number.of.digits) - 1
  benford.digits <- begin:ends
  return(benford.digits)
}

generate.benford.distribution <- function(benford.digits) {
  benford.dist <- sapply(benford.digits, p.these.digits)
  return(benford.dist)
}

generate.empirical.distribution <- function(data, number.of.digits, sign, last.two.dgts = FALSE, second.order = FALSE, benford.digits, discrete = TRUE, round = 3){
  x <- NULL
  v <- NULL
  if(last.two.dgts){
    data.frame <- last.two.digits(data, sign)
    DF <- data.table(x = c(data.frame$data.digits, 0:99),
                     v = c(data.frame$data.digits, 0:99))
  }else{
    data.frame <- extract.digits(data, number.of.digits, sign, second.order, discrete = discrete, round = round) 
    DF <- data.table(x = c(data.frame$data.digits, benford.digits),
                     v = c(data.frame$data.digits, benford.digits) )
  }
  n <- length(data.frame$data.digits)
  DFcount <- DF[ ,length(x) - 1, by = v][order(v)]
  dist.freq <- DFcount$V1
  dist <- dist.freq/n
  results <- list(data = data.frame$data, 
                  data.digits = data.frame$data.digits, 
                  dist = dist, 
                  dist.freq = dist.freq)
  return(results)
}



generate.summation <- function(benford.digits, data, data.digits) {
  x <- NULL
  v <- NULL
  table <- data.table(x = data.digits, v = data)
  table <- table[, sum(v), by = x][order(x)]
  setnames(table,c("x", "V1"), c("digits", "value"))
  
  if (length(which(!benford.digits %in% table$digits)) != 0) {
    add <- data.frame(digits = which(!benford.digits %in% table$digits), value = 0)
    table <- rbind(table, add)
    table <- table[order(table$digits),]
  }
  
  summation <- table$value
  return(summation)
}



#### Basic Calculations ####

excess.kurtosis <- function(x){ 
  (mean((x - mean(x))^4)/(mean((x - mean(x))^2)^2)) - 3
}


skewness <- function(x) {
  (mean((x - mean(x))^3)/(mean((x - mean(x))^2)^(3/2)))
}

