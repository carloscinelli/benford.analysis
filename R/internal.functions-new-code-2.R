#### Benford ####

DF <- function(data){
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
#' @param number.of.digits how many first digits to analyse .
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
      second - first
    } 
    
    positives <- positives[positives > 0]
  }
  
  results <- data.frame(data = positives,
                        data.digits = trunc((10^((floor(log10(positives))*-1) + 
                                                   number.of.digits - 1))*positives))
  return(results)
}


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
  if (d < 0) d <- d*(-1)
  prob <- log10(1 + 1/d)
  return(prob)
}


#' @title Probability of a digit at the nth position
#' @description It calculates the probability of digit "d" at the "n"th position.
#' @usage
#' 
#' p.this.digit.at.n(d,n)
#' @param d a digit from 0 to 9 (except at position n=1, where d cannot be 0, it wil give you NA).
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

generate.empirical.distribution <- function(data, number.of.digits,sign, second.order = FALSE, benford.digits, discrete = TRUE, round = 3){
   x <- NULL
   v <- NULL
   data.frame <- extract.digits(data, number.of.digits, sign, second.order, discrete = discrete, round = round)
   n <- length(data.frame$data.digits)
   DF <- data.table(x = c(data.frame$data.digits, benford.digits),
                    v = c(data.frame$data.digits, benford.digits) )
   DFcount <- DF[ ,length(x) - 1, by = v][order(v)]
   dist.freq <- DFcount$V1
   dist <- dist.freq/n
   results <- list(data = data.frame$data, 
                   data.digits = data.frame$data.digits, 
                   dist = dist, 
                   dist.freq = dist.freq)
   return(results)
}

extract.mantissa <- function(positives) {
  log <- log10(positives)
  log[log < 0] <- log[log < 0] + as.integer(log[log < 0])*(-1) + 1
  mantissa <- log - trunc(log)
  return(mantissa)
}

generate.summation <- function(benford.digits, data, data.digits) {
  x <- NULL
  v <- NULL
  table <- data.table(x = data.digits, v = data)
  table <- table[, sum(v), by = x][order(x)]
  setnames(table,c("x", "V1"), c("digits", "value"))
  
  if (length(which(!benford.digits %in% table$digits))!=0) {
    add <- data.frame(digits=which(!benford.digits %in% table$digits), value=0)
    add
    table<-rbind(table, add)
    table<-table[order(table$digits),]
  }
  
  summation <- table$value
  return(summation)
}



#### Basic Calculations ####

excess.kurtosis <- function(x) 
  (mean((x - mean(x))^4)/(mean((x - mean(x))^2)^2)) - 3
  

skewness <- function(x)
  (mean((x - mean(x))^3)/(mean((x - mean(x))^2)^(3/2)))

#### plot ####

plotting.data.vs.benford <- function (x, ...) {
  
  xmarks <- barplot(x[["bfd"]]$data.dist.freq, col="lightblue", 
                    main= "Digits Distribution",
                    xlab = "Digits", ylab="Freq",
                    ylim = c(0,max(c(x[["bfd"]]$data.dist.freq, x[["bfd"]]$benford.dist.freq))*1.1))
  
  axis(1, at=xmarks, labels=x[["bfd"]]$digits)
  
  lines(xmarks, x[["bfd"]]$benford.dist.freq, lty=2, lwd=2, col="red")
  
#   legend("topright", lty=c(1,2), lwd=2, col=c("lightblue", "red"), 
#          legend=c("Data", "Benford"))
}

plotting.second.order <- function (x, ...) {
  y<-x[["bfd"]]$benford.so.dist.freq
  xmarks <- barplot(x[["bfd"]]$data.second.order.dist.freq, col="lightblue", 
                    main= "Digits Distribution \nSecond Order Test",
                    xlab = "Digits", ylab="Freq",
                    ylim = c(0,max(c(x[["bfd"]]$data.second.order.dist.freq, y))*1.1)) 
  
  axis(1, at=xmarks, labels=x[["bfd"]]$digits)
  
  lines(xmarks, y, lty=2, lwd=2, col="red")
  
#   legend("topright", lty=c(1,2), lwd=2, col=c("lightblue", "red"), 
#          legend=c("Data", "Benford"))
}

plotting.summation <- function (x, ...) {
  xmarks<-barplot(x[["bfd"]]$data.summation, col="lightblue", 
                  main="Summation Distribution by digits",
                  xlab="Digits", ylab="Summation",
                  ylim = c(0,max(x[["bfd"]]$data.summation))*1.1)
  
  axis(1, at=xmarks, labels=x[["bfd"]]$digits)
  
  lines(x=xmarks, y=rep(mean(x[["bfd"]]$data.summation), length(xmarks)), col="red", lty=2)
}

plotting.ordered.mantissa <- function (x, ...) {
  
  plot(sort(x[["data"]]$data.mantissa), pch=".",col="blue", main ="Ordered Mantissa",
       xlab="Ordered Observation",
       ylab= "Mantissa")
  
  abline(a=0, b=1/length(x[["data"]]$data.mantissa), col="red", lty=2)
}

plotting.chi_squared <- function (x, ...) {
  
  plot(x[["bfd"]]$digits, x[["bfd"]]$squared.diff, pch="x", col="blue", 
       xlab="Digits",
       ylab="Chi-squared", main= "Chi-Squared Difference",xaxt="n")
  
  axis(1, at= x[["bfd"]]$digits)
}

plotting.abs.diff <- function (x, ...) {
  plot(x[["bfd"]]$digits, x[["bfd"]]$absolute.diff, pch="x", col="blue", xlab="Digits",
       ylab="Absolute Difference", 
       main= "Absolute Difference",xaxt="n")
  
  axis(1, at= x[["bfd"]]$digits)
}

plotting.ex.summation <- function (x, ...) {
  plot(x[["bfd"]]$digits, x[["bfd"]]$abs.excess.summation, pch="x", col="blue", 
       xlab="Digits",
       ylab="Absolute Excess Summation", 
       main= "Summation Difference",xaxt="n")
  axis(1, at= x[["bfd"]]$digits)
}

plotting.legend <- function (x) {
  plot(1, type = "n", axes=FALSE, xlab="", ylab="", main= paste("Legend \n Dataset:", x[["info"]]$data.name))
  plot_colors <- c("lightblue","blue","red")
  legend(x = "top",inset = 0,
         legend = c("data", "data", "benford"), 
         col=plot_colors, lwd=2, lty=c(1,1,2))
}