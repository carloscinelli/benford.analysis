#' @title Shows the first digits ordered by the mains discrepancies from Benford's Law
#' @description It creates a data frame with the first digits and the differences from Benford's
#' Law in decreasing order.
#' @usage
#' 
#' suspectsTable(bfd, by="absolute.diff")
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @param by a character string selecting how to order the digits.It can be
#' 'abs.excess.summation','difference','squared.diff' or'absolute.diff'.
#' @return A data frame with 2 variables: digits and the group chosen in \code{by}.
#' @examples
#' data(corporate.payment) #gets data 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' suspectsTable(cp) 
#' @export
suspectsTable<- function(bfd, by="absolute.diff"){
  if(class(bfd)!="Benford") stop("bfd must be a 'Benford' object.")
  if(!by %in% c("abs.excess.summation","difference","squared.diff","absolute.diff")){
    stop("By must be one of these: 'abs.excess.summation','difference','squared.diff','absolute.diff'")
  }
  digits <- bfd[["bfd"]][order(get(by), decreasing=TRUE)][,list(digits, get(by))]
  setnames(digits, c("digits", "V2"), c("digits", by))
  digits
}

#' @title Gets the 'suspicious' observations according to Benford's Law
#' @description It gets the original data from the 'suspicious' digits groups according
#' to benford analysis.
#' @usage
#' 
#' getSuspects(bfd, data, by="absolute.diff", how.many=2)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @param data the original data used for the benford analysis.
#' @param how.many how many groups of digits to get.
#' @param by a character string selecting how to order the digits.It can be
#' 'abs.excess.summation','difference','squared.diff' or'absolute.diff'.
#' @return The 'suspicious' observations from the original data.
#' @examples 
#' data(lakes.perimeter) #gets data
#' lk <- benford(lakes.perimeter[,1]) #generates benford object
#' suspects <- getSuspects(lk, lakes.perimeter)
#' @export
getSuspects <-function(bfd, data, by="absolute.diff", how.many=2){
  if(class(bfd)!="Benford") stop("bfd must be a 'Benford' object.")
  if(!is.data.frame(data)) stop("Data must be a data frame.")
  if(!by %in% c("abs.excess.summation","difference","squared.diff","absolute.diff")){
    stop("By must be one of these: 'abs.excess.summation','difference','squared.diff','absolute.diff'")
  }
  digits <- bfd[["bfd"]][order(get(by), decreasing=TRUE)][,list(digits)][1:how.many]
  suspects.lines <- bfd[["data"]]$lines.used[bfd[["data"]]$data.digits %in% digits$digits]
  data.table(data[suspects.lines, ,drop = FALSE])
}

#' @title Shows the duplicates of the data
#' @description It creates a data frame with the duplicates in decreasing order.
#' @usage
#' 
#' duplicatesTable(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A data frame with 2 variables: number and duplicates.
#' @examples
#' data(census.2009) #gets data
#' c2009 <- benford(census.2009$pop.2009) #generates benford object
#' duplicatesTable(c2009)
#' @import data.table
#' @export 
duplicatesTable <- function(bfd){
  data.used <- NULL
  v <- NULL
  V1 <- NULL
  if(class(bfd)!="Benford") stop("bfd must be a 'Benford' object.")
  numbers <- copy(bfd[["data"]])
  numbers <- numbers[,v:= data.used]
  duplicates.count <- numbers[,length(data.used), by=v][order(V1, decreasing=TRUE)]
  setnames(duplicates.count,c("v", "V1"),c("number", "duplicates"))
  duplicates.count
}

#' @title Gets the duplicates from data
#' @description It gets the duplicates from the original data.
#' @usage
#' 
#' getDuplicates(bfd, data, how.many=2)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @param data the original data used for the benford analysis.
#' @param how.many how many groups of duplicates to get.
#' @return The duplicates from the original data.
#' @examples
#' data(census.2000_2010) #gets data
#' c2010 <- benford(census.2000_2010$pop.2010) #generates benford object
#' duplicates <- getDuplicates(c2010, census.2000_2010)
#' @export
getDuplicates <-function(bfd, data, how.many=2){
  v<- NULL
  data.used <- NULL
  V1 <- NULL
  if(class(bfd)!="Benford") stop("bfd must be a 'Benford' object.")
  if(!is.data.frame(data)) stop("Data must be a data frame.")
  numbers <- copy(bfd[["data"]])
  numbers <- numbers[,v:= data.used]
  duplicates.count <- numbers[,length(data.used), by=v][order(V1, decreasing=TRUE)][,list(v)][1:how.many]
  duplicates.lines <- bfd[["data"]]$lines.used[bfd[["data"]]$data.used %in% duplicates.count$v]
  data.table(data[duplicates.lines, ,drop = FALSE])
}

#' @title Gets the data starting with some specific digits
#' @description It subsets the original data according to the leading digits.
#' @usage
#' 
#' getDigits(bfd, data, digits)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @param data the original data of the analysis.
#' @param digits the first digits to get.
#' @return The the original data starting only with the leading digits.
#' @examples
#' data(census.2000_2010) #gets data
#' 
#' #generates benford object
#' c2010 <- benford(census.2000_2010$pop.2010) 
#' 
#' #subsets data starting with digits 10 and 25
#' digits.10.25 <- getDigits(c2010, census.2000_2010, c(10,25)) 
#' @export
getDigits <- function(bfd, data, digits){
  data <- data.table(data[bfd[["data"]]$lines.used[bfd[["data"]]$data.digits %in% digits], ,drop=FALSE])
  return(data)
}

#' @title Gets the MAD of a Benford object
#' @description It gets the Mean Absolute Deviation (MAD) of a Benford object.
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' MAD(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return The MAD.
#' @examples
#' data(census.2000_2010) #gets data
#' c2010 <- benford(census.2000_2010$pop.2010) #generates benford object
#' MAD(c2010) #equivalent to c2010$MAD
#' @export
MAD <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$MAD
}


#' @title Gets the Chi-squared test of a Benford object
#' @description It gets the Chi-squared test for a Benford object.
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' chisq(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A list with class "htest" containing the results of the Chi-squared test.
#' @examples
#' data(census.2009) #gets data
#' c2009 <- benford(census.2009$pop.2009) #generates benford object
#' chisq(c2009) # equivalent to c2009$stats$chisq
#' @export 
chisq <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$stats$chisq
}

#' @title Gets the Mantissa Arc test of a Benford object
#' @description It gets the Mantissa Arc Test of a Benford object.
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' marc(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A list with class "htest" containing the results of the Matissa Arc test.
#' @examples
#' data(corporate.payment) #gets data 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' marc(cp) # equivalent to cp$stats$mantissa.arc.test
#' @export
marc <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$stats$mantissa.arc.test
}

#' @title Gets the Distortion Factor of a Benford object
#' @description It gets the Distortion Factor of a Benford object.
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' dfactor(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return The distortion factor.
#' @examples
#' data(corporate.payment) #gets data 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' dfactor(cp) # equivalent to cp$distortion.factor
#' @export
dfactor <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$distortion.factor
}


#' @title Gets the main stats of the Mantissa of a Benford object
#' @description It gets the Mean, Variance, Excess Kurtosis and Skewness of the Mantissa.
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' mantissa(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A data.frame with the main stats of the Mantissa.
#' @examples
#' data(corporate.payment) #gets data 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' mantissa(cp) # equivalent to cp$mantissa
#' @export
mantissa <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$mantissa
}

#' @title Gets the data used of a Benford object
#' @description It gets the lines, values, mantissa and first digits of the data used of a Benford object .
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' getData(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A data.frame with the lines, values, mantissa and first digits of the data.
#' @examples
#' data(corporate.payment) 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' getData(cp) # equivalent to cp$data
#' @export
getData <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$data
}

#' @title Gets the the statistics of the first Digits of a benford object
#' @description It gets the statistics of the first digits (Frequencies, Squared Differences, Absolute Differences etc).
#' See the section value of \code{\link{benford}}.
#' @usage
#' 
#' getBfd(bfd)
#' @param bfd an object of class "Benford". See \code{\link{benford}}.
#' @return A data.frame with first digits and their statistics.
#' @examples
#' data(corporate.payment) 
#' cp <- benford(corporate.payment$Amount) #generates benford object
#' getBfd(cp) # equivalent to cp$bfd
#' @export
getBfd <- function(bfd){
  if (class(bfd)!="Benford") stop("Object must be of class Benford")
  bfd$bfd
}
