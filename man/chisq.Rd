\name{chisq}
\alias{chisq}
\title{Gets the Chi-squared test of a Benford object}
\usage{
chisq(bfd)
}
\arguments{
  \item{bfd}{an object of class "Benford". See
  \code{\link{benford}}.}
}
\value{
A list with class "htest" containing the results of the
Chi-squared test.
}
\description{
It gets the Chi-squared test for a Benford object. See the
section value of \code{\link{benford}}.
}
\examples{
data(census.2009) #gets data
c2009 <- benford(census.2009$pop.2009) #generates benford object
chisq(c2009) # equivalent to c2009$stats$chisq
}

