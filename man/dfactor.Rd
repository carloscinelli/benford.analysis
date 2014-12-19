\name{dfactor}
\alias{dfactor}
\title{Gets the Distortion Factor of a Benford object}
\usage{
dfactor(bfd)
}
\arguments{
  \item{bfd}{an object of class "Benford". See
  \code{\link{benford}}.}
}
\value{
The distortion factor.
}
\description{
It gets the Distortion Factor of a Benford object. See the
section value of \code{\link{benford}}.
}
\examples{
data(corporate.payment) #gets data
cp <- benford(corporate.payment$Amount) #generates benford object
dfactor(cp) # equivalent to cp$distortion.factor
}

