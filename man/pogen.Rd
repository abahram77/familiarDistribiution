\name{pogen}
\alias{pogen}
\title{Poisson Distribution}
\usage{
  pogen(lambda,t)
}
\description{
If an exponentially distributed variable is modeled as the waiting time before an arrival, the Poisson
distributed variable can be modeled as the number of arrivals during a period of time of length t .
Using "expgen","pogen" is a function, taking a float number as the parameter of the underlying exponential distribution and a
float as the length of time interval and generates a random
number from Poisson distribution.(the generated Poisson variable will be of parameter lambda*t.)

related functions :
	rpois
}
\examples{
pogen(10,20)
}
