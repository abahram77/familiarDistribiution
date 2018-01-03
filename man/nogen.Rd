\name{nogen}
\alias{nogen}
\title{Normal Distribution}
\usage{
  nogen(u,s)
}
\description{
The Poisson (lambda) distribution can be considered as an approximation of N(lambda,lambda).
Using "pogen", "nogen" is a function, taking two parameters, one float "u" as the mean
and one float "s" as the variance of the distribution, and generates a random normal number with
mean "u" and variance "s". (First, generate a Poisson number and then use scale and transition to
achieve the desired mean and variance.)
note that the default value of tlength and lambda is 10.

related functions :
	rnorm
}
\examples{
nogen(20,20)
}