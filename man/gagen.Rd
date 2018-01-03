\name{gagen}
\alias{gagen}
\title{Gamma distribution}
\usage{
  gagen(lambda,k)
}
\description{
Summation of k i.i.d exponential random variables leads to a gamma distributed random variable.
Using "expgen","gagen" is a function, taking a float number as the parameter of the underlying exponential distribution and an integer as k and generates a random number from gamma distribution.

related functions :
	rgamma

}
\examples{
gagen(10,3)
}
