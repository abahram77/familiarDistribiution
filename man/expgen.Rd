\name{expgen}
\alias{expgen}
\title{Exponential Distribution}
\usage{
  expgen(lambda)
}
\description{
Exponential distribution is a popular distribution which is used to model waiting times and memoryless
processes. An exponential distribution with parameter lambda can be calculated as (-1/lambda)(log(X)) Where X
is a uniformly distributed random variable in [0, 1].
Using "cugen" ,"expgen" is a function which takes the parameter lambda and generates an
exponentially distributed random variable.

related functions :
	rexp

}
\examples{
expgen(10)
}
