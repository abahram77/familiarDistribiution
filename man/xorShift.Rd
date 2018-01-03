\name{xorShift}
\alias{xorShift}
\title{Random Renerator}
\usage{
  xorShift(num)
}
\description{

this function generates #num random numbers using the xorshift method through a vector with size num.
the seed for this generator is set automatically with each time the function is called.
this method is a fast, reliable and easy to implement method and a simple implementation of it can pass many PRNG tests like BigCrush suite
}
\examples{
xorShift(2)
}
