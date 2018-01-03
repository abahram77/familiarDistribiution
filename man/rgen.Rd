\name{rgen}
\alias{rgen}
\title{Random Generator}
\usage{
  rgen(num)
}
\description{
this function generates #num random numbers using the Linear congruential generator through a vector with size num.
the seed for this generator is set automatically with each time the function is called.
this generator is a fast and simple method that is used in many professional programs like C,C++, java, excel and etc.
the constants which are used in this function are the same as the ones used in GCC.
}
\examples{
rgen(2)
}
