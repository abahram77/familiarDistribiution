\name{bigen}
\alias{bigen}
\title{Binomial Distribution}
\usage{
  bigen(p,num)
}
\description{
A binomial random variable can be seen as the result of repeated Bernoulli Trials.
Using "brgen", "bigen" is a function, taking two parameters, one float as the probability
parameter and one integer "n" as the number of trials, and generates a random number from 0 to n.

related functions :
	rbinom


}
\examples{
bigen(0.27,10)
}
