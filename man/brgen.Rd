\name{brgen}
\alias{brgen}
\title{Bernoulli Numbers}
\usage{
  brgen(p,num)
}
\description{
Bernoulli distribution is one the most simple as well as famous distributions. In each Bernoulli trial
there are two possible outcomes, namely success or failure, for the probability of success equal to p,
the probability of failure is 1-p.
Using "cugen","brgen" is a function , taking a float number as the parameter of Bernoulli
distribution and generates a random number from {0,1}.

related functions :
	rbinom 
	**note that we can make a bernoulli random number by using a binom random generator which has just one trial. 


}
\examples{
brgen(0.27,10)
}