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
#############################################################################################################################
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
#############################################################################################################################
\name{dugen}
\alias{dugen}
\title{Uniform Numbers}
\usage{
  dugen(st,en,num)
}
\description{
Using "rgenerator","dugen" is a function, taking the two integer numbers as input and
generates random numbers uniformly distributed between two integer inputs.
}
\examples{
dugen(10,20,100)
}
#############################################################################################################################
\name{cugen}
\alias{cugen}
\title{Uniform Numbers}
\usage{
  cugen(num)
}
\description{
Using "dugen", write a function "cugen", which generates a random uniform number between
0 and 1.
}
\examples{
dugen(10)
}
#############################################################################################################################
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
Using "cugen", write a function "brgen", taking a float number as the parameter of Bernoulli
distribution and generates a random number from {0,1}.��� p.
}
\examples{
brgen(0.27,10)
}
#############################################################################################################################
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
}
\examples{
brgen(0.27,10)
}
#############################################################################################################################
\name{gegen}
\alias{gegen}
\title{Geometric Distribution}
\usage{
  gengen(p)
}
\description{
The number of failures in Bernoulli trials, between two wins, follows the geometric distributions.
Using "brgen","gegen" is a function, taking a parameters and generates a geometric random variable.
}
\examples{
brgen(0.27)
}
#############################################################################################################################
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
}
\examples{
expgen(10)
}
#############################################################################################################################
\name{gagen}
\alias{gagen}
\title{Gamma distribution}
\usage{
  gagen(lambda,k)
}
\description{
Summation of k i.i.d exponential random variables leads to a gamma distributed random variable.
Using "expgen","gagen" is a function, taking a float number as the parameter of the underlying exponential distribution and an integer as k and generates a random number from gamma distribution.
}
\examples{
gagen(10,3)
}
#############################################################################################################################
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
}
\examples{
pogen(10,20)
}
#############################################################################################################################
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
}
\examples{
nogen(20,20)
}
#############################################################################################################################
\name{visualizeCu}
\alias{visualizeCu}
\title{Uniform Numbers Visualization}
\usage{
  visualizeCu()
}
\description{
this function generates 10000 cugen random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizeCu()
}
#############################################################################################################################
\name{visualizeDu}
\alias{visualizeDu}
\title{Uniform Numbers Visualization}
\usage{
  visualizeDu()
}
\description{
this function generates 10000 dugen random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizeDu()
}
#############################################################################################################################
\name{visualizebr}
\alias{visualizebr}
\title{Bernoulli Numbers Visualization}
\usage{
  visualizeBr(p)
}
\description{
this function generates 10000 Bernoulli random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizebr(0.27)
}
#############################################################################################################################
\name{visualizebi}
\alias{visualizebi}
\title{Binomial Numbers Visualization}
\usage{
  visualizebi(p,num)
}
\description{
this function generates 1000 Binomial random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizebi(0.27,10)
}
#############################################################################################################################
\name{visualizegeo}
\alias{visualizegeo}
\title{Geometric Numbers Visualization}
\usage{
  visualizegeo(p)
}
\description{
this function generates 1000 Geometric random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizegeo(0.27)
}
#############################################################################################################################
\name{visualizeEXP}
\alias{visualizeEXP}
\title{Exponential Numbers Visualization}
\usage{
  visualizeEXP(lambda)
}
\description{
this function generates 1000 Exponential random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizeEXP(10)
}
#############################################################################################################################
\name{visualizeGAMMA}
\alias{visualizeGAMMA}
\title{Gamma Numbers Visualization}
\usage{
  visualizeGAMMA(lambda,k)
}
\description{
this function generates 1000 Gamma random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizeGAMMA(10,3)
}
#############################################################################################################################
\name{visualizePOISSON}
\alias{visualizePOISSON}
\title{Poisson Numbers Visualization}
\usage{
  visualizePOISSON(lambda,t)
}
\description{
this function generates 1000 Poisson random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizePOISSON(10,20)
}
#############################################################################################################################
\name{visualizeNORMAL}
\alias{visualizeNORMAL}
\title{Normal Numbers Visualization}
\usage{
  visualizeNORMAL(u,s)
}
\description{
this function generates 1000 Normal random numbers and then shows the plot for the generated random numbers using ggplot.
}
\examples{
visualizeNORMAL(20,20)
}
