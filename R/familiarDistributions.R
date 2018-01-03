recent <- 29
fdummyXor <- function(a , b){
  output <- 0
  if((a >= 2 ^ 31) && (b >= 2 ^ 31)){
    a <- (a - 2 ^ 31)
    b <- (b - 2 ^ 31)
    output <- bitwXor(a , b)
  }else if((a < (2 ^ 31)) && (b < (2 ^ 31))){
    output <- bitwXor(a,b)
  }else if(a >= 2 ^ 31){
    a <- (a - 2 ^ 31)
    output <- bitwXor(a,b)
    output <- output + 2 ^ 31
  }else{
    b <- (b - 2 ^ 31)
    output <- bitwXor(a,b)
    output <- output + 2 ^ 31
  }
  return(output)
}

xorShift <- function(num)
{
  seed <- recent
  rands <- c()
  for(i in 1:(num + 10))
  {
    x <- seed

    temp <- bitwShiftL(x,13)
    x <- dummyXor(x, temp)

    temp <- bitwShiftR(x,17)
    x <- dummyXor(x, temp)

    temp <- bitwShiftL(x,5)
    x <- dummyXor(x, temp)
    x <- abs(x)
    rands <- c(rands, x)
    seed <- x

  }
  for(i in 11:(num+ 10))
  {
    rands[i] <- (rands[i] / (2^31))
    rands[i] <- abs(rands[i])
  }


  seed <- (seed / 100000)
  seed <- ceiling(seed)
  seed <- (seed + as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 50))
  assign("recent", seed, envir = .GlobalEnv)
  return(rands[11:(num+10)])
}

recent <- 29


rgen <- function(num)
{
  seed <- recent
  m <- ((2**31) - 1)
  a <- 1103515245
  c <- 12345
  rand <- c(seed)
  for(i in 1:(num+1))
  {
    temp <- a * seed
    temp <- temp + c
    temp <- temp %% m
    rand <- c(rand, temp)
    seed <- temp
  }
  for(i in 2:(num+1))
  {
    rand[i] <- (rand[i] / (2**31))
  }
  seed <- (seed + as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 50))
  assign("recent", seed, envir = .GlobalEnv)
  return(rand[2:(num+1)])
}





dugen <- function(st, en, num)
{
  rands <- rgen(num)
  for(i in 1:num)
  {
    rands[i] <-  ((rands[i]* (en - st)) + st)
  }
  return(rands)
}


cugen <- function(num)
{
  rands <- rgen(num)
  return(rands)
}

brgen <- function(p, num)
{

  rands <- cugen(num)
  for(i in 1:num)
  {
    if(rands[i] > p)
    {
      rands[i] <- 0
    }
    else
    {
      rands[i] <- 1
    }
  }

  return(rands)

}

bigen <- function(p, num)
{

  rands <- brgen(p, num)

  result <- 0
  for(i in 1:num)
  {
    if(rands[i] == 1)
    {
      result <- result + 1
    }
  }
  return(result)
}




gegen <- function(p)
{
  count <- 0
  while(TRUE)
  {
    tr <- brgen(p,1)
    if(tr == 0)
    {
      count <- (count + 1)
    }
    else
    {
      break()
    }
  }

  return(count)
}

library(ggplot2)



expgen <- function(lambda){
  x<-(cugen(1)[1])
  result<-((-1/lambda)*log(x))
  return(result)
}
gagen <- function(lambda, k){
  result <- 0
  for (i in 1:k){
    result <- result + expgen(lambda)
  }
  return(result)
}
pogen <- function(lambda, t){
  times <- vector()
  counter <- 0
  while (sum(times) < t){
    times <- c(times, expgen(lambda))
    counter <- counter + 1
  }
  return(counter)
}

nogen<- function(u,s){
  lambda <- 10
  tLength <- 10
  zResult=(pogen(lambda,tLength)-(lambda*tLength))/sqrt(lambda)
  result=zResult*(sqrt(s)) + u
  return(resultlt)
}

visualizeCu <- function(brick, color){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, cugen(1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp , me , sep = "\n")

  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))



}
visualizeCuPlot <- function(){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, cugen(1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me , sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )



}

visualizeDu <- function(st,en , brick , color){

  test <- vector()
  for (i in 1:10000){
    test <- c(test, dugen(st,en,1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))

}

visualizeDuPlot <- function(st , en){

  test <- vector()
  for (i in 1:10000){
    test <- c(test, dugen(st,en,1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}

visualizebr <- function(p , brick , color){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, brgen(p,1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))

}

visualizebrPlot <- function(p){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, brgen(p,1))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )

}

visualizebi <- function(p,num , brick , color){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, bigen(p,num))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))
}

visualizebiPlot <- function(p,num){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, bigen(p,num))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}

visualizegeo <- function(p , brick , color){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, gegen3(p))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))

}

visualizegeoPlot <- function(p){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, gegen3(p))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}

visualizeEXP <- function(lambda , brick , color){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, expgen(lambda))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))


}

visualizeEXPPlot <- function(lambda){

  test <- vector()
  for (i in 1:1000){
    test <- c(test, expgen(lambda))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )

}

visualizeGAMMA <- function(lambda, k , brick , color){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, gagen(lambda, k))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))

}

visualizeGAMMAPlot <- function(lambda, k){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, gagen(lambda, k))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}

visualizePOISSON <- function(lambda, t , brick , color){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, pogen(lambda, t))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))

}

visualizePOISSONPlot <- function(lambda, t){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, pogen(lambda, t))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}


visualizeNORMAL <- function(u, s , brick , color){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, nogen(u, s))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median : ", med)
  xName <- paste(dev, exp, me ,sep = "\n")
  return(hist(test, brick, xlab = "random numbers", ylab = "count", main =xName, col = color,border = "white"))
}

visualizeNORMALPlot <- function(u, s){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, nogen(u, s))
  }
  sd <- sd(test)
  expected <- mean(test)
  med <- median(test)
  dev <- paste("standard deviation : ", sd)
  exp <-  paste("expected value : ", expected)
  me <- paste("median :  " , med)
  xName <- paste(dev, exp, med ,  sep = "\n")
  test <- data.frame(test)
  return(ggplot(test, aes(x = test)) + geom_density() + geom_vline(xintercept = expected, colour="green", linetype= "dashed", size=1) + geom_vline(xintercept = sd, colour="red", linetype= "dashed", size=1)
         + geom_vline(xintercept = med, colour="dodgerblue1", linetype= "dashed", size=1) )
}
