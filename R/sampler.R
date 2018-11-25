# math4800 project
# this file implemnts a funciton to sample from a random variable that is passed in



sampler <- function(n , pdf, lower, upper, C ){
  holder <- c()
  holder <- replicate(n, {
      x <- runif(1, 0, 1)
      y <- runif(1, 0, upper)
      z <- runif(1, 0, lower)
      if (y > x ) {
        holder <- c(holder,"na")
      }else {
        holder <- c(holder,x)
        }
    })
  return(holder)

}


tester <- sampler(4, rnorm(0,1) , 0, 1/2, 1)
tester
tester[1]
tester[2]
tester[3]
tester[4]








