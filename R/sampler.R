#' math4800 project
#' this file implemnts a funciton to sample from a random variable that is passed in
#'
#' @description This function returns a vector with n elemtns from a continus random variable
#'
#' @param n number of observations. n must be an integer >0
#' @param pdf a function that is the  pdf of the distribution.
#' @param lower lower and upper limits of the  of the distribution. Must be finite such that \eqn{P(a \le X \le b \cap a \le Y \le b) = 1}.
#' @param lower lower and upper limits of the distribution
#' @param C a numeric such that \eqn{f(x,y) \le C} for all values of x.
#'
#' @return data.frame of random deviate pairs
#' @export


sampler <- function(n , pdf, lower, upper, C ){
  holder <- c()
  while(length(holder) < n) {
    holder <- replicate(2*n, {
      x <- runif(1, 0, 1)
      y <- runif(1, 0, upper)
      z <- runif(1, 0, lower)
      if (y > x ) {
        NA
      }else {
        x
      }
    })
    holder <- holder[!is.na(holder)]
  }

  print(length(holder))
  print(holder)
  if(!(length(holder) == n)) {
    print("test 1")
    holder <- holder[1:n]
  }
  return(holder)

}








tester <- sampler(6, rnorm(0,1) , 0, 1/2, 1)
tester
tester3 <- tester[!is.na(tester)] # should be removing na
tester3
tester[1]
tester[2]
tester[3]
tester[4]
# functino documnetation now wokrking changes made to be able to save
# need to rerun roxygen 2 ever time you change documnation
# also devtool::document()


vec <- c(1,2,3,4,5)
vec
n <- 3
vec[1:n]
vec




