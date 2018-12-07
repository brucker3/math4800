#' math4800 project
#' this file implemnts a funciton to sample from a joint  random variable that is defifned on a square
#'
#' @description This function returns a data frome with n elemtns from a joint continus random variable
#'
#' @param n number of observations. n must be an integer >0
#' @param jointpdf a function that is the join  pdf of the distribution.
#' @param lower lower and upper limits of the  of the distribution. Must be finite such that \eqn{P(a \le X \le b \cap a \le Y \le b) = 1}.
#' @param upper upper and upper limits of the distribution
#' @param C a numeric such that \eqn{f(x,y) \le C} for all values of x.
#'
#' @return data.frame of random deviate pairs
#' @export
#'
#' @examples
#' jointRejectionSampler(n = 10000, jpdf = jointunif, a = 0, b = 1, C = 1))
#' jointRejectionSampler(n = 10000, jpdf = jointnorm, a = 0, b = 1, C = 1))
#'


jointRejectionSampler <- function(n, jointpdf, lower, upper, C) {
  samples <- data.frame(x = numeric(0),  y = numeric(0))
  while( !(n == nrow(samples)) ) {
    reject <- TRUE
    while(reject) {
      proposed <- runif(n = 2, min = lower, max = upper)
      proposed.density <- do.call(jointpdf, list(x = proposed[1], y = proposed[2]))
      sampler.checker <- runif(n = 1, min = 0, max = C)
      if ( sampler.checker > proposed.density) {
        reject <- FALSE
      }
    }
    samples <- rbind(samples, data.frame(x = proposed[1], y = proposed[2]))
  }
  return(samples)
}

#' math4800 project
#' this file implemnts a funciton to sample from a joint  random variable that is defifned on a square
#'
#' @description This function returns a value from a nuiform joint continus random variable
#'
#' @param x input for the first marginal uniform distribution
#' @param y input for the first marginal uniform distribution
#'
#' @return join uniform pdf
#' @export

jointunif <- function(x,y){
  joint <- dunif(x) * dunif(y)
  return(joint)

}

#' math4800 project
#' this file implemnts a funciton to sample from a joint  random variable that is defifned on a square
#'
#'
#' @description This function returns a value from a normal joint continus random variable note both x and y
#' must be the same value to be a valid for this 2D sampling function sense we are sampling from a square
#'
#' @param x input for the first marginal normal distribution
#' @param y input for the first marginal normal distribution
#'
#' @return join normal pdf
#' @export

jointnorm <- function(x,y){
  if( x == y) {
    if(x >=0 && x <= 1 && y >= 0 && y <= 1) {
      joint <- dnorm(x) * dnorm(y)
    }else{
      0
    }
  }else{
   0
  }
}



