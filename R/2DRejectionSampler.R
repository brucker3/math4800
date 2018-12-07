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

jointpdf <- function(x,y){
  joint <- dunif(x) * dunif(y)
  return(joint)

}





