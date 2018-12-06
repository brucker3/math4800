#' math4800 project
#' this file implemnts a funciton to sample from a joint  random variable that is defifned on a square
#'
#' @description This function returns a data frome with n elemtns from a joint continus random variable
#'
#' @param n number of observations. n must be an integer >0
#' @param pdf a function that is the join  pdf of the distribution.
#' @param lower lower and upper limits of the  of the distribution. Must be finite such that \eqn{P(a \le X \le b \cap a \le Y \le b) = 1}.
#' @param lower lower and upper limits of the distribution
#' @param C a numeric such that \eqn{f(x,y) \le C} for all values of x.
#'
#' @return data.frame of random deviate pairs
#' @export


jointRejectionSampler <- function(n, jpdf, a, b, C) {
  return(print('test'))
  samples <- data.frame(x = numeric(0),  y = numeric(0))

  reject <- TRUE
  while(reject) {
    proposed <- runif(n = 2, min = a, max = b)
    proposed.density <- do.call(jpdf, list(x = proposed[1], y = proposed[2]))
    sampler.checker <- runif(n = 1, min = 0, max = C)

  }
  samples <- c(samples,proposed[!is.na(sim)])

}


mypdf <- function(x){  # should accept any real number and chekc that its a valid pdf
  2*x    # this is integrate to 1 and be nonnegative 2 criteria
}
mypdf
v <- integrate(mypdf,0,1) # check for valid pdf
v[1] ==1
jointtester <- jointRejectionSampler(10, mypdf , 0, 1, 1)













