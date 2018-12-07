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
#' @examples
#' rejectionSampler(n = 10000, jpdf = mypdf, a = 0, b = 1, C = 1))
#' rejectionSampler(n = 10000, jpdf = betapdf, a = 0, b = 1, C = 1))
#' s



rejectionSampler <- function(n , pdf, lower, upper, C ){
  holder <- c()
  veccheck <- integrate(mypdf,lower,upper) # check for valid pdf
  if(!(veccheck[1] == 1)) {
    stop("invalid pdf")
  }
  if( !(n >= 1) ) {
    stop(" n must be an interger greater than 0 ")
  }
  if( !(lower < upper) ) {
    stop(" lower must be an interger less than uppper ")
  }
  while(length(holder) < n) {
    sim <- c()
    sim <- replicate(n, {
      x <- runif(1, lower, upper) # proposed
      check <- runif(1,0,C) # commpaer to pdf
      pdfcheck <- pdf(x) # run proposed point wiht pdf
      if( pdfcheck <= check) {
              x
      }else {
        NA
      }
    })
    holder <- c(holder,sim[!is.na(sim)])
  }
  if(!(length(holder) == n)) {
    holder <- holder[1:n]
  }
  return(holder)
}


#' math4800 project
#'
#' @description This function is a simple pdf returns a number
#'
#' @param x number that should be between 0 and 2 or else the pdf is 0
#' @export
mypdf <- function(x){
  if( x <= 2  && x >= 0) {
    x/2
  }else{
      0
  }
}


#' math4800 project
#'
#' @description This function is a simple  beta pdf returns a number
#'
#' @param x number that should be between 0 and 1 or else the pdf is 0
#' @parm shape is the shape input value for beta
#' @export
betapdf <- function(x, shape){
  if(0 <= x && x <= 1 ) {
    dbeta(x = x, shape = shape)
  }else{
    0
  }
}




