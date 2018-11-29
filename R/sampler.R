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


rejectionSampler <- function(n , pdf, lower, upper, C ){
  holder <- c()
  veccheck <- integrate(mypdf,lower,upper) # check for valid pdf
  if(!(v[1] == 1)) {
    stop("invalid pdf")
  }
  if( !(n >= 1) ) {
    stop(" n must be an interger greater than 0 ")
  }
  if( !(lower >= 1) || !(lower < upper) ) {
    stop(" lower must be an interger greater than 0 and less than uppper ")
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






mypdf <- function(x){  # should accept any real number and chekc that its a valid pdf
                2*x    # this is integrate to 1 and be nonnegative 2 criteria
  }
mypdf
v <- integrate(mypdf,0) # check for valid pdf
v[1] ==1
tester <- rejectionSampler(1, mypdf , 0, 1/2, 1)
tester
length(tester)
head(tester)
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




