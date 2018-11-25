# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# first chnages in setting up repo



HW7problem2 <- function(x) {
  if( x > 1  || x < 0 ) {
    print("invalide input x must be between 0 and 1 ")
  }else {
    print("valid input")
  }
  y <- (1/2)*((-2*x +2*(x^2-x)^(1/2)+1 )^(1/3) +(1/(-2*x +2*(x^2-x)^(1/2)+1 )^(1/3)) + 1)
  print("out put is ")
  print(y)

}


pexp(1/2, rate = 1)



?uniroot()

HW7problem2(.4)
c <- c(0:1)
f <- function(x) -6((x^3)/2 - (x^2)/2)
uniroot(f, c,lower = 0 , upper =1 , f.lower= 0, f.upper= 1)
min(c)


t <- function (x, a) x - a
str(xmin <- uniroot(f, c(0, 1), tol = 0.0001, a = 1/3))








