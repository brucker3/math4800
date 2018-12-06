# homework 8

# hw problem 1
install.packages("markovchain")
library(markovchain)
states <- as.character(c(0, 1, 2, 3 ))
byrow <- TRUE
transition_matrix <- matrix(c(.5, .5, 0, 0,
                              .5, 0, .5, 0,
                               0, .5, 0, .5,
                               0, 0, .5, .5
), byrow = TRUE, ncol = 4, dimnames = list(states, states))
mcBinomial <- new("markovchain", states = states, byrow = TRUE,
                  transitionMatrix = transition_matrix, name = "binomial")
mcBinomial

current_state <- "2"
dat <- current_state
my_pmf <- function(x, n, p) {
  dbinom(as.integer(x), n, p)
}
for(i in 1:10000) {
  proposed_state <- rmarkovchain(n = 1,
                                 object = mcBinomial,
                                 t0 = current_state)
  sj <- my_pmf(proposed_state, 3, 1/3)
  pij <- transition_matrix[current_state, proposed_state]
  if(pij == 0) break;
  si <- my_pmf(current_state, 3, 1/3)
  pji <- transition_matrix[proposed_state, current_state]
  accept <- runif(1, 0, 1) <= min(sj*pji/(si *pij), 1)
  current_state <- ifelse(accept, proposed_state, current_state)
  dat <- c(dat, current_state)
}
table(dat)/10000
round(dbinom(0:3, 3, 1/3), 4)


# hw problem 2
pdf <- function(x) {
  if(x <= 2 && x >= 0 ){
    x/2
  }else
    0
}


t0 <- runif(1,0,1)
dat <- t0
for(i in 1:20000) {
  proposed <- rnorm(1)
  if(runif(1,0,1) < pdf(proposed) * dnorm(t0) /pdf(t0)/dnorm(proposed)) {
    t0 <- proposed
  }else
  dat <- c(dat, t0)
}
hist(dat, probability = TRUE); curve(x/2, add = TRUE)



# hw problem 3
library(dplyr)
set.seed(1)
teams <- data.frame(player = 1:100,
                    team = sample(c(1:4),25,100),
                    ratting = sample(1:100,100))


head(teams)


score <- function(teams) {
  teamstat <- teams %>% group_by(team) %>%
    summarise(mean = mean(ratting), sd = sd(ratting))
  difference <- abs(teamstat$mean[1] -teamstat$sd[1])
  difference <- c(difference, abs(teamstat$mean[2] -teamstat$sd[2]))
  difference <- c(difference, abs(teamstat$mean[3] -teamstat$sd[3]))
  difference <- c(difference, abs(teamstat$mean[4] -teamstat$sd[4]))
  sum(abs(diff(difference)))
}
score(teams)






