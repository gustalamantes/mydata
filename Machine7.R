n_q <- 44
award <- 1
penalty <- -0.25

p <- 1/5
p

e_points <- (award*p) + (penalty * (1-p))
e_points

m <- n_q * e_points
m

se <- sqrt(n_q) * abs(penalty-award) * sqrt(p*(1-p))
se

1-pnorm (8, m, se)

set.seed(21, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  Results <- sample(c(award, penalty), n_q, replace = TRUE, prob = c(p, 1-p))
  sum (Results)
})
mean (S > 8)

p <- 1/4
p


e_points <- (award*p) + (penalty * (1-p))
e_points

m <- n_q * e_points
m[1]


p <- seq(0.25, 0.95, 0.05)

score <- sapply(p, function(v){
  e_points <- (award*v) + (penalty * (1-v))
  m <- n_q * e_points
  se <- sqrt(n_q) * abs(penalty-award) * sqrt(v*(1-v))
  1-pnorm (35, m, se)
})

min(p[which(score > 0.8)])



set.seed(21, sample.kind = "Rounding")
p <- seq(0.25, 0.95, 0.05)

score <- sapply(p, function(v){
  e_points <- (award*v) + (penalty * (1-v))
  m <- n_q * e_points
  se <- sqrt(n_q) * abs(penalty-award) * sqrt(v*(1-v))
  1-pnorm (35, m, se)
})

min(p[which(score > 0.8)])

p <- 5/38
a <- 6
b <- -1
n <- 500
a

mu <- (a*p) + b*(1-p)
mu

sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

mu

sigma / sqrt(n)

mu * n

sigma * sqrt(n)

pnorm (0, mu, sigma)
pnorm(0, mu * n, sigma * sqrt(n))



library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)


