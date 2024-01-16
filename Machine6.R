library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a){
    mean(x <= a)
  } 
F(70)


act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores <= 10)


x <- seq(1, 36)
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)


zscores <- (act_scores - mean(act_scores)) / sd(act_scores)
mean(zscores > 2)

2*sd(act_scores) + mean(act_scores)
x <- qnorm(.975, mean(act_scores), sd(act_scores))
x

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})

min(which(cdf >= .95))


qnorm(.95, 20.9, 5.7)

pnorm(26, mean(act_scores), sd(act_scores))

library(tidyverse)
library(ggplot2)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
