#Bootstrap
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income) 
m

N <- 100
X <- sample(income, N) 
median(X)

library(gridExtra)
B <- 10^4
#MointeCarlo
M <- replicate(B, {
  X <- sample(income, N)
  median(X) })
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + geom_abline()
grid.arrange(p1, p2, ncol = 2)

#Intervelo de confianza
median(X) + 1.96 * sd(X)/ sqrt(N) * c(-1, 1)
#Intervalo de confianza real
quantile(M, c(0.025, 0.975))

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

quantile(M_star, c(0.025, 0.975))

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
indexes[1]

y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

B <- 10^4
#MointeCarlo
M <- replicate(B, {
  X <- sample(y, N)
  median(X) })
M
mean(M)
sd(M)

n <- 10
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income) 
m
sd(m)
