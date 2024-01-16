B <- 10000
monty_hall <- function(strategy){
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) 
  stick <- my_pick
  stick == prize_door
  switch <- doors[!doors%in%c(my_pick, show)]
  choice <- ifelse(strategy == "stick", stick, switch) 
  choice == prize_door
}
stick <- replicate(B, monty_hall("stick")) 
mean(stick)
switch <- replicate(B, monty_hall("switch")) 
mean(switch)


B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
}
results <- replicate(B, same_birthday(50)) 
mean(results)

compute_prob <- function(n, B=10000){ 
  results <- replicate(B, same_birthday(n)) 
  mean(results)
}
n <- seq(1,60)
prob <- sapply(n, compute_prob)
library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)

cy <- rep("cyan",3)
cy
ma <- rep("magenta",5)
ma
ye <- rep("yellow",7)
ye

beads <- rep(c("cyan", "magenta",  "yellow"), times = c(3,5,7)) 
mean(beads == "cyan")
x <- sample(beads,1)
x

set.seed(1986)
B <- 100000
events <- replicate(B,sample(beads,1,TRUE))
tab <- prop.table(table(events))
pc <- tab["cyan"]
pc
nc <- 1 - tab["cyan"]
nc


cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p <- cyan/(cyan+magenta+yellow)

# Print the variable `p` to the console
p

cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan/(cyan+magenta+yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <- 1-(cyan-1)/(cyan+magenta+yellow-1)

# Calculate the probability that the first draw is cyan and the second draw is not cyan.
p_1*p_2

P8 = factorial(8)/factorial(8-3)
P8
jamaica <- choose(3,3)*factorial(3)
jamaica
jamaica/P8


runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})
mean(results)


combinations(6,2)
# Different drink = 2
6*15*3

f <- function(entree){
  print(3*15*entree)
}
options <- seq(1:12)
sapply(options, f)

n <- combn(sides,2)
n
ff <- function(sides){
  print(3*2*sides)
}
options <- seq(2:100)
sapply(options, ff)



