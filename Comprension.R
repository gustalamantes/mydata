library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat

(dat %>% 
    group_by(type) %>% 
    summarize(propf = mean(sex == "Female")))[1,] #propf = proportion of females

(dat %>% 
    group_by(type) %>% 
    summarize(propf = mean(sex == "Female")))[2,] #propf = proportion of females

ifelse(x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y)) -> y_hat 

mean(y_hat==y)

table(y_hat, y)

library(caret)
sensitivity(y_hat,y)
specificity(y_hat,y)

mean(y == "Female")


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
# line of code
test <- iris[test_index,]
train <- iris[-test_index,]

test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
f <- function(x){
  rv <- seq(min(x), max(x), by=0.1) #rv = ranged values
  sapply(rv,
         function(i){
           y_hat <- ifelse(x > i,'virginica','versicolor')
           mean(y_hat == train$Species)} #here we can find the accuracy 
  )}

predictions <- apply(train[,-5],MARGIN = 2, FUN = f)


sapply(predictions,max) 

predictions <- f(train[,3]) #f is previously created function
rv <- seq(min(train[,3]),max(train[,3]),by=0.1) #rv = ranged values
cutoffs <-rv[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

predictions <- apply(test[,-5], MARGIN = 2, FUN = f) #f is the previously created function
sapply(predictions,max) 

plot(iris, pch=21, bg=iris$Species)
petalLR <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1) #PetalLR = Petal Length Range
petalWR <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1) #PetalWR = Petal Width Range

length_predictions <- sapply(petalLR,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

length_cutoff <- petalLR[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWR,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

width_cutoff <- petalWR[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)




set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)
mean(disease[test==0])
mean(disease[test==1])
mean(disease[test==1]==1)/mean(disease==1) 

library(dslabs)
data("heights")
# MISSING CODE

qplot(height, p, data = heights %>% 
          mutate(height = round(height)) %>%
          group_by(height) %>%
          summarize(p = mean(sex == "Male")))


ps <- seq(0, 1, 0.1)
heights %>% 
	# MISSING CODE
	group_by(g) %>%
	summarize(p = mean(sex == "Male"), height = mean(height)) %>%
	qplot(heights, p, data = mutate(g = cut(heights, quantile(height, ps), include.lowest = TRUE)))


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)
ps <- seq(0, 1, 0.1)
dat %>% 
  # MISSING CODE
  qplot(x, y, data = mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
          group_by(g) %>%
          summarize(y = mean(y), x = mean(x)))
