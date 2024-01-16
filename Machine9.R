library(dslabs)
mnist <- read_mnist()
y <- mnist$train
y


y[5] + y[6]
y[5] > y[6]

library(tidyverse)
library(caret)

library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height

y

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_index

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

test_set
train_set

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
y_hat

mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat
mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
plot(cutoff,accuracy)

data.frame(cutoff, accuracy) %>% ggplot(aes(cutoff,accuracy)) + geom_line()
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)] 
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat) 
mean(y_hat == test_set$sex)

table(predicted = y_hat, actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male") 
prev

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm


cm$overall["Accuracy"]
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% ggplot(aes(cutoff,F_1)) + geom_line()
plot(cutoff,F_1)
max(F_1)
best_cutoff <- cutoff[which.max(F_1)] 
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)

specificity(data = y_hat, reference = test_set$sex)



p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex)) 
mean(y_hat == test_set$sex)

probs <- seq(0, 1, length.out = 10) 
guessing <- map_df(probs, function(p){
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))

ml <- list(method = "Guessing",
     FPR = 1 - specificity(y_hat, test_set$sex),
     TPR = sensitivity(y_hat, test_set$sex))
ml



dat <- data.frame(cutoff, F_1)
ggplot(dat, aes(cutoff,F_1, color = "orange")) +
  geom_line() +
  geom_point() +
  geom_text(label=cutoff, hjust = 1.2)

dat2 <- data.frame(guessing) 
ggplot(dat2, aes(dat2$FPR,dat2$TPR, color = "blue")) +
  geom_line() +
  geom_point() +
  geom_text(label = round(dat2$FPR, digits = 2), hjust = 1.2)


library(lubridate)
data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) &
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 &
                         between(minute(date_time), 15, 30),
                       "inclass", "online")) %>% select(sex, type)
y <- factor(dat$sex, c("Female", "Male"))
y

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
y_hat

mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat
mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == dat$type)
})
plot(cutoff,accuracy)

data.frame(cutoff, accuracy) %>% ggplot(aes(cutoff,accuracy)) + geom_line()
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)] 
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat) 
mean(y_hat == test_set$sex)

table(predicted = y_hat, actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male") 
prev

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm

cm$overall["Accuracy"]
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% ggplot(aes(cutoff,F_1)) + geom_line()
plot(cutoff,F_1)
max(F_1)
best_cutoff <- cutoff[which.max(F_1)] 
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)

specificity(data = y_hat, reference = test_set$sex) 

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex)) 
mean(y_hat == test_set$sex)

probs <- seq(0, 1, length.out = 10) 
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))

  
cut(heights, quantile(heights, seq(0, 1, 0.1)), include.lowest = TRUE)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$train %>% ggplot(mnist_27,aes(x_1, x_2, color = y)) + geom_point()

