library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

library(caret)
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2)) 
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") +
  scale_y_continuous(limits = c(0, 1), oob = scales::squish)


library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008) + geom_smooth(method="lm")

span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, kernel = "box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) + 
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, kernel = "normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

str(polls_2008)
total_days <- diff(range(polls_2008$day))
total_days
span <- 21/total_days
fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
str(fit_1)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)
polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.15, method.args = list(degree=1))

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

library(caret)
knn_fit <- knn3(y ~ ., data = mnist_27$train)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") 
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

fit_lm <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2)) 
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") 
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class") 
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class") 
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401) 
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class") 
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  tibble(train = train_error, test = test_error)
})
accuracy$test

dat <- data.frame(mnist_27)
  ggplot() +
  geom_line(aes(ks, accuracy$train), color="red", lty = 2) +
  geom_line(aes(ks, accuracy$test), color="orange", lty = 1)
  
ks[which.max(accuracy$test)]
max(accuracy$test)