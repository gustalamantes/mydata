# Load tidyverse
library(tidyverse)

# load package for decision tree
library(rpart)

# load the dslabs package
library(dslabs)

# fit a classification tree using the polls_2008 dataset, 
# which contains only one predictor (day)
# and the outcome (margin)
fit <- rpart(margin ~ ., data = polls_2008)

# display the decision tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# examine the fit from the classification tree model
polls_2008 %>%  
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# fit a classification tree on the mnist data using cross validation
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
# and plot it
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

# load library for random forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]



library(rpart)
library(dplyr)
library(ggplot2)

n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)


plot(fit)

text(fit)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) + geom_step(aes(x, y_hat), col=2)


library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)


library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
  
 #RPART 
  library(caret)
  library(dslabs)
  set.seed(1991)
  data("tissue_gene_expression")
  
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
  
  ggplot(fit)  
  
  
  set.seed(1991)
  data("tissue_gene_expression")
  
  fit_rpart <- with(tissue_gene_expression, 
                    train(x, y, method = "rpart",
                          tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                          control = rpart.control(minsplit = 0)))
  ggplot(fit_rpart)
  max(fit_rpart$results$Accuracy)
  confusionMatrix(fit_rpart)
  plot(fit_rpart$finalModel)
  text(fit_rpart$finalModel)
  
  set.seed(1991)
  library(randomForest)
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rf", 
                    nodesize = 1,
                    tuneGrid = data.frame(mtry = seq(50, 200, 25))))
  
  ggplot(fit)
  which.max(fit$results$Accuracy)
  max(fit$results$Accuracy)
  confusionMatrix(fit)
  fit$results$mtry[4]
  
  set.seed(1991) 
  library(randomForest)
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rf", 
                    nodesize = 1,
                    tuneGrid = data.frame(mtry = seq(50, 200, 25))))
  
  ggplot(fit)
  
  
  
  #TITANIC
  library(titanic)    # loads titanic_train data frame
  library(caret)
  library(tidyverse)
  library(rpart)
  
  # 3 significant digits
  options(digits = 3)
  
  # clean the data - `titanic_train` is loaded with the titanic package
  titanic_clean <- titanic_train %>%
    mutate(Survived = factor(Survived),
           Embarked = factor(Embarked),
           Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
           FamilySize = SibSp + Parch + 1) %>%    # count family members
    select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
  
  set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6
  test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
  train_set <- titanic_clean[-test_index,]
  test_set <- titanic_clean[test_index,]
  nrow(train_set)
  nrow(test_set)
  mean(train_set$Survived == 1)
  
  
  
  set.seed(3, sample.kind = "Rounding")
  guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
  mean(guess == test_set$Survived)
  
  
  train_set %>%
    group_by(Sex) %>%
    summarize(Survived = mean(Survived == 1))
  
  test_set %>%
    summarize( (sum(Sex == 'female' & Survived == 1) + sum(Sex == 'male' & Survived == 0)) / n())
  
  survival_class <- titanic_clean %>%
    group_by(Pclass) %>%
    summarize(PredictingSurvival = ifelse(mean(Survived == 1) >=0.5, 1, 0))
  survival_class
  
  
  test_set %>%
    inner_join(survival_class, by='Pclass') %>%
    summarize(PredictingSurvival = mean(Survived == PredictingSurvival))
  
  
  survival_class <- titanic_clean %>%
    group_by(Sex, Pclass) %>%
    summarize(PredictingSurvival = ifelse(mean(Survived == 1) > 0.5, 1, 0))
  survival_class
  
  
  test_set %>%
    inner_join(survival_class, by=c('Sex', 'Pclass')) %>%
    summarize(PredictingSurvival = mean(Survived == PredictingSurvival))
  
  
  # Confusion Matrix: sex model
  sex_model <- train_set %>%
    group_by(Sex) %>%
    summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
  test_set1 <- test_set %>%
    inner_join(sex_model, by = 'Sex')
  cm1 <- confusionMatrix(data = factor(test_set1$Survived_predict), reference = factor(test_set1$Survived))
  cm1 %>%
    tidy() %>%
    filter(term == 'sensitivity') %>%
    .$estimate
  cm1 %>%
    tidy() %>%
    filter(term == 'specificity') %>%
    .$estimate
  cm1 %>%
    tidy() %>%
    filter(term == 'balanced_accuracy') %>%
    .$estimate
  # Confusion Matrix: class model
  class_model <- train_set %>%
    group_by(Pclass) %>%
    summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
  test_set2 <- test_set %>%
    inner_join(class_model, by = 'Pclass')
  cm2 <- confusionMatrix(data = factor(test_set2$Survived_predict), reference = factor(test_set2$Survived))
  cm2 %>%
    tidy() %>%
    filter(term == 'sensitivity') %>%
    .$estimate
  cm2 %>%
    tidy() %>%
    filter(term == 'specificity') %>%
    .$estimate
  cm2 %>%
    tidy() %>%
    filter(term == 'balanced_accuracy') %>%
    .$estimate
  # Confusion Matrix: sex and class model
  sex_class_model <- train_set %>%
    group_by(Sex, Pclass) %>%
    summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
  test_set3 <- test_set %>%
    inner_join(sex_class_model, by=c('Sex', 'Pclass'))
  cm3 <- confusionMatrix(data = factor(test_set3$Survived_predict), reference = factor(test_set3$Survived))
  cm3 %>%
    tidy() %>%
    filter(term == 'sensitivity') %>%
    .$estimate
  cm3 %>%
    tidy() %>%
    filter(term == 'specificity') %>%
    .$estimate
  cm3 %>%
    tidy() %>%
    filter(term == 'balanced_accuracy') %>%
    .$estimate
  

  F_meas(data=factor(test_set1$Survived), reference = factor(test_set1$Survived_predict))
  F_meas(data=factor(test_set2$Survived), reference = factor(test_set2$Survived_predict))
  F_meas(data=factor(test_set3$Survived), reference = factor(test_set3$Survived_predict))
  
  
  #Pendiente
  fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
  Survived_hat <- predict(fit_lda, test_set)
  mean(test_set$Survived == Survived_hat)
  
  fit_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
  Survived_hat <- predict(fit_qda, test_set)
  mean(test_set$Survived == Survived_hat)
  #Fin
  
  fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
  survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
  mean(survived_hat_a == test_set$Survived)
  
  fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
  survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
  mean(survived_hat_b == test_set$Survived)
  
  str(train_set)
  fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
  survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
  mean(survived_hat_c == test_set$Survived)
  
  
  set.seed(6, sample.kind = "Rounding")
  # Method below doesn't give same result as EdX (though it is correct)
  # ks <- seq(3,51,2)
  # res_knn9a <- sapply(ks, function(k) {
  #     fit_knn9a <- knn3(Survived ~ ., data = train_set, k = k)
  #     survived_hat <- predict(fit_knn9a, train_set, type = "class") %>% factor(levels = levels(train_set$Survived))
  #     cm_test <- confusionMatrix(data = survived_hat, reference = train_set$Survived)
  #     cm_test$overall["Accuracy"]
  # })
  # ks[which.max(res_knn9a)]
  # Other method using train function
  k <- seq(3,51,2)
  fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
  fit_knn9a$bestTune
  
  ggplot(fit_knn9a)

  
  survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
  cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
  cm_test$overall["Accuracy"]  
  
  
  set.seed(8, sample.kind = "Rounding")
  fit_knn10 <- train(Survived ~ ., 
                     data=train_set, 
                     method = "knn",
                     tuneGrid = data.frame(k = seq(3, 51, 2)),
                     trControl = trainControl(method = "cv", number=10, p=0.9))
  fit_knn10
  survived_hat <- predict(fit_knn10, test_set)
  cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
  cm_test$overall["Accuracy"]
  
  
  
  set.seed(10, sample.kind = 'Rounding')
  fit_rpart11 <- train(Survived ~ ., 
                       data=train_set, 
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
  plot(fit_rpart11)
  survived_hat <- predict(fit_rpart11, test_set)
  cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
  cm_test$overall["Accuracy"]
  cm_test
  
  
  fit_rpart11$finalModel
  plot(fit_rpart11$finalModel, margin=0.1)
  text(fit_rpart11$finalModel, cex = 0.75)
  fit_rpart$finalModel

  
  
  
  
  
  set.seed(14, sample.kind = 'Rounding')
  fit12_rf <- train(Survived ~., 
                    data = train_set,
                    method = "rf", 
                    tuneGrid = data.frame(mtry = seq(1, 7)), 
                    ntree = 100)
  fit12_rf$bestTune
  survived_hat <- predict(fit12_rf, test_set)
  mean(survived_hat == test_set$Survived)
  varImp(fit12_rf)  
  
 