#Libraries, Options, and Data 
#Be sure that you have installed the titanic package before proceeding.

#Define the titanic dataset starting from the titanic library with the following code:
  
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(titanic)
class(titanic)
titanic
total <- titanic %>%
  filter(Sex %in% c("female","male")) %>%
  count(Sex)
total

age40 <- titanic %>%
  filter(Age == 40 & Sex %in% c("female","male")) %>%
  count(Sex)
age40

max_age <- titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  summarize(max = max(Age))
max_age

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, group = Sex, fill = Sex)) + 
  geom_density(alpha = 0.2, bw = 10) 



titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Sex)) + geom_density(alpha=0.2,bw=2,position="stack")
titanic %>% .$Sex %>% table()
titanic %>% filter(Age == 40) %>% .$Sex %>% table()
titanic %>% filter(Age >= 18 & Age <= 35) %>% .$Sex %>% table()
titanic %>% filter(Age < 17) %>% .$Sex %>% table()



params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params
titanic %>% ggplot(aes(sample=Age)) + geom_qq(dparams = params) + geom_abline()


p <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()
p


p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass ~ Sex)
p
