library(gutenbergr)

library(tidyverse)
library(tidytext)
options(digits = 3)

gutenberg_metadata %>% filter(str_detect(title,'Pride and Prejudice')) %>%
nrow()

gutenberg_works(title == "Pride and Prejudice")$gutenberg_id

#gutenberg_d
book <- gutenberg_download(1342,mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")
words <- book %>%
  unnest_tokens(word, text)
nrow(words)

words <- words %>% anti_join(stop_words) 
nrow(words) 

words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(n)

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)

#What proportion of words in afinn_sentiments have a positive value?
mean(afinn_sentiments$value > 0)
#How many elements of afinn_sentiments have a value of 4?
sum(afinn_sentiments$value == 4)


#LEER ARCHIVO CSV Y LIMITAR VALORES
getwd()
dat <- read_csv("mydata.csv")
#id min
idlo <- which.min(dat$revenue)
#id max
idup <- which.max(dat$revenue)
#min
vmin <- min(dat$revenue)
#max
vmax <- max(dat$revenue)

#Sustituir valores menores a 1 y mayores a 99
dat2 <- dat %>% 
  mutate(revenue = if_else(condition = revenue < 1, true      = 1, false     = revenue)) %>%
  mutate(revenue = if_else(condition = revenue > 99, true     = 99, false    = revenue))
dat2
max(dat2$revenue)
min(dat2$revenue)

#USAR CASE
dat3 <- dat %>% mutate(revenue = case_when(
  revenue < 1 ~ 1,
  revenue >= 1 & revenue <= 99 ~ revenue,
  revenue > 99 ~ 99))
dat3
max(dat2$revenue)
min(dat2$revenue)

#Guardar en archivo
write.table(dat2,file="submission.csv",row.names=FALSE,sep=",")

#ORDENAR POR CAMPO
dat_ord <- dat3[order(dat3$revenue, dat3$date, decreasing = TRUE), ]
dat_ord
print(dat_ord, n = 1000)



