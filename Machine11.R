library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(dslabs)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                  package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE) 
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n == 1),
           which(n >= 28), tail_index:length(s))
  s[-out] %>%  str_remove_all("[^\\d\\s]") %>% str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>% .[,1:5] %>% as_tibble() %>%
    setNames(c("day", header)) %>%
    mutate(month = month, day = as.numeric(day)) %>%
    pivot_longer(-c(day, month), names_to = "year", values_to = "deaths") %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month,
                        "JAN" = 1, "FEB" = 2, "MAR" = 3,
                        "APR" = 4, "MAY" = 5, "JUN" = 6,
                        "JUL" = 7, "AGO" = 8, "SEP" = 9,
                        "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

fit_3 <- loess(deaths ~ day, degree=1, span = span, data=dat)
fit_4 <- loess(deaths ~ day, span = span, data=dat)

dat %>% ggplot(aes(day, deaths)) +
  geom_line() +
  geom_smooth(method = "loess", span = 2, method.args = list(degree=1))


span <- 2
fit_3 <- loess(deaths ~ day, degree=1, span = span, data=dat)
fit_4 <- loess(deaths ~ day, span = span, data=dat)

dat %>% mutate(smooth_3 = fit_3$fitted, smooth_4 = fit_4$fitted) %>%
  ggplot(aes(day, deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_3), color="red", lty = 2) +
  geom_line(aes(day, smooth_4), color="orange", lty = 1)


library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>%
  glm(y ~ x_2, family = "binomial", data = .) %>%
  tidy()
qplot(x_2, y, data = mnist_27$train) +
  geom_smooth(method = "loess", span = 0.5, method.args = list(degree=1))


set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]