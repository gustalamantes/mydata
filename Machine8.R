library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

data(heights)
heights
str(heights)
heights$height[777]
heights$sex[777]
heights[777,1]
max(heights$height)
which.min(heights$height)

mean(heights$height)
median(heights$height)

group_counts <- table(heights$sex)
total_count <- sum(group_counts)
percentage_by_group <- (group_counts / total_count) * 100
percentage_by_group

library(dplyr)
data("heights")
result_dplyr <- heights %>%
  group_by(sex) %>%
  summarise(Percentage = n() / nrow(heights) * 100)
print(result_dplyr)

index <- sum(heights$height > 78 & heights$sex == "Female")
index
