library(tidyverse)

data(mtcars)

mtcars %>%
  ggplot(aes(x = mpg)) +
  stat_bin(binwidth = 2)
  geom_histogram()
           