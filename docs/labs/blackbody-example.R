library(tidyverse)
example <- data.frame(t = c(1:1000),
                      m = 5.7*10^-8*t^4,
                      lamda = 2898/t)

ggplot(data = example) +
  geom_point(aes(x = t, y = m)) +
  labs(x = "Temperature", y = "Emitted radiation")

ggplot(data = example) +
  geom_point(aes(x = t, y = lamda)) +
  labs(x = "Temperature", y = "Dominant wavelength")
