# Imitate Wiener process on [0;1]

library(ggplot2)

Wiener <- function(length, sigma = 1)
{
  # according to definition
  initValue <- 0
  # sd = sigma / length, as (t-s) = const
  return(c(initValue, cumsum(rnorm(n = (length-1), mean = 0, sd = sigma / length))))
}

Timeline <- seq(from = 0, to = 1, by = 0.001)
N <- Wiener(length(Timeline))

ggplot() + geom_path(aes(x = Timeline, y = N)) + ggtitle("Wiener process") + xlab('t') + ylab('N(t)')