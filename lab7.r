# Imitate N=100 copies of Wiener process:
# W(t) = sum(k = 1,N; 2*sqrt(2) * (sin((2k-1)*pi*t/2) / (2k-1)*pi )

library(ggplot2)

Wiener <- function(Timeline, sigma = 1)
{
  ksi <- rnorm(n = length(Timeline), mean = 0, sd = sigma)
  coef <- c()
  
  k <- 1
  for(t in Timeline)
  {
    c <- sqrt(2)*(sin((k-0.5)*pi*t)/((k-0.5)*pi))
    coef <- c(coef, c)
    k <- k + 1
  }
  return(cumsum(ksi*coef))
}


Timeline <- seq(from = 0, to = 1, by = 0.001)
N <- Wiener(Timeline)

ggplot() + geom_path(aes(x = Timeline, y = N)) + ggtitle("Wiener process") + xlab('t') + ylab('N(t)')