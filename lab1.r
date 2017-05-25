# Imitate Poisson process built on the exponential random variables and plot its trajectories

library(ggplot2)

Poisson <- function(seed_n, seed_rate)
{
  # function to generate values of the Poisson process contructed on exponential random variables
  return(c(0,cumsum(rexp(n = seed_n, rate = seed_rate))))
}

DrawTrajectory <- function(numInst,
                           timeline,
                           N,
                           inRate)
{
  # function drawing <numInst> trajectories of the Poisson process over <timeline>
  # <N> and <inRate> used to generate Poisson process instances within function
  
  plot <- ggplot() + ggtitle("Trajectories of Poisson process") + labs(x = 't', y = 'N(t)')
  for(i in 1:numInst)
  {
    plot <- plot + geom_path(aes(x = T, y = Poisson(N, inRate)))
  }
  plot
}

## input parameters
lambda <- 1.0
tMax <- 10 * lambda
num <- tMax
T <- 0:10
# call example
DrawTrajectory(10, T, num, lambda)