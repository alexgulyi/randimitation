# Imitate and plot Levy process built as
# N(t) = sum(k=1:t; xi_k), where xi_k ~ Po(lambda)

LevyProc <- function(arg, rate = Lambda)
{
  # process starts at 0 and receives increments that have Poisson distribution
  return(sum(rpois(n = arg, lambda = rate)))
}

## input parameters
Lambda <- 1.0
N <- 100
T <- seq(from = 0, to = 1, by = 1/n)

# construction of the Levy process for 0, 1/n, 2/n, .... k/n, .... 99/n, 1
# multiplication T by 100 needed as LevyProc takes integer <arg>
N <- sapply(X = T*100, FUN = LevyProc)

# drawing plot of the trajectory
ggplot() + geom_path(aes(x = T, y = N)) + ggtitle('Levy process trajectory') + labs(x = 't', y = 'N(t)')