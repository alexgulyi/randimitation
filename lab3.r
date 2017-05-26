# Imitating shot noise process according to:
# x(t) = sum(k=1:inf; g(t - sum(i=1:k; xi_i))) + sum(k=1:inf; g(t + sum(i=1:k; eta_i))),
# where g() - measured & absolutely integrated function,
# xi_i, eta_i - i.i.d.r.v. with Exp(1) distribution

g <- function(t)
{
  # g() - measured & absolutely integrated function to make x(t)
  g_scalar <- function(t)
  {
    if(t >= 0 && t <= 0.5)
      return(1)
    else
      return(0)
  }
  return(sapply(X = t, FUN = g_scalar))
}

ShotNoise <- function(t, num, inLambda = 1)
{
  # returns value of x(t)
  
  # auxiliary random variable to build process: xi or eta
  # 2 different calls of aux_rvar provides independency
  aux_rvar <- function(n = num, lambda = inLambda)
  {
    return(rexp(n, rate = lambda))
  }
  
  # X = X1 + X2
  X1 <- sum(g(t - cumsum(aux_rvar())))
  X2 <- sum(g(t + cumsum(aux_rvar())))
  X <- X1 + X2
  return(X)
}

EvaluateTail <- function(s, imitNum, over)
{
  # calculating P{max[X(t)]>=s} with imitation
  p <- 0
  for(i in 1:imitNum)
  {
    Sample <- sapply(over, ShotNoise, num = 100)
    M <- max(Sample)
    if(M >= s)
      p <- p + 1/imitNum
  }
  return(p)
}

library(ggplot2)
N <- 1000

Timeline = seq(from = 0, to = 10, by = 0.1)
ShotNoiseValues <- sapply(Timeline, ShotNoise, num = N)
qplot(x = Timeline,
      y = ShotNoiseValues,
      geom = "path",
      main = "Shot Noise Process",
      xlab = "t",
      ylab = "x(t)")
print("Calculated value for P{max[X(t)]>=3}:")
print(EvaluateTail(3, imitNum = N, over = Timeline))