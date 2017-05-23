# Imitating the amount of expenses in following:
# A person, older than 65 y.o., buys some medicine during 1 year.
# The number of purchases ~ Po(25), purchase's amount of money ~ U(5,95)

# calculating total costs for each year by the number of purchases
CostCalculate <- function(num)
{
  return(sum(runif(num, 5, 95)))
}

DrawPlot <- function(title, x = Timeline, y)
{
  ggplot(data = NULL, aes(x, y)) + geom_path() + ggtitle(title) + xlab(substitute(x)) + ylab(substitute(y))
}

main <- function()
{
  library(ggplot2)
  
  # number of years to imitate independently
  Y <- 1000
  Timeline <- seq(from = 1, to = Y, by = 1)
  
  # imitating Y years for one person:
  # number of purchases
  PurchaseNum <- rpois(Y,25)
  Expenses <- sapply(PurchaseNum, CostCalculate)
  
  DrawPlot(y = PurchaseNum, title = "Purchase Number")
  DrawPlot(y = Expenses, title = "Expenses")
  
  AvgCost <- mean(Expenses)
  print("Average expenses cost is:")
  print(AvgCost)
  
  # P{costs per year >= 2000} - ?
  p <- length(Expenses[Expenses >= 2000])/length(Expenses)
  print("P{costs per year >= 2000} is:")
  print(p)
}

# script execution
main()