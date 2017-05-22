# Imitating the amount of expenses in following:
# A person, older than 65 y.o., buys some medicine during 1 year.
# The number of purchases ~ Po(25), purchase's amount of money ~ U(5,95)

library(ggplot2)

# calculating total costs for each year by the number of purchases
CostCalculate <- function(num)
{
  return(sum(runif(num, 5, 95)))
}

DrawPlot <- function(title, DataFrame, x = "Timeline", y)
{
  ggplot() + geom_path(data = DataFrame, aes_string(deparse(DataFrame[x]),
                                                    deparse(DataFrame[y]))) + ggtitle(title)
}

main <- function()
{
  # number of years to imitate independently
  Y <- 1000
  Timeline <- seq(from = 1, to = Y, by = 1)
  
  # imitating Y years for one person:
  # number of purchases
  PurchaseNum <- rpois(Y,25)
  Expenses <- sapply(PurchaseNum, CostCalculate)
  DataFrame <- data.frame(Timeline, PurchaseNum, Expenses)

  ggplot(data = DataFrame, aes(x = DataFrame["Timeline"], y = DataFrame["PurchaseNum"])) + geom_line() + ggtitle("PurchaseNum")
  ggplot(data = DataFrame, aes(x = DataFrame["Timeline"], y = DataFrame["Expenses"])) + geom_path() + ggtitle("Amount of expenses")
  
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