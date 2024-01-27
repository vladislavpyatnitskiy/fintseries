library(lmtest) # Library

granger.test <- function(x, y, q, df){ # Granger Causality Test
  
  grangertest(y ~ x, order = q, data = df)[4]
}
granger.test(stock_data[,2], stock_data[,1], 10, stock_data) # Test
