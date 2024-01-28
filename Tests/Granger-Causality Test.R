library(lmtest) # Library

granger.test <- function(x, y, q, df, lg = F){ # Granger Causality Test
  
  if (isTRUE(lg)){ x <- diff(log(x))[-1,]
  
    y <- diff(log(y))[-1,] } # Make logs when needed
  
  l <- NULL
  
  # Find values until 20 lags
  for (n in 1:q) { l <- cbind(l,grangertest(y ~ x, order = n, data = df)[2,4])}
  
  colnames(l) <- seq(q) # Rename column names
  
  l
}
granger.test(stock_data[,2], stock_data[,1], 20, stock_data, lg = T) # Test
