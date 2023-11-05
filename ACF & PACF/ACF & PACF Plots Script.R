autocorrelation.plt <- function(x, lg = T){ # Plot both ACF and PACF
  
  if (isTRUE(lg)) { x = diff(log(x))[-1,] } # Make logs and remove NA if needed
  
  for (n in 1:ncol(x)){ s <- x[,n] # Plots for each column
  
    acf(s, main = sprintf("%s Series", colnames(s)), las = 1) # ACF
    pacf(s, main = sprintf("%s Series", colnames(s)), las = 1) } # PACF
}
autocorrelation.plt(stock_data) # Test
