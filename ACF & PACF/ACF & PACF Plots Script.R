# Function to plot both ACF and PACF
autocorrelation_plots <- function(x, lg = T){
  
  # Calculate Returns and remove NA if applicable
  if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  # For each column define variable and create plot
  for (n in 1:ncol(x)){ security <- x[,n]
    
    # ACF
    acf(security, main = sprintf("%s Series", colnames(security)))
    
    # PACF
    pacf(security, main = sprintf("%s Series", colnames(security))) }
}
# Test
autocorrelation_plots(stock_data)
