# Function to get decomposition plot
decomposition.plt <- function(x, lg = F, stat.measure = median){
  
  l <- NULL 
  
  for (n in 1:ncol(x)){ j <- x[,n]
  
    if (isTRUE(lg)){ j <- diff(log(j))[-1,] } # Calculate logs
    
    j <- data.frame(as.Date(rownames(j)), j) # Join main data set
    
    rownames(j) <- seq(nrow(j)) # Create sequence for index column
    
    D <- NULL # Define variable to contain values
    
    for (n in 2:ncol(j)){ s <- j[,n] # Loop to make monthly data
    
      # Convert daily data to monthly
      v <- tapply(s, format(as.Date(j[,1]), "%Y-%m"), stat.measure)
      
      v <- data.frame(as.data.frame(rownames(v)), v) # Join with new data set
      
      rownames(v) <- seq(nrow(v)) # Generate sequence for index column
      
      colnames(v)[1] <- 'Date' # Name column as Date
      
      # If defined empty variable is still empty # Put new dataset there
      if (is.null(D)){ D <- v } else { D <- merge(x = D, y = v, by = "Date")} }
      
    D <- as.data.frame(D) # Convert to data frame format
    
    colnames(D) <- colnames(D) # Give column names
    
    colnames(D)[1] <- 'Date'
    
    test.dec <- decompose(ts(D[,-1], frequency = 12), "multiplicative")
    
    decom.plt <- plot(test.dec) # Assign plot
    
    l <- list(l, decom.plt) } # Join plots
    
    l # Display
}
decomposition.plt(prices.yahoo(c("ZIM", "AAPL"), "2020-01-01")) # Test
