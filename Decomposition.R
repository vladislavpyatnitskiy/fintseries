# Function to get decomposition plot
decomposition.plt <- function(x, lg = F, stat.measure = median){
  
  l <- NULL # 
  
  for (n in 1:ncol(x)){ j <- x[,n] #
  
    if (isTRUE(lg)){ f.df <- diff(log(j)) # Calculate logs
    
      f.df[1,] <- 0 } else { f.df <- j } 
    
    r.rownames <- rownames(f.df) # Take dates from index column
    
    r.rownames <- as.Date(r.rownames) # Make it in date format
    
    f.df <- data.frame(r.rownames, f.df) # Join it with main data set
    
    rownames(f.df) <- seq(nrow(f.df)) # Create sequence for index column
    
    p.df <- NULL # Define variable to contain values
    
    for (n in 2:ncol(f.df)){ s <- f.df[,n] # Loop to make monthly data
    
      # Convert daily data to monthly
      v <- tapply(s, format(as.Date(f.df[,1]), "%Y-%m"), stat.measure)
      
      df.rownames <- rownames(v) # Take dates from index column
      
      v <- data.frame(df.rownames, v) # Join with new data set
      
      rownames(v) <- seq(nrow(v)) # Generate sequence for index column
      
      colnames(v)[colnames(v) == 'df.rownames'] <- 'Date' # Name column as Date
      
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(p.df)){ p.df<-v } else { p.df<-merge(x=p.df,y=v,by="Date")} }
    
    p.df <- as.data.frame(p.df) # Convert to data frame format
    
    colnames(p.df) <- colnames(f.df) # Give column names
    
    colnames(p.df)[colnames(p.df) == colnames(p.df[1])] <- 'Date'
    
    test.dec <- decompose(ts(p.df[,-1], frequency = 12), "multiplicative")
    
    decom.plt <- plot(test.dec) # Assign plot
    
    l <- list(l, decom.plt) } # Join plots
  
  l # Display
}
decomposition.plt(prices.yahoo(c("ZIM", "AAPL"), "2020-01-01")) # Test
