r.montly <- function(x){ # Function to make data frame with monthly returns
  
  x <- diff(log(x))[-1,] # Calculate logs
  
  x <- data.frame(as.Date(rownames(x)), x) # Join it with main data set
  
  rownames(x) <- seq(nrow(x)) # Create sequence for index column
  
  D <- NULL # Define variable to contain values
  
  for (n in 2:ncol(x)){ s <- x[,n] # Loop to make monthly data
  
    v <- tapply(s, format(as.Date(x[,1]), "%Y-%m"), sum) # daily to monthly
    
    v <- data.frame(rownames(v), v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[1] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(D)){ D <- v } else { D <- merge(x = D, y = v, by = "Date") } }
    
  D <- as.data.frame(D) # Convert to data frame format
  
  colnames(D) <- colnames(x) # Give column names
  
  colnames(D)[1] <- 'Date'
  
  D # Display
}
r.montly(stock_data) # Test
