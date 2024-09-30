lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

price.yearly <- function(x, y = mean, data=T, s=NULL, e=NULL){ 
  
  if (isTRUE(data)){ p <- NULL # 4 scenarios
    
    for (A in x){ if (is.null(s) && is.null(e)) { 
      
        q <- getSymbols(A, src = "yahoo", auto.assign = F)
        
      } else if (is.null(e)){ q <- getSymbols(A,from=s,src="yahoo",
                                              auto.assign=F)
      
      } else if (is.null(s)){ q <- getSymbols(A, to=e, src="yahoo",
                                              auto.assign=F)
      
      } else { q <- getSymbols(A, from = s, to = e, src="yahoo",auto.assign=F)}
        
        p <- cbind(p, q[,4]) } # Join all columns into one data frame
      
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in column names
    
    x <- as.timeSeries(p) } # Make it time series and display
  
  x <- data.frame(as.Date(rownames(x)), x) # Join it with main data set
  
  rownames(x) <- seq(nrow(x)) # Create sequence for index column
  
  D <- NULL # Define variable to contain values
  
  for (n in 2:ncol(x)){ s <- x[,n] # Loop to make monthly data
  
    v <- tapply(s, format(as.Date(x[,1]), "%Y"), y) # daily to monthly
    
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
price.yearly(c("HG=F"), median, data=T) # Test
