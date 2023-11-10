r.montly <- function(x, stat.measure = median, lg = F){
  
  if (isTRUE(lg)){ f.df <- diff(log(x)) # Calculate logs

    f.df[1,] <- 0 } else {  # Assign first log as 0
  
    f.df <- x } 
      
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
    if (is.null(p.df)){ p.df<-v } else { p.df <- merge(x=p.df,y=v,by="Date")} }
  
  p.df <- as.data.frame(p.df) # Convert to data frame format
  
  colnames(p.df) <- colnames(f.df) # Give column names
  
  colnames(p.df)[colnames(p.df) == colnames(p.df[1])] <- 'Date'
  
  return(p.df) # Rename again
}
r.montly(stock_data, stat.measure = median, lg = F) # Test
