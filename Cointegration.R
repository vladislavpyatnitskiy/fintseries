library("urca") # Library

cointegration <- function(x, s=NULL, e=NULL){ # Cointegration for stocks
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  for (A in x){ if (is.null(s) && is.null(e)) { 
    
      q <- getSymbols(A, src = "yahoo", auto.assign = F)
    
      } else if (is.null(e)){ q <- getSymbols(A,from=s,src="yahoo",
                                              auto.assign=F)
  
      } else if (is.null(s)){ q <- getSymbols(A,to=e,src="yahoo",auto.assign=F)
  
      } else { q <- getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F) }
    
    p <- cbind(p, q[,4]) } # Join all columns into one data frame
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in column names
  
  as.timeSeries(p) # Make it time series and display
  
  summary(ca.jo(p, type = "trace", ecdet = "const", K = 2))
}
cointegration(c("UNM", "AIG")) # Test
