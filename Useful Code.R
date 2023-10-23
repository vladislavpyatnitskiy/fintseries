# Open appropriate packages ##########################################

install.packages("tseries")
install.packages("quantmod")
library(fBasics)
# Upload Data from Excel #############################################

library(readxl)
data <- read_excel("Master Thesis/Commodities data.xlsx", 
                   +     sheet = "Returns")
View(data)          
attach(data)

# Plot Price Fluctuations across Commodities ##########################################

# Oil -----------------------------------------------------------------------
plot(Oil, type = "l", main = "Oil Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Trading Days", ylab = "Price in US$")

# Natural Gas -------------------------------------------------------
plot(Natural.Gas, type = "l", main = "Natural Gas Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# Gold --------------------------------------------------------------
plot(Gold, type = "l", main = "Gold Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# Silver -------------------------------------------------------------
plot(Silver, type = "l", main = "Silver Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# Corn ----------------------------------------------------------------
plot(Corn, type = "l", main = "Corn Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# Cocoa -------------------------------------------------------------------- 
plot(Cocoa, type = "l", main = "Cocoa Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# Coffee ----------------------------------------------------------------
plot(Coffee, type = "l", main = "Coffee Price Fluctuations", sub = "Source: Yahoo Finance", xlab = "Date", ylab = "Price in US$")

# More advanced path of plot creation

plot_data <- function(x){
  
  # For each column in data set
  for (n in 1:ncol(x)){ security <- x[,n]
        
    # Plot
    plot(security,
         main = sprintf("%s Performance", colnames(security),
         sub = "Source: Yahoo! Finance",
         xlab = "Trading Days",
         ylab = "Price in US$") }
}
plot_data(commodities_data)

# Jarque-Bera tests ###########################################################
jarque.bera.test(Oil)
jarque.bera.test(Natural.Gas)
jarque.bera.test(Gold)
jarque.bera.test(Silver)
jarque.bera.test(Corn)
jarque.bera.test(Cocoa)
jarque.bera.test(Coffe)

test_for_jb <- function(x){
  x <- diff(log(x))[-1,]
  apply(x, 2, function(col) jarqueberaTest(col))
}

test_for_jb(commodities_data)

# Autocorrelation tests ##################################################

autocorrelation_plots <- function(x){
  
  # Logs without NA
  x <- diff(log(x))[-1,]
  
  # For each column
  for (n in 1:ncol(x)){ security <- x[,n]
    
    # Plot acf
    acf(security, main = sprintf("%s Series", colnames(security)))
    
    # Plot pacf
    pacf(security, main = sprintf("%s Series", colnames(security)))
  }
}

autocorrelation_plots(commodities_data)

# Augmented Dickey-Fuller test  #########################################

adf.test(Oil)

# Examples ------------------------------------------------------------

adfoil <- adf.test(Oil)

# PP test ###################################################################

pp.test(Oil)

# KPSS test ################################################################

kpss.test(Oil)

# ARIMA ################################################################
arima(Oil, order = c(1,0,0)) doil <- diff(Oil)
arima(`Natural Gas`, order = c(1,0,0))
arima(Gold, order = c(1,0,0))
arima(Silver, order = c(1,0,0))
arima(Corn, order = c(1,0,0))
arima(Cocoa, order = c(1,0,0))
arima(Coffee, order = c(1,0,0))

model.arima = auto.arima(Corn, max.order = c(7 , 0 ,7) , stationary = TRUE , trace = T , ic = 'aicc')

# EWMA ################################################################

# Done in Excel

# ARCH-GARCH Models ################################################################

# ARCH -----------------------------------------------------------------

ArchTest(Oil)
arch.oil <- garchFit(~garch(1,0), Oil, trace = F)
arch.gas <- garchFit(~garch(1,0), `Natural Gas`, trace = F)
arch.gold <- garchFit(~garch(1,0), Gold, trace = F)
arch.silver <- garchFit(~garch(1,0), Silver, trace = F)
arch.corn <- garchFit(~garch(1,0), Corn, trace = F)
arch.coffee <- garchFit(~garch(1,0), Coffee, trace = F)
arch.cocoa <- garchFit(~garch(1,0), Cocoa, trace = F)

# GARCH -------------------------------------------------------------
garchoil = ugarchfit(spec=garch11.spec, Oil)
garchnat = ugarchfit(spec=garch11.spec, `Natural Gas`)
garchgold = ugarchfit(spec=garch11.spec, Gold)
garchsilver = ugarchfit(spec=garch11.spec, Silver)
garchcorn = ugarchfit(spec=garch11.spec, Corn)
garchcocoa = ugarchfit(spec=garch11.spec, Cocoa)
garchcof = ugarchfit(spec=garch11.spec, Coffee)

# EGARCH ---------------------------------------------------------------
oilegarch = ugarchfit(egarch11.spec, Oil)
nategarch = ugarchfit(egarch11.spec, `Natural Gas`)
goldegarch = ugarchfit(egarch11.spec, Gold)
silegarch = ugarchfit(egarch11.spec, Silver)
cornegarch = ugarchfit(egarch11.spec, Corn)
cofegarch = ugarchfit(egarch11.spec, Coffee)
cocoaegarch = ugarchfit(egarch11.spec, Cocoa)

# GARCH-M -------------------------------------------------------------------
mgarchoil = ugarchfit(mgarch11.spec, Oil)
mgarchnat = ugarchfit(mgarch11.spec, `Natural Gas`)
mgarchgold = ugarchfit(mgarch11.spec, Gold)
mgarchsilver = ugarchfit(mgarch11.spec, Silver)
mgarchcorn = ugarchfit(mgarch11.spec, Corn)
mgarchcof = ugarchfit(mgarch11.spec, Coffee)
mgarchcocoa = ugarchfit(mgarch11.spec, Cocoa)

# GARCH-T -----------------------------------------------------------------
tgarchoil = ugarchfit(spec=tgarch11.spec, Oil)
tgarchnat = ugarchfit(spec=tgarch11.spec, `Natural Gas`)
tgarchgold = ugarchfit(spec=tgarch11.spec, Gold)
tgarchsilver = ugarchfit(spec=tgarch11.spec, Silver)
tgarchcorn = ugarchfit(spec=tgarch11.spec, Corn)
tgarchcof = ugarchfit(spec=tgarch11.spec, Coffee)
tgarchcocoa = ugarchfit(spec=tgarch11.spec, Cocoa)

# GARCH-GJR ---------------------------------------------------------------
oilgjrgarch = ugarchfit(gjrgarch11.spec, Oil)
natgjrgarch = ugarchfit(gjrgarch11.spec, `Natural Gas`)
goldgjrgarch = ugarchfit(gjrgarch11.spec, Gold)
silgjrgarch = ugarchfit(gjrgarch11.spec, Silver)
corgjrgarch = ugarchfit(gjrgarch11.spec, Corn)
cofgjrgarch = ugarchfit(gjrgarch11.spec, Coffee)
cocoagjrgarch = ugarchfit(gjrgarch11.spec, Cocoa)
