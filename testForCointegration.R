library(zoo)
library(quantmod)
library(tseries)

stockLoaded = function(
  stock){
  # returns if a stock has been loaded properly
  if (exists(stock)) {
    return (sum(class(get(stock)) == c("xts","zoo"))==2)
    } else {
  return (FALSE)
  }
}
  

getStock = function(
  symbols,
  src="google",
  days=100,
  attribute="Close"
){
  # The function getStock is used to create a combined time series of multiple stocks
  
  # Only loads historical stock prices not currently in memory
  stocks.to.retrieve=symbols[!sapply(symbols,exists)]
  
  try(getSymbols(Symbols=stocks.to.retrieve, src=src, warnings=FALSE), silent=TRUE)
  
  # If one or more of the securities could not be loaded, exit
  if (sum(!sapply(symbols,stockLoaded)) > 0) {
    return (NA)  
  } else {
    z<-xts()
    for(symbol in symbols) { 
      data<-get(symbol)
      z<-merge(z,data)
    }
    z<-z[,grep(attribute,colnames(z))]
    z<-last(z,days)
    return (z)
  }
}

getCointegration <- function(
  securities, num.days){
  
  prices <- getStock(securities, days=num.days)
  if (is.na(prices) || ncol(prices) < 2 || sum(is.na(prices))) {
    return (NA)
  } else {
    
    # cat(paste("Date range is from ", min(index(prices)), " to ", max(index(prices))))
    
    prices<-as.data.frame(prices)
    
    colnames(prices)<-c("sec.one","sec.two")
    m <- lm(sec.one ~ sec.two + 0, data=prices)
    beta <- coef(m)[1]
    
    # cat("Assumed hedge ratio ", beta)
    
    sprd <- prices$sec.one - (beta*prices$sec.two)
    # sec.one = Beta * sec.two + error
    
    ht <- adf.test(sprd, alternative="stationary", k=0)
    # testing for stationary time series, null hypothesis = non-stationary
    
    p <- ht$p.value
    # the smaller the p-value the more likely it is to be mean reverting
    # approx < 0.10 or < 0.5
    
    #  if(p<TOLERANCE){
    #    cat(sec.one, " and ", sec.two, " likely mean reverting with a p value of", p )
    #  } else {
    #    cat(sec.one, " and ", sec.two, " likely NOT mean reverting with a p value of", p)
    #    }
  return(p)
  }
}

SRC <- "google"

# TOLERANCE<-0.05
NUM.DAYS = 100

# read symbols
symbols <- scan("sp100.txt", what="", sep="\n")

# create matrix to house cointegration values
cointegration.p.scores <- matrix(nrow=length(symbols), ncol=length(symbols))
dimnames(cointegration.p.scores) <- list(symbols,symbols)

# determine combinations
symbols.combinations <- combn(symbols,2)

for(i in 1:ncol(symbols.combinations)){
  coint.value<-getCointegration(symbols.combinations[,i], NUM.DAYS)
  cointegration.p.scores[symbols.combinations[1,i],symbols.combinations[2,i]]<- coint.value
}
