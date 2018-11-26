require(quantmod)
require(PerformanceAnalytics)

# Step 1: Get the data
getSymbols("SPL.AX")
SPL<-SPL[complete.cases(SPL),]                                                  # Delete NA

# Step 2: Create your indicator
dvi <- DVI(Cl(SPL$adjusted))
DVI(Cl(SPL))
# Convert indicartor to a tibble
dvi <- as_data_frame(dvi)

?DVI
# Step 3: Construct your trading rule
sig <- Lag(ifelse(dvi["dvi"]  < 0.5, 1, -1))
#sig <- Lag(ifelse(dvi$dvi < 0.5, 1, -1))

# Step 4: The trading rules/equity curve
ret <- ROC(Cl(SPL))*sig
ret <- ret['2009-06-02/2010-09-07']
eq <- exp(cumsum(ret))
plot(eq)

# Step 5: Evaluate strategy performance
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)

require(quantmod)
source("dvi.analysis.r")

getSymbols("SPL.AX", from="1900-01-01")
SPL<-SPL[complete.cases(SPL),]                                                  # Delete NA
aa = dvi.analysis(Cl(SPL["/2009"]), lags=seq(1,8), normalize=T, file.path="dvi.png")

require(quantmod)

prepare.indicator = function(close, indicator, roc.n, normalize=FALSE, func=mean) {
  rets = ROC(close, type="discrete", n=roc.n)
  
  if(normalize) {
    # Normalize the returns to daily
    rets = ((1 + rets) ^ (1/roc.n)) - 1
  }
  
  mm = merge(na.exclude(lag(indicator, k=roc.n)), na.exclude(rets), all=F)
  dd = as.numeric(mm[,1])
  
  # Map the indicator values into the tenth intervals
  ee = ceiling(dd*10)
  ee = ifelse(ee == 0, 1, ee)
  
  # Create the factors
  ff = factor(ee, labels=as.character(seq(0.1, 1, 0.1)))
  
  # Split the returns according to the factors
  gg = split(as.numeric(mm[,2]), ff)
  yy = sapply(gg, func)
  
  return(list(raw.res=gg, res=yy, rets=rets))
}

dvi.analysis = function(close, lags=c(5), normalize=FALSE, file.path, do.plot=TRUE, width=800, height=1200, func=mean) {
  # Redirect the plot if necessary
  if(do.plot && !missing(file.path)) {
    png(filename=file.path, width=width, height=height, units='px', pointsize=12, bg='white')
  }
  
  if(length(lags) %% 2 == 0) {
    par(mfrow=c(length(lags) / 2, 2))
  } else {
    par(mfrow=c(length(lags), 1))
  }
  
  ind = TTR:::DVI(close)[,3]
  res = list()
  raw.res = list()
  rets = list()
  
  for(ll in lags) {
    xx = prepare.indicator(close, ind, roc.n=ll, normalize=normalize, func=func)
    yy = xx$res
    
    barplot(
      yy,
      ylim=c(-max(abs(yy)), max(abs(yy))),
      col=ifelse(yy<0, "darkblue", "red"),
      main=paste(as.character(ll), "-day returns", sep=""),
      xlab="DVI level",
      ylab="Expected return")
    
    res[[as.character(ll)]] = xx$res
    raw.res[[as.character(ll)]] = xx$raw.res
    rets[[as.character(ll)]] = xx$rets
    # return(list(gg=gg, ff=ff, ee=ee, dd=dd))
  }
  
  # Restore the plot output
  if(do.plot && !missing(file.path)) {
    dev.off();
  }
  
  return(list(res=res, raw.res=raw.res, rets=rets))
}