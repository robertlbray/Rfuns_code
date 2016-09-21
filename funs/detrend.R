detrend<-function(x, season.factor, polynomial.order=3, t=seq(x)/length(x)){
#This function decomposes series x into time trends, seasonal shifts, and residuals. The time trends are the fitted values of a polynomial of degree polynomial.order. season.factor is a vector that specifies the seasons.
  library('reshape2')
  trend.reg=lm(x~poly(t, degree=polynomial.order), na.action=na.exclude)
  season.reg=lm(residuals(trend.reg)~as.factor(season.factor), na.action=na.exclude)
  data.frame(raw=x, trend=fitted(trend.reg), season=fitted(season.reg), detrend=residuals(season.reg))
}