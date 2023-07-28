#VAR P setting for Motnhly data
library(lattice)
library(caret)
#Lets find the optimal lag
Y <- t(data_md[c('BTC', 'UNEMP', 'FEDF', 'INF', 'SP500')])
Z_12 <- rbind(1, lag(data_md$BTC,1), lag(data_md$UNEMP,1), lag(data_md$FEDF,1), lag(data_md$INF,1), lag(data_md$SP500,1),
              lag(data_md$BTC,2), lag(data_md$UNEMP,2), lag(data_md$FEDF,2), lag(data_md$INF,2), lag(data_md$SP500,2),
              lag(data_md$BTC,3), lag(data_md$UNEMP,3), lag(data_md$FEDF,3), lag(data_md$INF,3), lag(data_md$SP500,3),
              lag(data_md$BTC,4), lag(data_md$UNEMP,4), lag(data_md$FEDF,4), lag(data_md$INF,4), lag(data_md$SP500,4),
              lag(data_md$BTC,5), lag(data_md$UNEMP,5), lag(data_md$FEDF,5), lag(data_md$INF,5), lag(data_md$SP500,5),
              lag(data_md$BTC,6), lag(data_md$UNEMP,6), lag(data_md$FEDF,6), lag(data_md$INF,6), lag(data_md$SP500,6),
              lag(data_md$BTC,7), lag(data_md$UNEMP,7), lag(data_md$FEDF,7), lag(data_md$INF,7), lag(data_md$SP500,7),
              lag(data_md$BTC,8), lag(data_md$UNEMP,8), lag(data_md$FEDF,8), lag(data_md$INF,8), lag(data_md$SP500,8),
              lag(data_md$BTC,9), lag(data_md$UNEMP,9), lag(data_md$FEDF,9), lag(data_md$INF,9), lag(data_md$SP500,9),
              lag(data_md$BTC,10), lag(data_md$UNEMP,10), lag(data_md$FEDF,10), lag(data_md$INF,10), lag(data_md$SP500,10),
              lag(data_md$BTC,11), lag(data_md$UNEMP,11), lag(data_md$FEDF,11), lag(data_md$INF,11), lag(data_md$SP500,11),
              lag(data_md$BTC,12), lag(data_md$UNEMP,12), lag(data_md$FEDF,12), lag(data_md$INF,12), lag(data_md$SP500,12))
lng <- nrow(data_md)
aic_var_md <- function(p, Y, Z_12){
  var <- Y[,(p+1):149]%*%t(Z_12[1:(1+5*p),(p+1):149])%*%solve(Z_12[1:(1+5*p),(p+1):149]%*%t(Z_12[1:(1+5*p),(p+1):149]))
  fitted <- var %*% Z_12[1:(1+5*p),]
  res <- Y - fitted
  t <- sum(is.na(res) == FALSE)/5
  sigma <- (res[,(p+1):lng]%*%t(res[,(p+1):lng]))/t
  aic <- log(det(sigma)) + 2/t *(p*5^2 + 5)
}

aic_m <- c()

for (i in c(1:12)){
  aic_m[i] <- aic_var_md(i,Y, Z_12)
  
}
lag <- which.min(aic_m)# Which is 3



#Create vector for prediction
fcst_m <- rep(NA,times=48)

data_md <- data_md[3:149,]
#Iterated forecasts for VAR(3) for montlhy data
for (i in 16:146) {
  
  var_model_m <- vars::VAR(data_md[1:i,-1],p=3,type="const")
  fcst_m[i+1] <- predict(var_model_m, n.ahead = 1)$fcst[[1]][,1]
  
}


fcst_m <- ts(fcst_m,start=c(2010,10),frequency = 12)
btcmm <- ts(data_md[,2],start=c(2010,10),frequency = 12)
#Plot the predictions 
ts.plot(fcst_m,type="l",col="red",ylab="",xlab="Months",ylim=c(-450,500))
lines(btcmm,type="l")
legend("bottomright", c("Actual ","VAR(3)"), col=c("black", "red"),lty=1)


#Get the residuals and RMSFE
residuals_varm <- na.omit(data_md[,2]-fcst_m)
rmsfe_var_m <- sqrt(mean(residuals_varm^2))



