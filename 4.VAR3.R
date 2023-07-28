

#Create predictions vector
fcst <- rep(NA,times=20)

#Findin optimal lag 
#AIC
A <- t(data_qd[c('BTC',"UNEMP","FEDF","INF","SP500")])

B_3 <- rbind(1, lag(data_qd$BTC,1), lag(data_qd$UNEMP,1), lag(data_qd$FEDF,1), lag(data_qd$INF,1), lag(data_qd$SP500,1),
             lag(data_qd$BTC,2), lag(data_qd$UNEMP,2), lag(data_qd$FEDF,2), lag(data_qd$INF,2), lag(data_qd$SP500,2),
             lag(data_qd$BTC,3), lag(data_qd$UNEMP,3), lag(data_qd$FEDF,3), lag(data_qd$INF,3), lag(data_qd$SP500,3))

aic_var_qd <- function(p, A, B_3){
  var <- A[,(p+1):50]%*%t(B_3[1:(1+5*p),(p+1):50])%*%solve(B_3[1:(1+5*p),(p+1):50]%*%t(B_3[1:(1+5*p),(p+1):50]))
  fitted <- var %*% B_3[1:(1+5*p),]
  res <- A - fitted
  sigma <- (res[,(p+1):50]%*%t(res[,(p+1):50]))/50
  aic <- log(det(sigma)) + 2/50 *(p*5^2 + 5)
}
aic_q <- c()

for (i in c(1:3)){
  aic_q[i] <- aic_var_qd(i,A, B_3)
}

lag_varp <- which.min(aic_q)
#Create predictions vector
fcst <- rep(NA,times=20)

#Create v. for residuals
res_varp_qd <- rep(NA,times=12)

#Iterated forecast for VAR(3)
for (i in 20:49) {
  
  var_model <- VAR(data_qd[1:i,-1],p=lag_varp,type="const")
  fcst[i+1] <- predict(var_model, n.ahead = 1,ci=0.95)$fcst[[1]][,1]
  
  res_varp_qd[i+1] <- fcst[i+1] - data_qd[,2][i+1]
}


btc_ts <- ts(data_qd[,2],start=c(2010,4),frequency = 4)
varp_ts <- ts(fcst,start=c(2010,4),frequency = 4)

#Plot predictions with AR1 VAR1 VAR3 and actual BTC
ts.plot(var1_ts,col="red",type="l",ylim=c(-1550,1400),lty=5,ylab="",xlab="Quarters")
lines(btc_ts,col="black",type="l",lty=2)
lines(for_ts,col="green",type="l",lty=2)
lines(varp_ts,col="blue",type="l",lty=2)
legend("bottomright", c("Actual BTC ","VAR(3)" ,"VAR(1)","AR(1)"), col=c("black", "blue","red","green"), lty=c(1,2,2,2))

#Calculate RMSFE of Var_p
rmsfe_varp <- sqrt(mean(na.omit(res_varp_qd)^2))

summary(var_model)