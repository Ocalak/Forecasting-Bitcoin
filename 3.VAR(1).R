


#Creating Vector for predictions
var1_fcast <- matrix(NA, nrow = nrow(data_qd[,-1]), ncol = 1)

#Residuals vector
res_var1 <- rep(NA,times=7)

#Iterated Forecasting 
for (i in 7:49) {
  
  # Subset the data up to the previous observation
  data_sub <- data_qd[1:i,-1]
  
  # Estimate a VAR(1) model using the previous data
  var1_fit <- VAR(data_sub, p = 1,season=NULL,type="const")
  
  # Forecast the next observation using the VAR(1) model
  var1_fcast[i+1] <- predict(var1_fit, n.ahead = 1,ci=0.95)$fcst$BTC[1]
  
  
  res_var1[i+1] <- (var1_fcast[i+1] - data_qd[,2][i+1])
}
#Calculate RMSFE of VAR(1)
rmsfe_var1 <- sqrt(mean(na.omit(res_var1)^2))

#
var1_ts <- ts(var1_fcast,start=c(2010,4),frequency = 4)

#Check stationarity of the unemployment
grg <- adf.test(data_qd[,3])
grg$p.value


#Plot predictions and actual BTC
ts.plot(var1_ts,lwd=2,ylab="BTC Growth Rate",col="blue",main="VAR(1) Predictions")
lines(for_ts,col="red")
lines(data_ts[,1],col="black")
points(for_ts,col="red",lwd=1)
points(var1_ts,col="blue",lwd=2)
legend("bottomright", c("Actual ","AR(1)" ,"VAR(1)"), col=c("black", "red","blue"), lty=1)


##Granger Causality

p_value <- 0
for (i in 3:6) {
  
  p_value[i-2] <- grangertest(data_qd[,i],data_qd[,2],order=1)$`Pr(>F)`[2]
}
p_value <- data_frame(p_value)
colnames(p_value) <- "Pr(>F)"
p_value$sig <- c("#","","","")
p_value$Variables <- c("UNEMP","SP500","FEDF","INF")
p_value
#  `Pr(>F)` sig   Variables
#<dbl> <chr> <chr>    
#1   0.0484 "#"   UNEMP   # 
#2   0.0722 ""    SP500    
#3   0.478  ""    FEDF     
#4   0.931  ""    INF 
