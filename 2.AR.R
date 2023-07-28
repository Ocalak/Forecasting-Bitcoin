#AR Model settings



#Create vectores for predictions and residuals
pred <- c(NA,NA,NA)
err <- c(NA,NA,NA)



#EStimation of AR model by using OLS. lm()
for (i in 2:48) {
  model <- lm(y[1:i] ~ y_lag[1:i])#OLS Yt = b_0 + Y_t-1 + e_t
  
  b0 <- model$coefficients[[1]]# EStimated intercepts
  
  b1 <- model$coefficients[[2]]#estimated slopes
  
  pred[i+1] <- b0 + b1* y_lag[i+1]# Predict i+1 th obs by using the estimated coefficient from 1:i obs.
  
  err[i+1] <- pred[i+1] -y[i+1] #residual of  time i+1
}

for_ts <- ts(pred,start=c(2010,4),frequency=4)
#Caluclate RMSFE of AR(1)
rmfse_ar <- sqrt(mean(na.omit(err^2)))

#Plot pred vs actual btc
ts.plot(data_ts[,1],lwd=1,ylab="Bitcoin Growth Rates",ylim=c(-100,900),xlab="Quarters",main="AR(1) Predictions and Actual Bitcoin Growth Rates")
points(data_ts[,1],lwd=2,col="black")
lines(ts(pred,start=c(2010,4),frequency=4),col="red",lty=2)
points(ts(pred,start=c(2010,4),frequency=4),col="red",lwd=2)
legend("topright", c("Actual Bitcoin Growth Rate", "AR(1) Predictions"), col=c("black", "red"), lty=2)