#U_MIDAS
#Run OLS with full model
#K=1
f_aic_1 <- lm(y ~ y_lag +X1_1 + X2_1 + X3_1 + X4_1+X1_2+X2_2+X3_2+X4_2)

#K=2
f_aic_2 <- lm(y ~ y_lag +X1_1 + X2_1 + X3_1 + X4_1+X1_2+X2_2+X3_2+X4_2+X1_3+X2_3+X3_3+X4_3)

#K=3
f_aic_3 <- lm(y ~ y_lag +X1_1 + X2_1 + X3_1 + X4_1+X1_2+X2_2+X3_2+X4_2+X1_3+X2_3+X3_3+X4_3+X1_4+X2_4+X3_4+X4_4)

#Implement a function that calculate AIC score.

#AIC =log(SSR/T)+(p+1)*T/2 
AIC_score <- function(model){
  res <- model$residuals
  SSR <- sum(na.omit(res)^2)
  Tx <- length(na.omit(res))
  npar <- length(model$coef)
  aic <- log(SSR/Tx)+(npar)*(2/T)
  return(aic)
}
#Save AIC score 
optimal_lag <- c(AIC_score(f_aic_1),AIC_score(f_aic_2),AIC_score(f_aic_3))

#Find the lag which is min AIC score 
p <- which.min(optimal_lag)

#Create a vectore for predictions
pred_umidas <- rep(NA,11)

print(p)# p =1
data_ts <- ts(y,start = c(2010,4),frequency = 4)
#Iterated forecast for U-MIDAS
for (i in 10:48) {
  model_1 <- lm(y[1:i] ~ y_lag[1:i] +X1_1[1:i] + X2_1[1:i] + X3_1[1:i] + X4_1[1:i]+X1_2[1:i]+X2_2[1:i]+X3_2[1:i]+X4_2[1:i])#
  
  pred_umidas[i+1] <- (model_1$coefficients[1] + model_1$coefficients[2]*y[i] + 
                         model_1$coefficients[3]*X1_1[i+1] + model_1$coefficients[4]*X2_1[i+1] +
                         model_1$coefficients[5]*X3_1[i+1] + model_1$coefficients[6]*X4_1[i+1] +
                         model_1$coefficients[7]*X1_2[i+1] + model_1$coefficients[8]*X2_2[i+1] +
                         model_1$coefficients[9]*X3_2[i+1] + model_1$coefficients[10]*X4_2[i+1])
  
}


#Plot predictions of U-Midas full
ts.plot(ts(pred_umidas,start=c(2010,4),frequency = 4),type="l",col="red",ylab="",xlab="Quarters",ylim=c(-600,1000),lty=3)
lines(data_ts,col="black",type="l",lty=3,lwd=2)
#points(data_ts,col="purple")
#points(ts(pred_umidas,start=c(2010,4),frequency = 4),col="green")
legend("topright", c("Actual ","U-MIDAS"), col=c("black", "red"), lty=3)
#
#RMSFE
res_mid <- na.omit(pred_umidas-data_ts)
rmsfe_umidas_f <- sqrt(mean(res_mid^2))
pacf(res_mid)
summary(model_1)

jarque.bera.test(res_mid)
#REduced U-MIDAS with UNemp

red_pred <- rep(NA,5)
for (i in 3:48) {
  model_red <- lm(y[1:i] ~ y_lag[1:i]+ X1_1[1:i])#+X1_2[1:i])
  red_pred[i+1] <- (model_red$coefficients[1]  + model_red$coefficients[2]*y[i]+
                      model_red$coefficients[3]*X1_1[i+1])#+model_red$coefficients[4]*X1_2[i])

}

plot(ts(red_pred, start=c(2010,4),frequency = 4),col="blue",ylim=c(-500,1000),type="l",ylab="",xlab="Quarters",lwd=2,lty=3)
lines(data_ts,col="black",type="l",lwd=2,lty=3)
#lines(ts(data_ts,start=c(2010,4),frequency = 4),col="clack")
lines(ts(pred_umidas, start=c(2010,4),frequency = 4),col="red",type="l",lty=3)
legend("topright", c("Actual ","U-MIDAS","U-MIDAS Reduced"), col=c("black", "red","blue"),lty=3)
#RMSFE
res_red <- na.omit(red_pred-y)
rmse_umidas_red <- sqrt(mean(res_red^2))




