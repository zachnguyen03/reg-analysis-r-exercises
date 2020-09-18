#Load dataset
carstop_data <- read.table(file='./carstop.txt', header=TRUE)
head(carstop_data)

x.2 <- carstop_data$Speed
y.2 <- carstop_data$StopDist
n = length(x.2)
#a. Fit regression line using Least Squares Method
reg_line.2 <- lm(y.2~x.2)

#b. Give standard error of the parameter estimates
b0.2 <- reg_line.2$coefficients[1]
b1.2 <- reg_line.2$coefficients[2]
yhat.2 <- b0.2 + b1.2*x.2 # fitted yhat
MSE.2 <- (sum((yhat.2-y.2)^2)/(n-2))
#Standard Error of b1
SE_b1.2 <- (1/sqrt(n))*sqrt(MSE.2) / sqrt(mean((x.2-mean(x.2))^2))

#c. Diagnostics to check model assumptions
par(mfrow = c(2,2))
plot(reg_line.2) #First line not so horizontal => not good. Second graph: Normal Q-Q: residuals are normally distributed.

#d. Improve model by transforming the response variable Y to sqrt(Y)
y_sqrt <- sqrt(carstop_data$StopDist)
reg_line_ysqrt <- lm(y_sqrt~x.2)
b0.2_sqrt <- reg_line_ysqrt$coefficients[1]
b1.2_sqrt <- reg_line_ysqrt$coefficients[2]
yhat.2_sqrt <- b0.2_sqrt + b1.2_sqrt*x # fitted yhat
MSE.2_sqrt <- (sum((yhat.2_sqrt-y_sqrt)^2)/(n-2))
#Standard Error of b1.2
SE_b1.2_sqrt <- (1/sqrt(n))*sqrt(MSE.2_sqrt) / sqrt(mean((x.2-mean(x.2))^2))

#Diagnostics of second model
par(mfrow=c(2,2))
plot(reg_line_ysqrt)

#e. 95% prediction intervals
new_data <- data.frame(x=c(10,20,30,40,50))
reg_line_95p <- predict(reg_line_ysqrt, interval="p", newdata=new_data)

#f. transform predicted values and intervals back to the original scale
