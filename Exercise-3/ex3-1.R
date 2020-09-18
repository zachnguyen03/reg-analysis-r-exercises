library(tidyverse)

hosp_data <- read.table(file='./hospital.txt', header= TRUE)
head(hosp_data)

x <- hosp_data$Stay
y <- hosp_data$InfctRsk

#a. Use lm() to fit regression line for x and y
reg_line <- lm(y~x)
summary(reg_line)
plot(x,y)
abline(reg_line)
# anova(reg_line) prints ANOVA table for reg_line


#b. 95% confidence interval
new_data <- data.frame(x = c(7.5,10, 12.5))
reg_line_95c <- predict(reg_line, interval="c", newdata=new_data)

#c. 95% prediction interval
reg_line_95p <- predict(reg_line, interval="p", newdata=new_data)
 
#d. Unbiased estimator for error variability
b0 <- reg_line$coefficients[1]
b1 <- reg_line$coefficients[2]
yhat <- b0 + b1*x # fitted yhat
MSE <- (sum((yhat-y)^2)/(n-2))
#Standard Error of b1
SE_b1 <- (1/sqrt(n))*sqrt(MSE) / sqrt(mean((x-mean(x))^2))
