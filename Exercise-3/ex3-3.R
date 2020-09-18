#Load dataset
hwsign_data <- read.table(file='./Highwaysigns.txt', header=TRUE)
head(hwsign_data)

x.3 <- hwsign_data$age
y.3 <- hwsign_data$distance
n.3 <- length(x.3)

#a. Fit simple regression line using the least squares method
reg_line.3 <- lm(y.3~x.3)
plot(x.3, y.3)
abline(reg_line.3)

#b. 95% CI average distance 75 years of age
new_data.3 <- data.frame(x.3 = c(75))
reg_line_95c.3 <- predict(reg_line.3, interval="c", newdata = new_data.3)
#c. 95% PI average distance 75 years of age
reg_line_95p.3 <- predict(reg_line.3, interval="p", newdata = new_data.3)

#d. Overlay two intervals from b) and c) onto the scatterplot of the datapoints and explain the difference between them


#e. Diagnostics plots
par(mfrow=c(2,2))
plot(reg_line.3)