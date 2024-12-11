#import packages
library(ggplot2)
library(solitude)

#creating data
n = 1000
Var1 = c(rnorm(n, 0, 0.5), rnorm(n*0.1, -2, 1))
Var2 = c(rnorm(n, 0, 0.5), rnorm(n*0.1,  2, 1))
outliers = c(rep(0, n), rep(1, (0.1*n))) + 3
data = data.frame(Var1, Var2)

#plotting data
ggplot(data, aes(x = Var1, y = Var2)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

#create isolation forest using isolationForest function
iforest <- isolationForest(data)

#predict outliers within dataset
data$pred <- predict(iforest, data, type = "anomaly_score")
data$outlier <- as.factor(ifelse(data$pred >=0.50, "outlier", "normal"))

#plot data again with outliers identified
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

#adjusting threshold can isolate greater outliers
data$outlier <- as.factor(ifelse(data$pred >=0.55, "outlier", "normal"))

#plot data again
ggplot(data, aes(x = Var1, y = Var2, color = outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")