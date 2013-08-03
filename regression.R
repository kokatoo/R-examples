# simple regression
fitted <- lm(col1 ~ col2, data=data)
coef(fitted)
intercept <- coef(fitted)[1]
slope <- coef(fitted)[2]

ggplot(data, aes(x=col1, y=col2)) + geom_point() + geom_smooth(method="lm")

predict(fitted)
residuals(fitted)
summary(fitted)
# residuals vs fitted
plot(fitted.regrssion, which=1)

# log-log regression
lm.fit <- lm(log(col1) ~ log(col2), data=data)

# correlation
cor(data$col1, data$col2, na.rm=T)
# the correlation is the same as the slope of the scaled regression
coef(lm(scale(data$col1) ~ scale(data$col2)))[2]

# covariance
cov(data$col1, data$col2)
