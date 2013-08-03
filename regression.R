# simple regression
fitted <- lm(col1~col2, data=data)
coef(fitted)
intercept <- coef(fitted)[1]
slope <- coef(fitted)[2]

ggplot(data, aes(x=col1, y=col2)) + geom_point() + geom_smooth(method="lm")

predict(fitted)
residuals(fitted)
# residuals vs fitted
plot(fitted.regrssion, which=1)
