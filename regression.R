# simple regression
fitted <- lm(col1 ~ col2, data=data)
coef(fitted)
intercept <- coef(fitted)[1]
slope <- coef(fitted)[2]

ggplot(data, aes(x=col1, y=col2)) + geom_point() + geom_smooth(method="lm")
# plot a smooth, nonlinear representation
ggplot(data, aes(x=col1, y=col2)) + geom_point() + geom_smooth()

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

# plot scatterplots of all pairs
plot(data)
# using ellipse
library(ellipse)
plotcorr(cor(data))

# logistic regression
log.fit <- glmnet(x, y, family="binomial")
# turning logistic regression output into probs
library(boot)
inv.logit(predict(log.fit, newx=x, s=.001))

# polynomial regression
poly.fit <- lm(coly ~ poly(colx, degree=3), data=data)

# regression subsets
# each subset show the top 2 and use all subsets
result <- summary(regsubsets(X, y, nbest=2, nvmax=ncol(X)))
cbind(result$which, result$rsq, result$adjr2, out$cp)

# one way to measure model complexity
model.complexity <- sum(coef(lm.fit)^2)

# residual plot
plot(model$res~model$fitted)
hist(model$res)

##---- lasso

lasso.ex <- function(data) {

    library(lars)
    X <- model.matrix(col1 ~ col2 + col3 + col4 + col5, data=data)
    lasso <- lars(x=X, y=data$y, trace=T)
    plot(lasso)

    coef(lasso, s=c(.25, .5, .75, 1), mode="fraction")

    # K-fold cross validation
    cv.lars(x=X, y=data$y, K=10)

}

#----

##---- Cross validation using regularization
library(glmnet)
rmse <- function(y, pred) {
    return(sqrt(mean((y-pred)^2)))
}
cv.regularization <- function(x, y) {
    n <- length(x)
    indices <- sort(sample(1:n, round(n/2)))

    training.x <- x[indices]
    training.y <- y[indices]

    test.x <- x[-indices]
    test.y <- y[-indices]

    data <- data.frame(x, y)
    training.df <- data.frame(x=training.x, y=training.y)
    test.df <- data.frame(x=test.x, y=test.y)

    glmnet.fit <- with(training.df, glmnet(poly(x, degree=10), y))
    lambdas <- glmnet.fit$lambda

    result <- data.frame()
    for (lambda in lambdas) {
        result <- rbind(result, data.frame(Lambda=lambda,
                                       RMSE=rmse(test.y,
                                       with(test.df, predict(glmnet.fit, poly(x, degree=10), s=lambda)))))
    }

    print(ggplot(result, aes(x=Lambda, y=RMSE)) + geom_point() + geom_line() + scale_x_log10())
    best.lambda <- with(result, Lambda[which(RMSE==min(RMSE))])

    glmnet.fit <- with(data, glmnet(poly(x, degree=10), y))
}
set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2*pi*x) + rnorm(length(x), 0, 0.1)
cv.regularization(x, y)

#----
