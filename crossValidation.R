leaveOneOut <- function(X, y, formula) {

    n <- length(y)
    diff <- NULL
    perDiff <- NULL

    for (i in 1:n) {
        train <- c(1:n)
        train <- train[train != i]
        fit <- lm(formula, data=X[train,])

        obs <- y[-train]
        pred <- predict(fit, newdat=X[-train,])
        diff[k] <- obs -pred
        perDiff[k] <- abs(diff[k])/obs
    }

    meanError <- mean(diff)
    rmse <- sqrt(mean(diff**2))
    meanAbsPerError <- 100*(mean(perDiff))

    data.frame(meanError, rmse, meanAbsPerError)

}
