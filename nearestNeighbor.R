# for standardizing the data
library(textir)

knn.example <- function(X, y, k) {

    library(class)

    n <- dim(X)[1]
    n.train <- floor(n*.6)

    set.seed(1)
    train <- sample(1:n, nt)

    X <- normalize(X)

    nearestK <- knn(train=X[train,], test=X[-train,], cl=y[train], k)
    plot(X[train,], col=y[train], main="KNN")
    legend("topright", legend=levels(y))

    pCorrectClassification <- 100*sum(y[-train] == nearestK) / (n - n.train)

    # cross validation
    pCorrectClassifications <- NULL
    for(k in 1:10) {
        pred <- knn.cv(X, y, k)
        pCorrectionClassifications[k] <- 100*sum(y == pred) / n
    }

    max(pCorrectionClassifications)

}
