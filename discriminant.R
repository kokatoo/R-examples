library(MASS)

discriminant.ex <- function() {

    lin <- lda(y~., data, prior=c(1, 1, 1)/3)
    predict(lin, newdata=data.frame(col1=1, col2=2, col3=3))$class
    plot(lin)

    quad <- qda(y~., data, prior=c(1, 1, 1)/3)
    predict(quad, newdata=data.frame(col1=1, col2=2, col3=3))$class

}
