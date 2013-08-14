##---- Linear Regression

linearRegressionSimu <- function () {
    library(ggplot2)
    x <- seq(1, 10, 0.1)
    y <- 10 * 3*x
    y <- y + rnorm(length(x), 0, 50)

    data <- data.frame(x=x, y=y)
    ggplot(data, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm")

    cat("\nFrom lm:")
    cat("\n")
    print(coef(lm(y ~ x, data=data)))

    lm.fn <- function(x, a, b) {
        return(a + b*x)
    }

    squared.error <- function(data, a, b) {
        preds <- with(data, lm.fn(x, a, b))
        errors <- with(data, y - preds)

        return(sum(errors^2))
    }

    ridge.error <- function(data, a, b, lambda) {
        preds <- with(data, lm.fn(x, a, b))
        errors <- with(data, y - preds)

        return(sum(errors^2) + lambda*(a^2+b^2))
    }

    result <- optim(c(0, 0), function(x) {
        squared.error(data, x[1], x[2])
    })

    lambda <- 3
    ridge.result <- optim(c(0, 0), function(x) {
        ridge.error(data, x[1], x[2], lambda)
    })

    cat("\nFrom Optimizer:")
    cat("\nSquared Error:")
    cat("\nIntercept: ", result$par[1],  "\nSlope: ", result$par[2])
    cat("\nRidge Error:")
    cat("\nIntercept: ", ridge.result$par[1],  "\nSlope: ", ridge.result$par[2])
    cat("\n\n")
}

linearRegressionSimu()

#----

##---- Golden Section Method

goldenSection <- function(f, a, b) {

    tol <- 1e-7

    curve(f, a, b, col="skyblue")
    goldenRatio <- 2/(sqrt(5)+1)

    x1 <- b - goldenRatio*(b-a)
    x2 <- a + goldenRatio*(b-a)

    fx1 <- f(x1)
    fx2 <- f(x2)

    while(abs(b-a) > tol) {
        if (fx2 > fx1) {
            b <- x2
            x2 <- x1
            fx2 <- fx1
            x1 <- b-goldenRatio*(b-a)
            fx1 <- f(x1)
        } else {
            a <- x1
            x1 <- x2
            fx1 <- fx2
            x2 <- a+goldenRatio*(b-a)
            fx2 <- f(x2)
        }
    }

    result <- ((a+b)/2)
    grid()
    abline(v=result)
    abline(h=f(result))

}
goldenSection(function(x) x^3 - 2*x + 2, -2, 2)

#----

##---- Newton's Method

newton <- function() {

    a <- -2
    b <- 2
    f <- function(x) x^3 - 2*x + 2
    curve(f, a, b)
    grid()
    tol <- 1e-15

    fprime <- function(x) 3*x^2 - 2
    fdoublePrime <- function(x) 6*x

    x <- 2
    for(i in 1:1000) {
        fval <- f(x)
        fprimeVal <- fprime(x)

        if (abs(fprimeVal) < tol) {
            break
        }

        fdoublePrimeVal <- fdoublePrime(x)
        x <- x - fprimeVal/fdoublePrimeVal
    }

    grid
    abline(v=x)
    abline(h=f(x))
    x
}
newton()

#----
