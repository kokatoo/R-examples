##---- Sampling

N <- 100
flips <- sample(x=c(0,1), prob=c(.5, .5), size=N, replace=T)
cum <- cumsum(flips)
n <- 1:N
probs <- cum / n
plot(n, probs, type="o", log="x")

#----
samplingDist <- function(sampleSize, N) {
    flips <- NULL
    for (i in 1:N/sampleSize) {
        flips <- rbind(flips,mean(sample(x=seq(1, 6), prob=rep(1/6, 6), size=sampleSize, replace=T)))
    }
    hist(flips, breaks=6, col="skyblue", main=paste("Sample Size = ", sampleSize))

    cat("\n\n")
    cat("Sample Size: ", sampleSize)
    cat("\n", "Mean = ", mean(flips), "\n", "Std Dev =", sd(as.vector(flips)))
}

samplingDistributionSimu <- function () {
    N <- 10000
    flips <- sample(x=seq(1,6), prob=rep(1/6, 6), size=N, replace=T)

    par(mfrow=c(2,2))
    hist(flips, breaks=6, col="skyblue", main="All Data")
    cat("\n")
    cat("All Data")
    cat("\n", "Mean = ", mean(flips), "\n", "Std Dev =", sd(flips))

    samplingDist(10, N)
    samplingDist(100, N)
    samplingDist(1000, N)

    cat("\n\n")
    par(mfrow=c(1,1))
}
samplingDistributionSimu()
