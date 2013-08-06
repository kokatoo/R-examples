##---- Sampling

N <- 100
flips <- sample(x=c(0,1), prob=c(.5, .5), size=N, replace=T)
cum <- cumsum(flips)
n <- 1:N
probs <- cum / n
plot(n, probs, type="o", log="x")

#----

##---- Sampling Distribution Simulation

samplingDist <- function(sampleSize, N) {
    flips <- NULL
    for (i in 1:N) {
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

#----

#---- Confidence Interval Simulation

confidenceIntervalSimu <- function() {
    sampleSize <- 500
    popMean <- 100
    popStdDev <- 50
    reps <- 30
    zValue <- 1.96

    results <- c()
    for (i in 1:reps) {
        data <- rnorm(sampleSize, popMean, popStdDev)
        sampleMean <- mean(data)

        lower <- sampleMean - zValue*popStdDev/sqrt(sampleSize)
        higher <- sampleMean + zValue*popStdDev/sqrt(sampleSize)
        results <- rbind(results, c(round(sampleMean, digits=2),
                                    paste(round(lower, digits=2), round(higher, digits=2), sep=", "),
                                    popMean,
                                    popMean <= higher && popMean >= lower))
    }

    cat("\nPopulation Mean: ", popMean, "\nPopulation Std Dev: ", popStdDev,
        "\nSample Size: ", sampleSize, "\nReps: ", reps)

    cat("\nWithin CI: ", length(which(output[,4]==T))/reps * 100, "%")
    cat("\n")

    output <- matrix(results, reps, 4,
                     dimnames=list(rep("", reps),
                     c("Sample Mean", "Confidence Interval", "Pop Mean", "Within CI")))

    colnames(output) <- c("Sample Mean", "Confidence Interval", "Pop Mean", "Within CI")
    rownames(output) <- rep("", reps)
    print(output, min.colwidth=20, page.width=100, prefix.width=0, quote=F)
}

confidenceIntervalSimu()
#----
