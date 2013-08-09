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

##---- Hypothesis Testing Simulation
pValue <- function(t.stat, df, two.tailed=F) {
    pVal <- .5 - abs(.5-pt(t.stat, df))

    if(two.tailed) {
        pVal <- 2*pVal
    }

    pVal
}
hypothesisTestingSimu <- function() {

    popMean <- 11
    popVar <- 5

    nullMean <- 10
    numSamples <- 100
    two.tailed <- T
    alpha <- .05
    sampleSize <- 30

    output <- NULL
    decisions <- c()
    for (i in 1:numSamples) {
        popMean <- 10
        popVar <- 5
        data <- rnorm(sampleSize, popMean, sqrt(popVar))

        t.stat <- (mean(data) - nullMean) / sqrt(var(data)/sampleSize)
        prob <- pValue(t.stat, sampleSize-1, two.tailed)
        t.prob <- t.test(data, mu=nullMean)$p.value

        if (prob < alpha) {
            decision = "REJECT"
        } else {
            decision = "RETAIN"
        }

        decisions <- c(decisions, decision)
        output <- rbind(output,
                        cbind(round(mean(data), digits=2), round(sqrt(var(data)), digits=2),
                              round(t.stat, digits=3), decision, round(prob, digits=3), round(t.prob, digits=3)))

    }

    dimnames(output) <- list(rep("", numSamples),
                             c("Sample Mean", "Sample SD", "t-stat", "Decision", "p-Value", "t.test$p.value"))

    cat("% RETAINED: ", length(decisions[decisions=="RETAIN"]) / length(decisions) * 100, "%")
    cat("\n")

    print(output, min.colwidth=15, prefix.width=0, quote=F)

}
hypothesisTestingSimu()
#----
