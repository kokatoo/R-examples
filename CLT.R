CLT <- function(sampleSize=500, numRep=5000, dist="Uniform") {
    results = NULL
    for (i in 1:numRep) {
        if (dist == "Uniform") {
            results <- c(results, mean(runif(sampleSize, 0, 2)))
            label = "Uniform"
        } else if (dist == "Normal") {
            results <- c(results, mean(rnorm(sampleSize, 1, 2)))
            label = "Normal"
        } else if (dist == "Exponential") {
            results <- c(results, mean(rexp(sampleSize, 1)))
            label = "Exponential"
        } else if (dist == "Bimodal") {
            results <- c(results, c(mean(rnorm(sampleSize/2, 1, 2)),
                                    mean(rnorm(sampleSize/2, 2, 2))))
            label = "Bimodal"
        }
    }

    hist(results, breaks=seq(floor(min(results)*10)/10, ceiling(max(results)*10)/10, len=100),
         main=paste("Sampling Distribution of the Means (", label, ")", sep=""),
         ylab="Freq", xlab="Means", cex.main=.7)

    inputs <- c(sampleSize, numReps, dist)
    names(inputs) <- c("Sample Size", "Num Reps", "Distribution")
    inputs <- data.frame(inputs)
    
    cat("Sampling Distribution Mean =", mean(results), " Variance =", var(results), "\n")
}

sizes <- function () {
    par(mfrow=c(1,1))
    par(mfrow=c(2,2))

    sampleSize <- 5
    numReps <- 5000
    dist <- "Uniform"
    CLT(sampleSize, numReps, dist)

    sampleSize <- 50
    CLT(sampleSize, numReps, dist)

    sampleSize <- 500
    CLT(sampleSize, numReps, dist)

    sampleSize <- 5000
    CLT(sampleSize, numReps, dist)
}

distributions <- function () {
    par(mfrow=c(1,1))
    par(mfrow=c(2,2))

    sampleSize <- 500
    numReps <- 5000
    dist <- "Uniform"
    CLT(sampleSize, numReps, dist)

    dist <- "Exponential"
    CLT(sampleSize, numReps, dist)

    dist <- "Normal"
    CLT(sampleSize, numReps, dist)

    dist <- "Bimodal"
    CLT(sampleSize, numReps, dist)
}
    

sizes()
distributions()



