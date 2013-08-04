set.seed(1)

# Cauchy distribution
# fatter tails than norm
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)
ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

# gamma distribution
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()

# binomial distribution
binomial.values <- rbinom(numReps, numTrials, prob)
table(factor(binomial.values, levels=0:numTrails)) / numReps
# expected value/variance
for (i in 0:numTrials) {
    probs <- choose(numTrials,i) * (prob^i) * (1-prob)^(numTrials-i)
    exp <- exp + probs[i+1]*i
    variance <- variance + probs[i+1]*i^2
}

# Monte Carlo Integration for Standard Normal Distribution
monteCarloInt <- function () {
    numPoints <- 10000
    min <- -1
    max <- 1
    height <- 1/sqrt(2*pi)
    xPts <- runif(numPoints, min, max)
    yPts <- runif(numPoints, 0, height)

    # P(-1 < x < 1)
    prob <- sum(yPts <= (1/sqrt(2*pi)) * exp(-.5*xPts^2))/numPoints * height * (max - min)

    prob
}
monteCarloInt()

# chisquare simulation
chiSqSimu <- function () {
    popMean <- 0
    popStdDev <- 1
    sampleSize <- 6
    reps <- 250

    data <- NULL
    for(i in 1:reps) {
        sampleData <- rnorm(sampleSize, popMean, popStdDev)
        data <- cbind(data, sum((sampleData - mean(sampleData))^2) / popStdDev^2)
    }

    range <- seq(0,20,1)
    freqTable <- table(cut(data, range), exclude=NA)/reps

    barplot(freqTable, xlab="Group", ylab="Freq", col="skyblue", main="Hist of Chi-square Values", names=as.character(1:length(freqTable)))
}
chiSqSimu()

# t simulation
tTestSimu <- function () {
    popMean <- 50
    popStdDev <- 15
    sampleSize <- 5
    reps <- 250
    par(mfrow=c(2,1))

    zData <- NULL
    tData <- NULL
    for(i in 1:reps) {
        sampleData <- rnorm(sampleSize, popMean, popStdDev)
        zData <- cbind(zData, (mean(sampleData) - popMean) / (popStdDev/sqrt(sampleSize)))
        tData <- cbind(tData, t.test(sampleData, mu=popMean)$statistic)
    }

    range <- seq(-4, 4, .5)
    zFreqTable <- table(cut(zData, range), exclude=NA)/reps
    tFreqTable <- table(cut(tData, range), exclude=NA)/reps

    barplot(zFreqTable, xlab="Group", ylab="Freq", col="skyblue", main="Hist of Z Values", names=as.character(1:length(zFreqTable)))
    barplot(tFreqTable, xlab="Group", ylab="Freq", col="skyblue", main="Hist of t Values", names=as.character(1:length(tFreqTable)))

    par(mfrow=c(1,1))
}
tTestSimu()
