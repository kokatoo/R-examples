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

##---- Monte Carlo Integration for Standard Normal Distribution
numPoints <- 10000
min <- -1
max <- 1
height <- 1/sqrt(2*pi)
xPts <- runif(numPoints, min, max)
yPts <- runif(numPoints, 0, height)

# P(-1 < x < 1)
prob <- sum(yPts <= (1/sqrt(2*pi)) * exp(-.5*xPts^2))/numPoints * height * (max - min)

#----
