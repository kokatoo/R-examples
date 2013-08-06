bayesUpdate <- function () {
    numThetas <- 50
    thetas <- seq(0.01, 0.99, len=numThetas)

    # make the probs triangular
    priors <- pmin(thetas, 1-thetas)
    priors <- priors / sum(priors)

    # 3 H 9 T
    data <- c(rep(1,3), rep(0,9))
    numHeads <- sum(data)
    numTails <- length(data) - numHeads
    
    # likelihood
    pDataGivenTheta = thetas^numHeads * (1-thetas)^numTails
    
    pData = sum(pDataGivenTheta*priors)
    posteriors = pDataGivenTheta * priors / pData

    par(mfrow=c(2,2))
    plot(thetas, priors, type="h", lwd=3, main="Prior",
         xlim=c(0,1), xlab=bquote(thetas),
         ylim=c(0,1.1*max(posteriors)), ylab=bquote(priors),
         cex.axis=1.2, cex.lab=1.5, cex.main=1.5, col="skyblue")

    # plot the likelihood
    plot(thetas, pDataGivenTheta, type="h", lwd=3, main="Likelihood",
     xlim=c(0,1), xlab=bquote(theta),
     ylim=c(0, 1.1*max(pDataGivenTheta)), ylab=bquote(pDataGivenTheta),
     cex.axis=1.2, cex.lab=1.5, cex.main=1.5, col="skyblue")
    
    # plot the posterior
    plot(thetas, posteriors, type="h", lwd=3, main="Posterior",
     xlim=c(0,1), xlab=bquote(theta),
     ylim=c(0, 1.1*max(posteriors)), ylab=bquote(posteriors),
     cex.axis=1.2, cex.lab=1.5, cex.main=1.5, col="skyblue")

    # show p(D)
    text(.55, .85*max(posteriors), cex=1.0, bquote("p(D)=" *.(signif(pData, 3))), adj=c(0,.5))
    
    par(mfrow=c(1,1))
}
bayesUpdate()

bernoulliGrid <- function (thetas, priors, data) {
    z <- sum(data == 1)
    N <- length(data)

    pDataGivenTheta <- thetas^z*(1-thetas)^(N-z)
    pData <- sum(pDataGivenTheta * priors)

    pThetaGivenData <- pDataGivenTheta * priors / pData

    # plot prior
    plot(thetas, priors, pch=".", xlim=c(0,1), ylim=c(0, 1.1* max(priors)),
         main="Prior", xlab=bquote(theta), ylab=bquote("p(" * theta * ")"))
    # plot likelihood
    plot(thetas, pDataGivenTheta, pch=".", xlim=c(0,1), ylim=c(0, 1.1* max(pDataGivenTheta)),
         main="Likelihood", xlab=bquote(theta), ylab=bquote("p(D|" * theta * ")"))
    # plot posterior
    plot(thetas, pThetaGivenData, pch=".", xlim=c(0,1), ylim=c(0, 1.1* max(pThetaGivenData)),
         main="Posterior", xlab=bquote(theta), ylab=bquote("p(" * theta * "|D)"))
}

bernoulliGridSimu <- function () {
    binwidth <- 1/100
    thetas <- seq(binwidth/2, 1-binwidth/2, binwidth)
    
    # 3H 1T
    data <- c(rep(1, 3), rep(0, 1))

    # uniform distribution
    priors <- c(rep(1, length(thetas)))
    priors <- priors / sum(priors)

    par(mfrow=c(3, 3)
    bernoulliGrid(thetas, priors, data)

    # unimodal distribution
    priors <- sapply(seq(-3, 3, len=length(thetas)), dnorm)
    priors <- priors/sum(priors)
    bernoulliGrid(thetas, priors, data)

    # bimodal distribution
    priors <- sapply(seq(-3, 3, len=length(thetas)), function (x) { dnorm(x, mean=-2) })
    priors <- priors + sapply(seq(-3, 3, len=length(thetas)), function(x) { dnorm(x, mean=2) })
    bernoulliGrid(thetas, priors, data)
       
    par(mfrow=c(1,1))
}
