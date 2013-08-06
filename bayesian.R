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
