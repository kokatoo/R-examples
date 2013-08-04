bernoulliBeta <- function (a=1, b=1) {
    par(mfrow=c(3, 1))
    N = 100
    data <- rbinom(N, 1, .8)
    z <- sum(data==1)

    posterior.param <- c(a+z, b+N-z)
    pData <- beta(posterior.param[1], posterior.param[2]) / beta(a, b)

    binwidth <- .005
    theta <- seq(from=binwidth/2, to=1-(binwidth/2), by=binwidth)
    pTheta <- dbeta(theta, a, b)

    # N flips, z Heads
    pDataGivenTheta <- theta^z * (1-theta)^(N-z)
    pThetaGivenData <- dbeta(theta, a+z, b+N-z)

    maxY <- max(c(pTheta, pThetaGivenData))

    # plot the prior
    plot(theta, pTheta, type="l", lwd=3, xlim=c(0,1), ylim=c(0, maxY), cex.axis=1.2,
         xlab=bquote(theta), ylab=bquote(p(theta)), cex.lab=1.5,
         main="Prior", cex.main=1.5)

    if (a > b) {
        textX <- 0
        textadj <- c(0,1)
    } else {
        textX <- 1
        textadj <- c(1,1)
    }

    text(textX, max(pThetaGivenData), bquote("beta("*theta*"|"*.(a)*","*.(b)*")" ),
         cex=1.0, adj=textadj)
    # plot the prior
    plot(theta, pDataGivenTheta, type="l", lwd=3, xlim=c(0,1), ylim=c(0, 1.1*max(pDataGivenTheta)),
         cex.axis=1.2, xlab=bquote(theta), ylab=bquote("p(D" * theta * ")"), cex.lab=1.5,
         main="Likelihood", cex.main=1.5)

    if (z>.5*N) {
        textX=0
        textadj=c(0,1)
    } else {
        textX = 1
        textadj = c(1,1)
    }
    text(textX , max(pDataGivenTheta) , cex=1.0 ,
         bquote("Data: z=" * .(z) * ", N=" * .(N) ), adj=textadj)

    # plot the posterior
    plot(theta, pThetaGivenData, type="l", lwd=3, xlim=c(0,1), ylim=c(0, 1.1*max(pThetaGivenData)),
         cex.axis=1.2, xlab=bquote(theta), ylab=bquote("p(" * theta * "|D)"), cex.lab=1.5,
         main="Likelihood", cex.main=1.5)

    if(a+z > b+N-z){
        textX=0
        textadj=c(0,1)
    } else {
        textX = 1
        textadj = c(1,1)
    }

    text(textX , max(pThetaGivenData) , cex=1.0,
         bquote("beta(" * theta * "|" * .(a+z) * "," * .(b+N-z) * ")") , adj=textadj)

    # P(D)
    text(textX, .75*max(pThetaGivenData), cex=1.0,
         bquote("p(D)=" * .(signif(pData, 3))), adj=c(0,2))

    par(mfrow=c(1,1))

    # mean of the posterior
    (z+a) / (N + a + b)
}

bernoulliBeta(.5, .5)
