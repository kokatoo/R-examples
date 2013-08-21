kmeans.ex <- function() {

    clusters <- kmeans(data, centers=3, nstart=10)
    o <- order(clusters$cluster)

    plot(data$colx, data$coly, type="n")
    text(x=data$colx, y=data$coly, labels=data$col.label, col=clusters.cluster+1)

    data.frame(data$col.label[o], clusters$cluster[o])

}

clustering.EM <- function () {

    library(mixtools)
    result <- mvnormalmixEM(data$X, arbvar=T, k=k, epsilon=1e-2)

    prob <- round(result$posterior[,1])

    plot(data$colx, data$coly, type="n")
    text(x=data$colx, y=data$coly, labels=data$col.label, col=prob+1)

    o <- order(prob)
    data.frame(data$col.label, prob[o])

}
