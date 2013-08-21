kmeans.ex <- function() {

    clusters <- kmeans(data, centers=3, nstart=10)
    o <- order(clusters$cluster)

    plot(data$colx, data$coly, type="n")
    text(x=data$colx, y=data$coly, labels=data$col.label, col=clusters.cluster+1)

    data.frame(data$col.label[o], clusters$cluster[o])

}
