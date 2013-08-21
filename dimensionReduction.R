pca.ex <- function() {

    pca <- prcomp(data, scale=T)

    # plot the variances
    plot(pca)

    # plot the first 2 components
    plot(data[,1:2], type="n")
    text(x=data[,1], y=data[,2], labels=data$col.label)

}

