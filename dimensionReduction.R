pca.ex <- function() {

    pca <- prcomp(data, scale=T)

    # plot the variances
    plot(pca)

    # plot the first 2 components
    result <- predict(pca)
    plot(result[,1:2], type="n")
    text(x=result[,1], y=result[,2], labels=data$col.label)

}

