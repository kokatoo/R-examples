pca.ex <- function() {

    pca <- prcomp(data, scale=T)

    # plot the variances
    plot(pca)

    # plot the first 2 components
    scores <- predict(pca)
    plot(scores[,1:2], type="n")
    text(x=scores[,1], y=scores[,2], labels=data$col.label)

}

