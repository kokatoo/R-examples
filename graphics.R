##---- scatterplots

# plot
plot(data$col1, data$col2)
# show the data point you click
locator(1) # and click on the point to get $x, $y

# all pairs
pairs(data[data$col > 100, c("AB", "AC", "AD")))

#----

##--- timeseries

plot(data.ts)
# autocorrelation plot
acf(data.ts)

#----

##---- bar charts

# barplot doesn't work on data frame
barplot(data.m[rownum,])

# show legend and plot horizontally
barplot(data.m, beside=TRUE, horiz=TRUE, legend=TRUE, cex.names=.75)

# stacked bars
# set ylim to make sure legend is visible
barplot(t(data.m), legend=TRUE, ylim=c(0, 10000))

#----

##---- Pie charts

pie(data$col, init.angle=100, cex=.6)

#----

##---- Categorical Data plots

# plotting the conditional density of a set of categories dependent on a numerical value
cdplot(categories~numeric, data=data, subset=(data$col > 100))

# plotting 2 categorical variables
mosaicplot(formula=cat1~cat2, data=data, color=TRUE)
spineplot(formula=cat1~cat2, data=data)

#----

##---- 3D Plots

# persp
x <- seq(-10, 10, length=30)
y <- x
f <- function(x, y) {
    r <- sqrt(x^2+y^2);
    10*sin(r)/r;
}
z <- outer(x, y, f)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

# image and contour plots
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col=terrain.colors(100), axes=F)
contour(x, y, volcano, levels=seq(90,200,by=5), add=T, col="peru")

#----

##---- Plotting Distributions

# histogram
hist(data$col, breaks=50)

# density
plot(density(data$col))
# plot strip plot along the axis of density
# each point is represented by a line
rug(data$col)

# Q-Q plot
# plot quantiles from data to quantiles from a theoretical distribution
qqnorm(data$col)

# Box plot
# the adjacent values = largest value less than 1.5 * IQR
boxplot(col1~categorical1, data=data)


#----
