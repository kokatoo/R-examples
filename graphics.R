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


