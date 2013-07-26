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
barplot(t(data.m), legend=TRUE, ylim=c(0, 10000)

#----
