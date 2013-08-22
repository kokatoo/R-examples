##---- Summary Statistics

summary(data$col)
range(data$col)
quantile(data$col)

library(Hmisc)
describe(data$col1)

library(fBasics)
basicStats(data$col1)

#----

##---- Aggregation

# tapply is good at summarizing vector
# group by INDEX
tapply(X=data$col, INDEX=list(data$col.id), FUN=sum)

# by works on dataframe
by(data[, c("col1", "col2", "col3")], INDICES=list(data$col.id), FUN=mean)

# aggregate
aggregate(x=data[, c("col1", "col2", "col3")], by=list(data$col.id), FUN=sum)

# summing up for each group and attribute (2x2 statistics)
rowsum(data[, c("col1", "col2", "col3")], group=data$col.id)

# counts from 1 to max
data.counts <- tabulate(data$col1)
# related fn for categorical variable
table(data$col1)
table(data$co1, data$col2)

#----

##---- Reshaping Data

# each row represents a unique col1 and each column represents a unique col2
data.reshape <- reshape(data, idvar="col1", timevar="col2", direction="wide"))
# calls to reshape are reversible
reshape(data.reshape)

library(reshape)
# cast col1 to be the rows, col2 to be the column and col3 to be the value in the cell
date.matrix <- cast(data, col1 ~ col2, value=col3)

#----

##---- Group Summary Statistics

# y, z are response variables
# x, y are explanatory variables
aggregate(y~x, data, mean)
aggregate(y~x+y, data, mean)
aggregate(cbind(y, z)~x+y, data, mean)

# parallel min, max
x <- 1:10
y <- 2:11
z <- 3:12
pmin(x, y, z)
pmax(x, y, z)

## tapply
# group x by y or (y & z)
tapply(x, y, mean)
tapply(x, list(x, y), mean)

#----

##--- SQL

library(sqldf)
sqldf("select * from iris where Sepal_Width<3 order by Sepal_Length", row.names=T)
sqldf("select avg(Sepal_Width) from iris group by Species", row.names=T)

#----

##---- Visualization

# create multiple plots
dev.new()
plot()
dev.next()
plot()
dev.off()

# saving old par
old.par <- par(no.readonly=T)
par(old.par)

# abline
abline(lm.fit)

# histogram
data.hist <- ggplot(data, aes(x=DateOccurred)) + geom_histogram()
data.hist <- ggplot(data, aes(x=DateOccurred)) + geom_histogram(binwidth = 5)

# kde
data.kde <- ggplot(heights.weights, aes(x = col1, fill = factor2)) + geom_density()
# faceted plot
ggplot(heights.weights, aes(x = col1, fill = factor2)) + geom_density() + facet_grid(factor2 ~ .)

# with scale
library(scale)
data.hist <- ggplot(data, aes(x=DateOccurred)) + geom_histogram() + scale_x_date(major="5 years")
# save plot
ggsave(plot=data.hist, filename="images/data_hist.png", height=6, width=8)

# scatterplots
# geom_smooth
ggplot(heights.weights, aes(x = colx, y = coly)) + geom_point() + geom_smooth()
# fit a logit model
logit.model <- glm(y ~ col1 + col2, data = data, family = binomial(link = 'logit'))
ggplot(data, aes(x = colx, y = coly, color = factor2)) + geom_point() +
    stat_abline(intercept = - coef(logit.model)[1] / coef(logit.model)[2],
                slope = - coef(logit.model)[3] / coef(logit.model)[2], geom = 'abline',
                color = 'black')

# mosaicplot
mosaicplot(factor(data$col1)~factor(data$col2))
tab <- table(factor(data$co1), factor(data$col2))
mosaicplot(tab)

#----

#--- Anova

# aov requires one big vector as well as a second parallel factor column (ind) that identifies the group
stackedData <- stack(list(data1=data1, data2=data2, data3=data3))
aov(values~ind, data=stackedData)

#----
