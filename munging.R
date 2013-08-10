# help
?t.test
help.search("acf")
??acf
args(t.test)
example(t.test)
# learn more about package
help(package="stats")
vignette(package="stats")
# restricted to R documentation
RSiteSearch("stats package")
# find
find("lowess")
apropos("lm")

# details about variables
ls()
ls.str()

# list files
list.files()
list.files(all.files=T)

# current loaded packages
search()
# show all packages installed
library()
install.packages()

# remove variables
rm(var.name)
rm(list=ls())

# saving workspace
save.image()

# accessing last value
x <- .Last.value

# accessing datasets in packages
data(datasetName, package="packageName")
# show datasets
data()
# show dataset in a particular package
data(package="name")

# read html table from web
library(XML)
# 3rd table
data <- readHTMLTable('http://en.wikipedia.org/wiki/World_population', which=3)

# read in data as dataframe
data <- read.delim("data.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")
data <- read.csv("data.csv", header=TRUE)

# inspect data
head(data)
names(data)
# rename columns
names(data)<-c("col1", "col2", "col3")

# remove invalid dates
head(data[which(nchar(data$Date) != 8)])
good.rows <- ifelse(nchar(data$Date) != 8, FALSE, TRUE)
length(which(!good.rows))
length(which(good.rows))

# transform strings to dates
data$Date <- as.Date(data$Date, format="%Y%m%d"))
data$Date <- strptime(data$Date, format="%Y%m%d")=
# or using transform
data$Date <- transform(data, Date=as.Date(Date, format="%Y%m%d"))

# construct matrix from a list
matrix <- do.call(rbind, some_list)
# add matrix to dataframe using transform
data <- transform(data, colnew1=col1, colnew2=col2)

# lookup and match
us.states <- c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt", "wa","wi","wv","wy")
data$states <- data$states[match(us.states)]

# subset of data (just put in col name)
# same as bracket notation
data[!is.na(col)]
data <- subset(data, subset=(!is.na(col)))
# exclude first three element
data[-(1:3)]
training.x <- x[indices]
test.x <- x[-indices]

# merging data frames
data.merged <- merge(df1, df2, by.x=c("df1.col1, df1.col2"), by.y=c("df2.col1, df2.col2", all=TRUE))

# cut data into bins
data.bins <- cut(data, breaks=10)
table(data.bins)

# sampling
data[sample(1:nrow(data), num.samples),]
# sampling based on factors
data[sample(levels(data$col), 3)]

# duplicates
duplicated(data)
data.unique <- data.unique[!duplicated(data),]
# or
data.unique <- unique(data.unique)

# sorting
sort(data$col)
# sorting data frame
data[order(data$col),]
data[order(data$col, data$col2),]
# sorting the whole data frame
data[do.call(order, data),] # order expects a list of vectors but interprets a dataframe as a vector

# convert a list to a vector
mean(unlist(listData))

# remove element from list
lst[["element"]] <- NULL
lst[condition] <- NULL
# removing null elements from list
lst[sapply(lst, is.null)] <- NULL

# attach dataframe
attach(dataset)
# using with
with(dataset,
{
    summary(col1)
    plot(col2)
    # use <<- to create objects outside with
    summary.col <<- summary(col1)
})

# factors
grades <- factor(c("A", "B", "C", "D"), ordered=T)
edit(grades)

# max/min
which.max(data$col)
which.min(data$col)
which(data$col == max(data$col))
which(data$col == min(data$col))

## Tukey 5 num summary
# min, lower hinge, medium, upper hinge, max
fivenum(data$col)

# table
counts <- rnbinom(10000, mu=0.92, size=1.1)
table(counts)

# tapply
with(data, tapply(col1, col2, mean, na.rm=T))
# trim 20% of the left and right tail of the data
tapply(col1, col2, mean, trim=.2)
# multi-dimensional table
with(data, tapply(col1, list(col1, col2), mean))

