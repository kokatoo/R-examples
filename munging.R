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

# ranking
highestRank <- rank(data$col1)[length(data$col1)]
lowestRank <- rank(data$col1)[1]

# order col1 by the order of col2
data$col1[order(data$col2)]
with(data, col1[order(col2, col3)])

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

# tranform
data <- transform(data, newCol1=col1+col2, newCol2=col1*col2)

## recoding variables
# within is similar to with but allow you to modify dataframe
users <- within(users, { age.cat <- NA
                         age.cat[age < 12] <- kids
                         age.cat[age < 21] <- teens
                         age.cat[age >= 21] <- adults})

# renaming variables
library(reshape)
users <- rename(users, c(id="userID", Name="First-Name"))
names(users) <- c("userID", "First-Name")

# omitting incomplete data
na.omit(data)

## Dates
# check out lubridate and fCalendar packages
as.Date(c("2013-01-01", "2013-01-02"))
as.Date(c("2013-30-01", "2013-31-01"), format="%Y-%d-%m")
# today's date
Sys.Date()
format(Sys.Date(), format="%B %d %Y")
format(Sys.Date(), format="%A")
# today's date and time
date()
# difftime
difftime(Sys.Date(), as.Date("2012-01-01"), unit="weeks")

## Merging Datasets
merged <- merge(data1, data2, by="id")
merged <- merge(data1, data2, by=c("id", "state"))

)

## Subset
subdata <- subset(data, col1 > 10 | col2 < 100,
                  select=c(col1, col2, col3))
