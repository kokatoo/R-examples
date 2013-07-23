getwd()

# read in data as dataframe
data <- read.delim("data.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")

# inspect data
head(data)
names(data)
# rename columns
names(data)<-c("col1", "col2", "col3")

# convert to date
data$DateOccurred <- as.Date(data$DateOccurred, format="%Y%m%d")

# remove invalid dates
head(data[which(nchar(data$DateOccurred) != 8)])
good.rows <- ifelse(nchar(data$DateOccurred) != 8, FALSE, TRUE)
length(which(!good.rows))
length(which(good.rows))

# construct matrix from a list
matrix <- do.call(rbind, some_list)
# add matrix to dataframe using transform
data <- transform(data, colnew1=matrix[,1], colnew2=matrix[,2])

# lookup and match
us.states <- c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt", "wa","wi","wv","wy")
data$states <- data$states[match(us.states)]

# subset of data (just put in col name)
data <- subset(data, !is.na(col))




