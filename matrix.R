# create 2x3 matrix from vector (columnwise)
mat <- 1:6
dim(mat) <- c(2, 3)

# colwise
mat <- matrix(1:6, nrow=2)
# rowwise
mat <- matrix(1:6, nrow=2, byrow=T)

# naming rows and columns
rownames(mat) <- rownames(mat, prefix="Num.", do.NULL=F)
colnames(mat) <- c("A", "B", "C")
dimnames(mat) <- list(c(1, 2), c("A", "B", "C"))

# summary statistics


