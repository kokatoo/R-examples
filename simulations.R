##---- Sampling

N <- 100
flips <- sample(x=c(0,1), prob=c(.5, .5), size=N, replace=T)
cum <- cumsum(flips)
n <- 1:N
probs <- cum / n
plot(n, probs, type="o", log="x")

#----
