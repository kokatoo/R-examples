set.seed(1)

# Cauchy distribution
# fatter tails than norm
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)
ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

# gamma distribution
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()
