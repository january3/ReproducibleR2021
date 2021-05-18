rand_data <- rnorm(10)
mtx <- matrix(rand_data, ncol=2, nrow=5)
df <- as.data.frame(mtx)
class(mtx)
class(df)
LETTERS
length(LETTERS)
class(LETTERS)
nrow(df)
ncol(df)
df$ID <- LETTERS[1:nrow(df)] 
class(df$ID)
mtx2 <- data.matrix(df)

as.numeric("a")

2 + 2
`+`(2, 2)

2 + 5 * 6

`+`(2, `*`(5, 6))

samples <- list()
means <- c()
sds <- c()

for(i in 1:100) {
  samples[[i]] <- rnorm(10)
  means[i] <- mean(samples[[i]])
  sds[i] <- sd(samples[[i]])
}

samples <- replicate(100, rnorm(10, mean=1, sd=2))
samples <- as.data.frame(samples)
means   <- sapply(samples, mean)
sds     <- sapply(samples, sd)

v1 <- sample(20:500, size=150)
rand_vecs <- lapply(v1, rnorm)
means <- sapply(rand_vecs, mean)
sds <- sapply(rand_vecs, sd)

plot(v1, means)
lines(1:500, 1/sqrt(1:500), col="red")
lines(1:500, -1/sqrt(1:500), col="red")

days <- 1:10
y <- 1/100 * 2 ^ (days - 1)
plot(days, y, ylim=c(0,1))
abline(h=.5)

pond_surface_covered <- function(day, init_pond_surface=1/100) {
  ## "**" and "^" do the same thing
  ret <- init_pond_surface * 2^(day - 1)
  return(ret)
}

y <- sapply(days, pond_surface_covered)
y <- sapply(days, function(day, init_pond_surface=1/100) {
  ## "**" and "^" do the same thing
  ret <- init_pond_surface * 2^(day - 1)
  return(ret)
})
