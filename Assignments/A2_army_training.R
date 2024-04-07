n <- 10000
p <- 1/36
k <- 300

probability <- pbinom(k, n, p)
print(probability)

# Calculated expected value 
lambda = n*p 

# Calculate comulative poison probability 
poison_probability = ppois(k, lambda)

# Calculate variance of binomial distribution
mu = lambda 
variance = n*p*(1-p)
sigma = sqrt(variance)

# Evaluate normal probability 
normal_probability <- pnorm(k, mu, sigma)

# Simulate probability 
set.seed(760532)
num_simulations <- 1000

simulated_Y <- rbinom(num_simulations, n, p)
estimated_probability <- sum(simulated_Y <= 300) / num_simulations

# Simulate the next probability 
set.seed(760532)

in.service <- function(people) {
  rbinom(1, people, 31/36)
}

num_simulations <- 1000
N.vec <- numeric(num_simulations)

for (i in 1:num_simulations) {
  men <- 10000
  N <- 0
  while (men > 0) {
    men <- in.service(men)
    N <- N + 1
  }
  N.vec[i] <- N
}

estimated_expected_N <- mean(N.vec)
print(paste("Estimated expected value of N:", estimated_expected_N))

plot(table(N.vec) / length(N.vec), xlab = "N", ylab = "Estimated Probability", main = "Estimated Probability Mass Function of N")
