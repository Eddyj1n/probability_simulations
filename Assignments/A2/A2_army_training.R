#======================================================================
# Probability (MAST20006) Assignment 2
# Question 2: Army training 
# Author: Edward Jin
#=======================================================================


# 1. Analytical solutions to first part -----------------------------------

# Set parameters
n <- 10000
p <- 1/36
k <- 300

# Calculate binomial probability
probability <- pbinom(k, n, p)
print(probability)

# Calculated expected value 
lambda = n*p 

# Calculate comulative poison probability 
poison_probability = ppois(k, lambda)
print(poison_probability)

# Calculate variance of binomial distribution
mu = lambda 
variance = n*p*(1-p)
sigma = sqrt(variance)

# Evaluate normal probability 
normal_probability <- pnorm(k, mu, sigma)
print(normal_probability)

# 2. Simulate the army training experiment  -------------------------------

# Set parameters
n <- 10000
p <- 1/36
k <- 300

# Simulate probability 
set.seed(760532)
num_simulations = 1000

# Store each simulated realisation into a storage vector 
simulated_Y = rbinom(num_simulations, n, p)

# Count the number of realisations that are less than or equal to 300
estimated_probability = sum(simulated_Y <= 300) / num_simulations

print(estimated_probability)
