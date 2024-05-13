# Generate Pseudo Random variables

# Define trials
n = 10000

# Sample n observations from standard uniform
x_vector = runif(n, min=0, max=1)

# Define function g()
g <- function(x){
  1-(1-x)^4
}

# Apply function to vector x to return vector of g observations
g_vector <- sapply(x_vector, g)

# Calculate expected value
ev_g = mean(g_vector)

# Calculate variance
var_g = var(g_vector)

# Print the results
cat("Expected value of g(X):", ev_g, "\n")
cat("Variance of g(X):", var_g, "\n")
