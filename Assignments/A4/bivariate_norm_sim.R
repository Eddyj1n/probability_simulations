# Install and load the MASS package if not already installed
# install.packages("MASS")
library(MASS)

# Set seed
set.seed(760532)

# Set the mean vector and covariance-variance matrix
mean_vector <- c(1, 2)
covariance_matrix <- matrix(c(2, 3, 3, 5), nrow = 2)

# Set the number of simulations
n_simulations <- 100000

# Generate multivariate normal random variables
simulated_data <- mvrnorm(n_simulations, mean_vector, covariance_matrix)

# Calculate P(X_1 > 2)
p_x1_greater_than_2 <- sum(simulated_data[, 1] > 2) / n_simulations
cat("P(X_1 > 2) =", p_x1_greater_than_2, "\n")

# Calculate P(X_1 > 2 | X_2 > 2)
simulated_data_x2_greater_than_2 <- simulated_data[simulated_data[, 2] > 2, ]
p_x1_greater_than_2_given_x2_greater_than_2 <- sum(simulated_data_x2_greater_than_2[, 1] > 2) / nrow(simulated_data_x2_greater_than_2)
cat("P(X_1 > 2 | X_2 > 2) =", p_x1_greater_than_2_given_x2_greater_than_2, "\n")