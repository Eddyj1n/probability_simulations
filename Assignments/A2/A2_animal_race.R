#======================================================================
# Probability (MAST20006) Assignment 2
# Question 4: Hare and tortoise race 
# Author: Edward Jin
#=======================================================================

# 0. Set Seed -------------------------------------------------------------
set.seed(760532)

# 1. Define function  -----------------------------------------------------

# Create function to simulate race
simulate_animal_race <- function(n, minutes) {
  # Create storage vectors for hare and tortoise displacements
  hare_displacement_vector <- numeric(n)
  tortoise_displacement_vector <- numeric(n)
  
  # Create a variable to store the number of times the hare wins
  hare_wins <- 0
  
  # Repeat experiment (race) n times
  for (j in 1:n) {
    # Initialize variables
    hare_displacement <- 0
    tortoise_displacement <- 0
    current_minute <- 1
    
    # Continue the race until either the hare or tortoise reaches the finish line or 10 minutes have passed
    while (hare_displacement < 10 && tortoise_displacement < 10 && current_minute <= minutes) {
      # Increment hare displacement if hare rolls 1,2,3,4 move 3 steps
      if (sample(1:6, 1) <= 4) {
        hare_displacement <- hare_displacement + 3
      } else {
        # Otherwise, move 3 steps back
        hare_displacement <- hare_displacement - 3
      }
      
      # Increment tortoise displacement
      tortoise_displacement <- tortoise_displacement + 1
      
      current_minute <- current_minute + 1
    }
    
    # Store hare and tortoise displacements in storage vectors
    hare_displacement_vector[j] <- hare_displacement
    tortoise_displacement_vector[j] <- tortoise_displacement
    
    # Check if the hare wins the race and increment the count
    if (hare_displacement >= 10) {
      hare_wins <- hare_wins + 1
    }
  }
  
  # Return the storage vectors and the number of times the hare wins
  return(list(
    hare_displacement = hare_displacement_vector,
    tortoise_displacement = tortoise_displacement_vector,
    hare_wins = hare_wins
  ))
}

# 2. Run function  --------------------------------------------------------

# Set function parameters
minutes = 10 # Set fixed number of minutes per race
n = 100000 # Set number of experiments to run

# Run function
result <- simulate_animal_race(n, minutes)

# Calculate the Hare's EXPECTED displacement after 10 minutes
print(mean(result$hare_displacement))


# 3. Analytical Solution --------------------------------------------------

p <- 4/6 # prob of hare moving forward
q <- 9/20 # prob of owl moving forward

# Analytical solution to hare winning 
p_hare_wins = p^4+4*p^5*(1-p)+15*p^6*(1-p)^2 +56*p^7*(1-p)^3

# Analytical solution of owl winning [BELOW IS WRONG NEED TO USE CATALAN SERIES]
p_owl_wins_within_3_moves = q + (q^2)
p_owl_wins_in_5_moves = 2*(q^3)*((1-q)^2)
p_owl_wins_in_7_moves = 3*(q^4)*((1-q)^3)
p_owl_wins_in_9_moves = 20*(q^5)*((1-q)^4)
p_owl_wins = p_owl_wins_within_3_moves + p_owl_wins_in_5_moves + p_owl_wins_in_7_moves + p_owl_wins_in_9_moves


