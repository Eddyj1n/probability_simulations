#======================================================================
# Probability (MAST20006) Assignment 2
# Question 4: Hare and tortoise race 
# Author: Edward Jin
#=======================================================================

# 0. Set Seed -------------------------------------------------------------
set.seed(760532)

# 1. Define function  -----------------------------------------------------

# Create function to simulate race
simulate_animal_race <- function(n, minutes){
  
  # Create storage vectors for hare and tortoise displacements
  hare_displacement_vector = numeric(n)
  tortoise_displacement_vector = numeric(n)
  
  # Repeat experiment (race) n times
  for ( j in 1:n){
    
    #Initialize variables
    hare_displacement = 0
    tortoise_displacement = 0
    current_minute = 1
    
    # Continue the race until either the hare or tortoise reaches the finish line or 10 minutes have passed
    while (hare_displacement < 10 && tortoise_displacement < 10 && current_minute <= minutes) {
      
      # Increment hare displacement if hare rolls 1,2,3,4 move 3 steps
      if (sample(1:6,1)<=4){
        hare_displacement = hare_displacement + 3
        
        # Otherwise, move 3 steps back
      } else {
        hare_displacement = hare_displacement - 3
      }
      
      # Increment tortoise displacement
      tortoise_displacement = tortoise_displacement + 1
      
      current_minute = current_minute + 1
    }
    
    # Store hare and tortoise displacements in storage vectors
    hare_displacement_vector[j] = hare_displacement
    tortoise_displacement_vector[j] = tortoise_displacement
  }
  
  # Return the storage vectors
  return(list(hare_displacement = hare_displacement_vector, 
              tortoise_displacement = tortoise_displacement_vector))
}

# 2. Run function  --------------------------------------------------------

# Set function parameters
minutes = 10 # Set fixed number of minutes per race
n = 1000000 # Set number of experiments to run

# Run function
result <- simulate_animal_race(n, minutes)

# Calculate the Hare's EXPECTED displacement after 10 minutes
print(mean(result$hare_displacement))



# 3. Analytical Solution --------------------------------------------------

p <- 4/6
q <- 1 - p
d <- 3

expected_displacement <- 0

# Cases where the hare reaches the finish line within 10 steps (using negative binomial)
for (n in 4:10) {
  probability <- choose(n-1, 3) * p^4 * q^(n-4)
  displacement <- (4*d - (n-4)*d) * probability
  expected_displacement <- expected_displacement + displacement
}

# Cases where the hare doesn't reach the finish line within 10 steps
for (k in 0:3) {
  probability <- choose(10, k) * p^k * q^(10-k)
  displacement <- (k*d - (10-k)*d) * probability
  expected_displacement <- expected_displacement + displacement
}

print(expected_displacement)





