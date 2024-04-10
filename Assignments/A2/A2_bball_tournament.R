 #======================================================================
# Probability (MAST20006) Assignment 2
# Question 3: Basketball Tournament Simulation 
# Author: Edward Jin
#=======================================================================

# Set seed as student number 
set.seed(760532)

# 0. Background -----------------------------------------------------------

# The structure of the game is as follows in perspective of team A 
# p | p | p | (1 - p) | (1 - p) | (1 - p) | (1 - p) 

# The structure of the game is as follows in perspective of team B 
# (1 - p) | (1 - p) | (1 - p) | p | p | p | p

# 1. Define function to simulate basketball tournament
simulate_championship <- function(p, n){
  
  # Initialize  parameters 
  A_championships = 0   # Team A wins championship 
  B_championships = 0  # Team B wins championship
  total_A_wins = 0     # Total wins by Team A
  total_B_wins = 0     # Total wins by Team B
  total_games = 0      # Total games played

# Loop through n experiments 
  for (i in 1:n){
    
    # Initialize  parameters 
    A_wins = 0  # Team A wins 
    B_wins = 0  # Team B wins 
    games = 0   # Game number for each experiment 
  
  # Create while loop to terminate the experiment as soon as any team wins 4 games
    while (A_wins < 4 && B_wins < 4) {
      
      # As the probabilities switch partitions in the tournament,
      # Partition 1: First 3 games when A wins with probability p
      if (games < 3) {
        # Team A is the home team for the first 3 games
        # Generate a random number between 0 and 1 and compare it with p
        # If the random number is less than p, Team A wins the game
        
        if (runif(1) <= p) {
          A_wins <- A_wins + 1
        } else {
          B_wins <- B_wins + 1
        }
      } else {
        
        # Partition 2: Next 4 games when A wins with probability (1-p)
        # Team B is the home team for the next 4 games
        if (runif(1) <= (1 - p)) {
          A_wins <- A_wins + 1
        } else {
          B_wins <- B_wins + 1
        }
      }
      # Increment the game number after each game
      games <- games + 1
    }
    
    # Record the championship wins by checking which team reached 4 wins first 
    if (A_wins == 4) {
      A_championships <- A_championships + 1
    } else {
      B_championships <- B_championships + 1
    }
    
    # Record the total number of wins of both team A and B as well as the total games played
    total_A_wins = total_A_wins + A_wins 
    total_B_wins = total_B_wins + B_wins
    total_games = total_games + games
  }

  # Return the championship wins as vector 
  results <- data.frame(
    "Team A Championships" = A_championships,
    "Team B Championships" = B_championships,
    "Total Team A Wins" = total_A_wins,
    "Total Team B Wins" = total_B_wins,
    "Total Games Played" = total_games
  )
  
  return(results)
}

# 2. Run Experiments ------------------------------------------------------

# Set function arguments 
n = 100000 # no of experiments 
p = 0.55 # probability of win in home team 

# Run the function
results=simulate_championship(p, n)

# Calculate expected values and probability 
expected_wins_A = results$Total.Team.A.Wins/n
expected_wins_B = results$Total.Team.B.Wins/n
simulated_prob_A_trophy= results$Team.A.Championships/n
simulated_prob_B_trophy= results$Team.B.Championships/n


  