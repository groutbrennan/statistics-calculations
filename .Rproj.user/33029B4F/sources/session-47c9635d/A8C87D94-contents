# Define a function to calculate factorial
factorial <- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

###########################################
######### Permutations ###################
##########################################
# n!/(n-x)!

# Calculate permutations for top 8 players from pool of 50 players.
# 50!/(50-8)!
# 50!/(42)!
# 50!/42!
top_8_permutations <- factorial(50)/factorial(50-8)
print(top_8_permutations)
## Answer: 2.164695e+13

############################################
########## Combination Probabilities 
###########################################
# n!/[(n-x)!*x!]

# Calculate percentage probability that the top 2 players in Flesh and Blood will end up in the same 4 player pod for top 8 players.
# Calculate the pod of 4 possible combinations for top 8 players.
# 8!/[(8-4)!*4!]
# 8!/[4!*4!]
pod_combination <- factorial(8) / (factorial(4) * factorial(4))
## Answer: 70

# Calculate the probability of the 2 top players to end up in the same 4 person pod.
# 8!/[(8-2)!*2!]
# 8!/[6!*2!]
top_player_combination <- factorial(8)/ (factorial(6) * factorial(2))
## Answer: 28

# Calculate the percentage probability that top 2 players will be in the same 4 person pod.
print(top_player_combination/pod_combination)*100
## 40%


####################################################
########### Percentile Rank ########################
#####################################################
# %rank = [# of values below x + 0.5 / total # of values] * 100

# Calculate the percentile rank if you scored a 85% in your class of 20 students with the following scores:
## 75, 90, 95, 100, 75, 90, 95, 95, 75, 85, 95, 95, 70, 85, 90, 95, 55, 80, 90, 95
## There are 6 scores below 85%

# %rank = [6+0.5/20]*100
# %rank = [6.5/20]*100
# %rank = [0.325]*100
# %rank = 32.5%
percent_rank <- ((6+0.5)/20)*100
print(percent_rank)
# Answer: 32.5 percentile