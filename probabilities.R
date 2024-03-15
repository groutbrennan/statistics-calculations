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

###################################################
########### Percentile Rank #######################
###################################################
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

##########################################
########### Multiplication Rule ##########
##########################################

########
## Independent Events
########
# Find the probability that two dice come up "one" when rolled.
# Probability of the first dice coming up "one".
first_dice <- 1/6
## 0.16
## 16%
second_dice <- 1/6
## 0.16
## 16%
both_dice <- (first_dice * second_dice) * 100
print(both_dice)
## 2.79%

########
## Dependent Events
#########
# What are the odds of picking two out of the three "Sigil of Solace" fab cards from your deck of 60 cards.

## Odds of picking the first "Sigil of Solace"
first_sigil <- 3/60
## 0.05
## 5%
second_sigil <- 2/59
## 0.033
## 3.3%
two_sigils <- (first_sigil * second_sigil) * 100
print(two_sigils)
## 0.16%

#########################
##### Bayes Theorem #####
#########################
# P(A|B) = P(B|A) P(A) / P(B|A) P(A)

# millionaires               % of total Millionaires                Inherited their wealth
# High School Drop-out                 0.05                                 0.40
# High School Diploma                  0.15                                 0.20
# Undergraduate degree                 0.60                                 0.20
# Graduate degree                      0.20                                 0.15

# P(A|B) = (PROB DROP) (PROB DROPOUT EARN) / (PROB DROPOUT EARN) + (PROB HS EARN) + (PROB UG EARN) + (PROB GRAD EARN)

# Calculate hs_dropout_prob_earned
hs_dropout_prob_earned <- (0.05 * 0.60) / ((0.05 * 0.60) + (0.15 * 0.80) + (0.60 * 0.80) + (0.15 * 0.85))

# Print the result
print(hs_dropout_prob_earned)
# Answer: 0.039
#         3.9%


# Flesh and Blood Meta Breakdown

# hero        %_of_total_meta         wins
# Dromai        0.40                  0.20
# Kayo          0.20                  0.30
# Victor        0.20                  0.20
# Katsu         0.10                  0.20
# Kassai        0.10                  0.10

# Calculate probability of Dromai winning the pro tour with this current meta.
dromai_win_prob <- (0.40 * 0.20) / ((0.40 * 0.20) + (0.20 * 0.30) + (0.20 * 0.20) + (0.10 * 0.20) + (0.10 * 0.10))
print(dromai_win_prob)
# Answer: 0.38
#         38%

## Define meta array by %_of_total and wins
# Create arrays for each hero's percentage of total meta and wins
dromai <- c(0.40, 0.20)
kayo <- c(0.20, 0.30)
victor <- c(0.20, 0.20)
katsu <- c(0.10, 0.20)
kassai <- c(0.10, 0.10)

dromai_win_probability <- (dromai[1] * dromai[2]) / ((dromai[1] * dromai[2]) + (kayo[1] * kayo[2]) + (victor[1] * victor[2]) + (katsu[1] * katsu[2]) + (kassai[1] * kassai[2]))
print(dromai_win_probability)
# Answer: 0.38
#         38%

### More elegant way to find the probability of Dromai winning the meta in the pro tour.
# Define hero data as a list of vectors
heroes <- list(
  dromai = c(0.40, 0.20),
  kayo = c(0.20, 0.30),
  victor = c(0.20, 0.20),
  katsu = c(0.10, 0.20),
  kassai = c(0.10, 0.10)
)

# Calculate dromai's win probability. 
# Iterate through the heroes list vector, using dromai values as the numerator and the sum of heroes values as the denominator.
dromai_win_probability <- with(heroes, {
  num <- dromai[1] * dromai[2]
  denom <- sum(sapply(heroes, function(hero) hero[1] * hero[2]))
  num / denom
})

# Print the result
print(dromai_win_probability)
# Answer: 0.38
#         38%

# Kayo win probability.
kayo_win_probability <- with(heroes, {
  num <- kayo[1] * kayo[2]
  denom <- sum(sapply(heroes, function(hero) hero[1] * hero[2]))
  num / denom
})
print(kayo_win_probability)
# Answer: 0.29
#         29%



# Define hero data as a list of vectors
heroes <- list(
  dromai = c(0.40, 0.20),
  kayo = c(0.20, 0.30),
  victor = c(0.20, 0.20),
  katsu = c(0.10, 0.20),
  kassai = c(0.10, 0.10)
)

# Function to calculate win probability for a given hero
calculate_win_probability <- function(hero_data, all_heroes) {
  num <- hero_data[1] * hero_data[2]
  denom <- sum(sapply(all_heroes, function(hero) hero[1] * hero[2]))
  return(num / denom)
}

# Print the results for each hero's win probability
for (hero_name in names(heroes)) {
  win_prob <- calculate_win_probability(heroes[[hero_name]], heroes)
  cat(hero_name, "win probability:", win_prob)
}





