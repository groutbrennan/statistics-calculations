# Discrete Mean

#################                            Relative Frequency          Weight
# Drinks Ordered per Customer   Frequency   (Frequency / Total Drinks)  (Drinks * Rel Freq)  
# 0                                1                0.025                   0.000
# 1                               22                0.550                   0.550
# 2                               10                0.250                   0.500
# 3                               4                 0.100                   0.300
# 4                               2                 0.050                   0.200
# 5                               1                 0.025                   0.125
# Total Customers               # 40                               # MEAN = 1.68

# Define the data
drinks_data <- c(0, 1, 2, 3, 4, 5)
frequency <- c(1, 22, 10, 4, 2, 1)

total_customers <- sum(frequency)
print(total_customers)
# Answer: 40

relative_frequency <- frequency / total_customers
print(relative_frequency)
# Answer: 0.025 0.550 0.250 0.100 0.050 0.025

weight <- drinks_data * relative_frequency
print(weight)
# Answer: 0.000 0.550 0.500 0.300 0.200 0.125

mean_weight <- round(sum(weight), 2)
print(mean_weight)
# Answer: 1.68

##### Standard Deviation
#################                            
# Drinks Ordered per Customer   Mean              Drinks - Mean           (Drinks - Mean)^2     Relative Frequency    Rel Freq * (Drinks - Mean)^2
# 0                             1.68                  -1.68                      2.82                 0.025                    0.07
# 1                             1.68                  -0.68                      0.46                 0.550                    0.25  
# 2                             1.68                   0.32                      0.10                 0.250                    0.03
# 3                             1.68                   1.32                      1.74                 0.100                    0.17
# 4                             1.68                   2.32                      5.38                 0.050                    0.27
# 5                             1.68                   3.32                      11.02                0.025                    0.28
                                                                                                                  # Variance = 1.07  
                                                                                                        # Standard Deviation = 1.03     
                                                                                                        #   (Variance Square Root)
# Drinks - Mean 
drinks_minus_mean <- round(drinks_data - mean_weight, 2)
print(drinks_minus_mean)
# Answer: -1.68 -0.68  0.32  1.32  2.32  3.32

# (Drinks - Mean)^2
drinks_minus_mean_squared <-round(drinks_minus_mean ^ 2, 2)
print(drinks_minus_mean_squared)
# Answer: 2.82  0.46  0.10  1.74  5.38 11.02

# Rel Freq * (Drinks - Mean)^2
rel_freq_drinks_mean_squared <- round(relative_frequency * drinks_minus_mean_squared, 2)
print(rel_freq_drinks_mean_squared)

# Variance
variance <- sum(round(rel_freq_drinks_mean_squared, 2))
print(variance)
# Answer: 1.07

# Standard Deviation
standard_deviation <- round(sqrt(variance), 2)
print(standard_deviation)
# Answer: 1.03

##### Discrete Range                            (Mean + Number of SD From Mean * Variance)
# Number of Standard Deviation From the Mean                    Number of                           Discrete Range
# 3                                                              4.77                                   4
# 2                                                              3.74                                   3
# 1                                                              2.71                                   2
# 0                                                              1.68
# -1                                                             0.65                                   1
# -2                                                            -0.38                                   0
# -3                                                            -1.41                                   0

# Number of Standard Deviation From the Mean 
number_standard_deviation_from_mean <- c(3, 2, 1, 0, -1, -2, -3)


# (Mean + Number of SD From Mean * Variance)
number_of <- mean_weight + (number_standard_deviation_from_mean * variance)
print(number_of)
# Answer: 4.89  3.82  2.75  1.68  0.61 -0.46 -1.53

# Discrete Range
discrete_range <- ifelse(number_of < 0 | number_of == mean_weight, 0, floor(number_of))
print(discrete_range)
# Answer: 4 3 2 0 0 0 0

