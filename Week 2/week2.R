################################################################################
# Week 2 Lab Reference Code
# Charles Z. Liu
################################################################################


# Load library
library(ggplot2)
library(dplyr)

# Read Data
customer_data <- read.csv("customer_data.csv", stringsAsFactors = FALSE)
str(customer_data)

# ggplot scatter
ggplot(customer_data, aes(x = Age, y = Salary)) +
  geom_point() +
  theme_minimal()


# Histogram
ggplot(customer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, 
                 fill = "skyblue", 
                 color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count") +
  theme_minimal()



# Prior probability of spam
P_spam <- 0.2
# Probability of "win" given spam
P_word_given_spam <- 0.7
# Probability of "win" given not spam
P_word_given_not <- 0.1
# Posterior probability using Bayes
P_spam_given_word <- (P_word_given_spam * P_spam) /
  (P_word_given_spam * P_spam + P_word_given_not * (1 - P_spam))
P_spam_given_word


# Bayesian Updating
# Beta-Binomial example
prior_alpha <- 2
prior_beta <- 2
observed_success <- 5
observed_fail <- 3
posterior_alpha <- prior_alpha + observed_success
posterior_beta <- prior_beta + observed_fail
posterior_alpha / (posterior_alpha + posterior_beta)

#########################################################

# Test Lucky Coin
# --- Step 1: Initialization ---
# Initial Prior Probability: The probability that you think the coin is biased
prior <- 0.1
# Likelihoods
p_heads_if_rigged <- 1.0 # If biased, it must be heads
p_heads_if_fair <- 0.5 # If fair, 50% probability of heads

# Create a vector to store the posterior probability after each toss
results <- numeric(10)

# --- Simulate 10 tosses (all heads) ---
for (i in 1:10) {
  # Steps 2 & 3: Calculate the posterior probability using Bayes' theorem
  # P(Rigged|H) = [P(H|Rigged) * P(Rigged)] / P(H)
  # Where P(H) = P(H|Rigged)*P(Rigged) + P(H|Fair)*P(Fair)
  numerator <- p_heads_if_rigged * prior
  denominator <- (p_heads_if_rigged * prior) + (p_heads_if_fair * (1 - prior))
  posterior <- numerator / denominator
  
  # Record results
  results[i] <- posterior
  # Key point: Update the posterior probability of this time to the prior probability calculated next time (New Prior)
  prior <- posterior
  cat(sprintf("Throwing %d times (Heads): Posterior probability = %.4f\n", i, posterior))
  
}
# --- Visualization (Step 4: Long-term trend) ---
plot(1:10, results, type="b", pch=19, col="blue",
     
     xlab="Number of consecutive heads tosses", ylab="Probability that the coin is biased (Posterior)", main="Bayesian Belief Update Process")
abline(h=0.99, col="red", lty=2) # Mark the 0.99 threshold

#ER Model
install.packages("igraph")
library(igraph)
n <- 50
p <- 0.1
g <- erdos.renyi.game(n, p, directed = FALSE)
plot(g)

## Statistic Feature of ER Model
n <- 10
p <- 0.3
m <- choose(n, 2)
m * p
m * p * (1-p)
