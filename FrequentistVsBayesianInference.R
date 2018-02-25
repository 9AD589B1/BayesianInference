rm ( list = ls ( all = TRUE ) )

# We have a population of M&Ms
# The percentage of yellow M&Ms is either 10% or 20%
# You have been hired as a statistical consultant to decide whether the true percentage of yellow M&Ms is 10%
# You are being asked to make a decision, and there are associated payoff/losses that you should consider

# The Data
# you can "buy" a random sample from the population
# you pay $200 for each M&M, and you must buy in $1,000 increments (5 M&Ms at a time)
# you have a total of $4,000 to spend (you may buy 5, 10, 15, or 20 M&Ms)

# Frequentist Inference

# hypotheses: 
# H0: 10% yellow M&Ms
# HA: >10% yellow M&Ms
#
# M&M sample RGYBO
#
# obs. data k = 1, n = 5
#
# sig. level = 0.05
#
# p-value P(K >= 1 | n = 5, p = 0.10)
#

# To calculate the probability of one success, we can calculate this probability as the complement 
# of no successes in five trials. In a sample space with five trials, you could have zero successes, 
# one success, two successes, three successes, four successes or five successes. If you're interested 
# in the number of successes being greater than or equal to one that means that the only outcome 
# that you're not interested in is the number of successes being equal to zero. Hence, the two 
# probabilities, the probability of at least one, and the probability of none are complements of 
# each other. The probability of no successes in five trials with a probability of success for each trial 
# is 0.1 Is 0.90 to the 5th power.

prob <- 1 - 0.9^5

prob

# This is our p-value, so we fail to reject the null hypothesis of there being 10% yellow M&Ms

# Bayesian Inference

# Hypotheses
# H1: 10% yellow M&Ms
# H2: 20% yellow M&Ms

# Priors
PH1 = 0.5
PH2 = 0.5
# sample RGYBO
# Observed Data
k <- 1 # number of successes
n <- 5 # sample size

# Likelihoods

H1Likelihood <- dbinom(k, size = n, prob = 0.1)
H2Likelihood <- dbinom(k, size = n, prob = 0.2)

# Posteriors

H1Posterior <- ( PH1 * H1Likelihood ) / ( ( PH1 * H1Likelihood ) + ( PH2 * H2Likelihood ) )
#Since we assigned only 2 Priors, the H2 Posterior is the complement of the H1 Posterior
H2Posterior <- 1 - H1Posterior
