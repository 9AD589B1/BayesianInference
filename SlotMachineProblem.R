#You go to Las Vegas and sit down at a slot machine. You are told by a highly reliable source that, 
#for each spin, the probability of hitting the jackpot is either 1 in 1,000 or 1 in 1,000,000, 
#but you have no prior information to tell you which of the two it is. You play ten times, but do 
#not win the jackpot. What is the posterior probability that the true odds of hitting the jackpot are 1 in 1,000?

rm ( list = ls ( all = TRUE ) )

#Priors are set to %50 / %50
PH1 = 0.5
PH2 = 0.5

# Observed Data
k <- 0 # number of successes
n <- 10 # sample size

# Likelihoods

H1Likelihood <- dbinom(k, size = n, prob = 0.001)
H2Likelihood <- dbinom(k, size = n, prob = 0.0001)

# Posteriors

H1Posterior <- ( PH1 * H1Likelihood ) / ( ( PH1 * H1Likelihood ) + ( PH2 * H2Likelihood ) )
#Since we assigned only 2 Priors, the H2 Posterior is the complement of the H1 Posterior
H2Posterior <- 1 - H1Posterior
