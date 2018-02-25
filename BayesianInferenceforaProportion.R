#Research question: Is RU-486 an effective"morning after" contraceptive?
#  -participants: 40 women who came to a health clinic asking for emergency contraception
#  -design: Random assignment to RU-486 or standard therapy (20 in each group)
#  -data:
#   - 4 out of 20 in RU-486 (treatment) became
#     pregnant
#   - 16 out of 20 in standard therapy (control)
#     pregnant
#   - question: How strongly do these data indicate that
#     the treatment is more effective than the control?

#Calculate the posterior of the model where p = 0.2

#Note the Likelihood is defined as the probability of the observed data given the model
#So, this is represented as a binomial distribution of 4 successes in 20 trials, given
#each of our models

#Model (p)                   0.1    0.2    0.3    0.4    0.5    0.6    0.7  0.8  0.9  Total
#Prior, P(model)             0.06   0.06   0.06   0.06   0.52   0.06   0.06 0.06 0.06 1
#Likelihood, P(data | model) 0.0898 0.2182 0.1304 0.035  0.0046 0.0003 0    0    0
#P(data|model) x P(model)    0.0054 0.0131 0.0078 0.0021 0.0024 0      0    0    0    0.0308

options(scipen = 999) #don't display scientific notation
p <- seq (from = 0.1, to = 0.9, by = 0.1) #set models from 0% - 90%
prior <- c(rep(0.06, 4), 0.52, rep(0.06, 4)) #set priors for models so the 50% model has the most weight
likelihood <- c(dbinom(4, size = 20, prob = p))

#For our posterior calculations, the numerator is simply the vector of products of prior probabilities 
#we defined earlier and the likelihood data for the given model we calculated. The denominator is simply 
#the sum of the probabilities for the various models in the numerator. 
#This mimics the calculation based on probability trees
numerator <- prior * likelihood
denominator <- sum (numerator)
posterior <- numerator / denominator

( 0.2182 * 0.0131 ) / denominator


