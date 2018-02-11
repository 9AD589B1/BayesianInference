#‣ research question: Is RU-486 an effective"morning after" contraceptive?
#  ‣ participants: 40 women who came to a health clinic asking for emergency contraception
#  ‣ design: Random assignment to RU-486 or
#    standard therapy (20 in each group)
#  ‣ data:
#   ‣ 4 out of 20 in RU-486 (treatment) became
#     pregnant
#   ‣ 16 out of 20 in standard therapy (control)
#     pregnant
#   ‣ question: How strongly do these data indicate that
#     the treatment is more effective than the control?

#Calculate the posterior of the model where p = 0.2

#Model (p)                   0.1    0.2    0.3    0.4    0.5    0.6    0.7  0.8  0.9  Total
#Prior, P(model)             0.06   0.06   0.06   0.06   0.52   0.06   0.06 0.06 0.06 1
#Likelihood, P(data | model) 0.0898 0.2182 0.1304 0.035  0.0046 0.0003 0    0    0
#P(data|model) x P(model)    0.0054 0.0131 0.0078 0.0021 0.0024 0      0    0    0    0.0308

options(scipen=999)
p <- seq (from = 0.1, to = 0.9, by = 0.1)
prior <- c(rep(0.06, 4), 0.52, rep(0.06, 4))
  likelihood <- c(dbinom(4, size = 20, prob = p))

numerator <- prior * likelihood
denominator <- sum (numerator)
posterior <- numerator / denominator

( 0.2182 * 0.0131 ) / denominator


