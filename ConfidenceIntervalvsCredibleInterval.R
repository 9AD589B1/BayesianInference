#Consider a simple problem, with Bernoulli trials with insurance claims. 
#We want to derive some confidence interval for the probability to claim a loss. 
#There were n= 1047 policies and 159 claims

rm ( list = ls ( all = TRUE ) )

#Let us generate some samples, of size n, with the same probability as the empirical one, 
#i.e. theta. For each sample, compute the confidence interval with the relationship above. 
#It is a 95% confidence interval because in 95% of the scenarios, 
#the empirical value lies in the confidence interval.

xbar <- 159
n <- 1047
ns <- 100
M = matrix(rbinom(n * ns, size = 1, prob = xbar / n), nrow = n)

#I generate 100 samples of size n. 
#For each sample, I compute the mean, and the confidence interval, from the previous relationship

fIC = function(x)
  mean(x) + c(-1, 1) * 1.96 * sqrt(mean(x) * (1 - mean(x))) / sqrt(n)
IC = t(apply(M, 2, fIC))
MN = apply(M, 2, mean)

# Then we plot each confidence interval, and indicate with red when they do not contain the empirical mean

k = (xbar / n < IC[, 1]) | (xbar / n > IC[, 2])
plot.new()
plot(
  MN,
  1:ns,
  xlim = range(IC),
  axes = FALSE,
  pch = 19,
  cex = .7,
  col = c("blue", "red")[1 + k]
)
axis(1)
segments(IC[, 1], 1:ns, IC[, 2], 1:+ns, col = c("blue", "red")[1 + k])
abline(v = xbar / n)

