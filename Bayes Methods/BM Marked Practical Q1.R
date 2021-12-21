#WEEK 4 BAYES METHODS NON-ASSESSED QUESTION
#
#Inspired by an example in the book BDA 3ed. The model for draws might seem odd but it does make sense.
#See "On Extending the Bradley-Terry Model to Accommodate Ties in Paired Comparison Experiments"
#Roger R. Davidson, Journal of the American Statistical Association, Vol. 65, No. 329 (Mar., 1970), pp. 317-328 

library(MCMCpack)
library(lattice)


# Load the data
cn.dat <- read.table("http://www.stats.ox.ac.uk/~nicholls/BayesMethods/candidates.txt",header=TRUE)
str(cn.dat)
cn.names <- levels(as.factor(cn.dat$white))
num.play <- length(cn.names)


# calculate the function p_{i,j,y} giving the probability to get y = 1, 2, 3 when i plays j with i moving first
pw <- function(lambda, gamma, delta, i, j, r) {
  den = exp(lambda[i]) + exp(lambda[j] + gamma) + exp(delta + (lambda[i] + lambda[j] + gamma) / 2)
  prob = (r == 1) * exp(lambda[i]) / den +
    (r == 2) * exp(lambda[j] + gamma) / den +
    (r == 3) * exp(delta+(lambda[i] + lambda[j] + gamma) / 2) / den
  if (any(prob == 0)) stop('some results r not recognised in pw()')
  return(prob)
}
# Remark: I had some difficulty getting this to be numerically stable. 
# I would need to remove a large constant from the sum of exponentials
# and work on a log scale.

# check it works
pw(lambda = rep(0, num.play), gamma = 0, delta = 0, i <- c(1,3,5), j <- c(2,4,6), r <- c(1,2,3))


# prior ellicitation by simulating possible priors and checking
# 1) non informative with respect to advantage for white
# 2) probability of draw is larger than white or black win
# 3) probabilities range over moderate values as players have roughly equal skills

K = 1000
out = matrix(NA, K, 3)
for (k in 1:K) {
  lambda = rt(num.play, df = 5)  # on log-odds scale, equal players, uncertain outcomes, no extreme values
  gamma = rt(1, df = 5)          # subtract -1ish here weights white win, don’t as informative of hypothesis
  delta = rt(1, df = 5) + 2      # favor draws than white or black win
  out[k, ] = pw(lambda, gamma, delta, c(1,1,1), c(2,2,2), c(1,2,3))
}
# plot the  p_{i,j,y} for y = 1, 2, 3
plot(as.mcmc(out)) 
# interpretation based largely on thinking carefully about histograms
# rows of out[] should sum to one - check apply(out,1,sum) gives ones
head(out)
plot(density(out[, 1]))
lines(density(out[, 2]),col=2)
lines(density(out[, 3]),col=3)

# convert names to numbers from problem statement: i_k = cn.dat$wi[k] and j_k = cn.dat$bi[k]
cn.dat$wi = as.numeric(as.factor(cn.dat$white))
cn.dat$bi = as.numeric(as.factor(cn.dat$black))
head(cn.dat)

llk <- function(lambda, gamma, delta) {
  # bradley terry log likelihood allowing for draws
  return(sum(log(pw(lambda, gamma, delta, cn.dat$wi, cn.dat$bi, cn.dat$result))))
}

lpr <- function(lambda, gamma, delta) {
  # log prior from simulation and elicitation
  log.prior = sum(dt(lambda, df = 5, log = TRUE)) + dt(gamma, df = 5, log = TRUE) + dt(delta - 2, df = 5, log = TRUE)
  return(log.prior)
}

# initialise state for MCMC
lambda = rep(0, num.play); gamma = delta = 0;
oll = llk(lambda,gamma,delta); oll
olp = lpr(lambda,gamma,delta); olp

# usual run parameters
K = 100000; SS = 100; # SS for sub-sampling
out.mcmc = matrix(NA, K / SS, num.play + 2 + 2); 
lami = 1:num.play; gi = 9; di = 10; lpi = 11:12
colnames(out.mcmc) = c(cn.names,'gamma','delta','olp','oll')
for (k in 1:K) {
  # proposal distribution is the prior so the MHR is L(theta';y) / L(theta;y) (ratio of posteriors)
  # works here as little information in data (or prior matches likelihood closely)
  # proposal dists:
  lambdap = rt(num.play, df = 5)
  gammap = rt(1, df = 5)
  deltap = rt(1, df = 5) + 2
  nll = llk(lambdap,gammap,deltap)
  if (log(runif(1)) < nll - oll) {  # log scale - note prior cancels with proposal (so ratio of posterior is the ratio of likelihood)
    lambda = lambdap; gamma = gammap; delta = deltap;
    oll = nll
    olp = lpr(lambda, gamma, delta)
  }
  if (k %% SS == 0) {
    out.mcmc[k / SS, ] = c(lambda, gamma, delta, olp, oll)
  }
}

# standard convergence analysis for both runs - convergence looks just acceptable
xyplot(out.mcmc_trace <- as.mcmc(out.mcmc[,1:4])) # sticks in a state a bit, consider pushing up run length and sub-sampling
levelplot(out.mcmc, scales = list(x = list(rot = 45)))  # correlation can be a problem
splom(as.matrix(out.mcmc[, c(gi,di,lpi)]))  # some of the more strongly correlated cases
effectiveSize(out.mcmc)  # these could be bigger closer to 1000
par(mfrow=c(3,4),oma=c(1,1,1,1));                   # acf plots
for (i in 1:dim(out.mcmc)[2]) {
  par(mai = 0.2 * c(1,1,1,1)); 
  plot(acf(out.mcmc[, i], lag.max = 200, plot = F), type = 'l', ann = F, xaxp = c(0, 200, 2), yaxp = c(0, 1, 1)); 
  text(50, 0.8, colnames(out.mcmc)[i])
}
# reasonable convergence, not perfect
# extra thing I haven't done but should do is overlay histograms/densities from 
# multiple independent runs and check estimated post dist is stable

# answer questions posed on the data
# QUESTION A) Is there a white advantage? White advantage is evidenced by gamma < 0
p.WA = mean(out.mcmc[, gi] < 0) # posterior probability for gamma < 0
p.WA / (1 - p.WA)              # posterior odds for gamma < 0 is big >> 10, strong evidence for a white advantage



# QUESTION B - is there any evidence for significant strength variation lambda_i > lambda_j
# pairwise comparison of player strength measures lambda
ibj = matrix(NA, num.play, num.play)
for (i in 1:num.play) {
  for (j in 1:num.play) {
    ibj[i,j] = mean(out.mcmc[,i] > out.mcmc[,j])
  }
}
# compute posterior odds for lambda[i] > lambda[j]
colnames(ibj) = rownames(ibj) = cn.names
round(ibj / (1 - ibj), 2) # Topalov had a poor tournament (most entries > 10)

# Missing from this discussion is goodness of fit - one obvious possible
# problem is serial correlation of outcomes as the tournament goes on

# Closing remarks: the posterior was rather diffuse so there may be some sensitivity
# to the choice of prior - adjust prior and check results stable.

# QUESTION C) Is there a trend toward greater risk taking (for eg smaller delta) 
# as the tournament goes on

# One straightforward approach is to split the data in two (rounds 1-7 and 8-14)
# and look for evidence of unequal delta or more generally replace delta with delta_k 
# in round k and set delta_k = delta * exp(f(k) * beta) with say f(k) = k 
# priors delta ~ t(df = 5) + 2 as before and beta ~ N(0,1) as before

# More sophisticated we could replace delta with delta_k for round k
# and consider a random walk prior that starts at the same value as before delta_1 ~ t(df = 5) + 2
# but then drifts away with normal increments: delta_{k + 1} = delta_k + epsilon with epsilon ~ N(v, sigma ^ 2)
# In the first instance I would fix sigma to a small (0.1 say) value
# The prior on v, delta_1,..., delta_K for K rounds would be something like 
# pi(delta) = dt(delta_1 - 2, df=5) * prod_k N(delta_{k+1} - delta_k - v) 
# pi(v) = N(1/K,(1/K)^2) - because there are only 4 games per round this is probably overparameterised,
# but Bayesian inference differs from Frequentist inference in this respect. More heavilly
# parameterised models may be useful if the prior is carefully constructed
