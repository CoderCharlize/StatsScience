# my DP
n = c(10, 50, 100); y = c(4, 16, 26); #data
a = 2; b = 3 #prior

# plot posterior evolving with n
x = seq(from = 0, to = 1,length.out = 100)
plot(x, dbeta(x, a + y[3], b + n[3] - y[3]), type = "l", col = 3, ylab = "Density of Posterior")
for (i in 1:2) {
  lines(x, dbeta(x, a + y[i], b + n[i] - y[i]), col = i)}
legend("topright", c("n = 10", "n = 50", "n = 100"), col = 1:3, lty = 1)
#dev.off()


# posterior probability
pbeta(0.5, a + y, b + n - y)

# posterior odds
pso = pbeta(0.5, a + y, b + n - y) / (1 - pbeta(0.5, a + y, b + n - y))
pso

# prior odds
pro = (pbeta(0.5, a, b) / (1 - pbeta(0.5, a, b)))
pro

#Bayes Factor
BF=pso/pro
BF


# Bayes Factor using a different prior
a = 2; b = 3; ap = 3; bp = 2 
BF.me.v.you = (beta(a + y, b + n - y) * beta(ap, bp)) / (beta(ap + y, bp + n - y) * beta(a, b))
BF.me.v.you

# check the Bayes Factor calculation using simulation
N=1000000
ml.me = ml.you = rep(0, 3)
# estimate the marginal likelihood by averaging the likelihood in the samples from the prior
for (i in 1:3) {
  ml.me[i] = mean(dbinom(y[i], prob = rbeta(N, a, b), size = n[i]))
  ml.you[i] = mean(dbinom(y[i], prob = rbeta(N, ap, bp), size = n[i]))
}
ml.me / ml.you
# the Bayes factor for my model against theirs is only 2.7 even at n = 100
# so there is no strong evidence in favor of my model over theirs


# sample the two posteriors under the separate models
N=1000000
ps.me = rbeta(N, a + y[1], b + n[1] - y[1])  #first entry as that is n = 10
ps.you = rbeta(N, ap + y[1], bp + n[1] - y[1])

# sample the mixture (for the model average)
# first compute pi_M(m|y) using the formula computed in the text
deno = beta(a + y, b + n - y) / beta(a, b) + beta(ap + y, bp + n - y) / beta(ap, bp)
pm1 = (beta(a + y, b + n - y) / beta(a, b)) / deno
pm1
# check this match the estimate we get using the marginal likelihoods estimated above when we did the checking 
ml.me / (ml.me + ml.you)

pm2 = (beta(ap + y, bp + n - y) / beta(ap, bp)) / deno
pm2
ml.you / (ml.me + ml.you)

# lets do the HPD for the case of n=10 samples
# That means we use the first entry pm1[1] = pi_M(1|y[1]) and 
# pm2[1] = pi_M(2|y[1]), and select samples from ps.me vector with 
# prob pm1[1] and samples from ps.you vector with prob pm2[1]
ps.mix = ps.me                           # create a sample vector that is all ps.me
choose.you = which(runif(N) < pm2[1])    # sample the ones that come from ps.you
ps.mix[choose.you] = ps.you[choose.you]  # write them into our sample vector

#estimate the HPD credible interval
library(coda)
round(HPDinterval(as.mcmc(cbind(ps.me, ps.you, ps.mix))), 2)
