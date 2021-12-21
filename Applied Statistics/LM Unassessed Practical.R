# Q1
setwd("~/Desktop/MSc/Applied Statistics")
cloud <- read.csv("cloud.seeding.txt", header = TRUE, sep="\t")
head(cloud)
str(cloud)

corr <- cor(cloud)
sort(corr[, "Y"], decreasing = TRUE)
# variables T, SNe, E and C have relatively higher correlation with Y
pairs(cloud, upper.panel = NULL)
# the variable C seems to show a linear trend

# alternatively, call individual plots with Y
plot(Y ~ as.factor(A), data = cloud)
plot(Y ~ as.factor(E), data = cloud)
# I see evidence of different dists of Y in levels of both A and E


# Q2
mT <- lm(Y ~ T, data = cloud.seeding)
coef(mT)
residuals(mT)
fitted.values(mT)
summary(mT)
# no, as the coefficient for T is negative
# there is significant relationship between Y and T, as the p-value is smaller than 5%

mC <- lm(Y ~ C, data = cloud)
summary(mC)

mP <- lm(Y ~ P, data = cloud)
summary(mP)
# neither coefficients is significant

summary(lm(Y ~ log(C), data = cloud))$r.squared
summary(lm(Y ~ I(C^2), data = cloud))$r.squared
summary(lm(Y ~ sqrt(C), data = cloud))$r.squared

summary(lm(log(Y) ~ C, data = cloud))
# there is no applicable difference in model fit

mCPT <- lm(Y ~ C + P + T, data = cloud)
coef(mCPT)

cloud[, "A"] = as.factor(cloud[, "A"])
mCPTA <- lm(Y ~ C + P + T + A, data = cloud)
summary(mCPTA)

summary(lm(Y ~ A * (C + P + T), data = cloud))


# Q3
# standard four diagonostic plots
n <- dim(cloud)[1]
p <- dim(cloud)[2]

par(mfrow = c(2, 2))

#plot(mCPT)

qqnorm(rstudent(mCPTA), main = NULL)
qqline(rstudent(mCPTA))

plot(fitted.values(mCPTA), rstudent(mCPTA))
text(fitted.values(mCPTA), rstudent(mCPTA), adj = -0.2)

plot(hatvalues(mCPTA), ylim = c(0, 1))
text(hatvalues(mCPTA), row.names(cloud), srt = 90, adj = -0.1)
abline(2*p/n, 0, lty = 2)

plot(cooks.distance(mCPTA), ylim = c(-0.2, 1.5))
text(cooks.distance(mCPTA), row.names(cloud), srt = 90, adj = 1.1)
abline(8/(n - 2*p), 0, lty = 2)

# remove observations 1, 2, 15 based on Cook's distance, Residuals, RSS, R^2
mCPT2 = lm(Y ~ C + P + T, data = cloud[-c(1, 2, 15), ])
summary(mCPT2)
