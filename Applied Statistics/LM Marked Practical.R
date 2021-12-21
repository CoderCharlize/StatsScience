options(digits = 5)
library(MASS) # for boxcox()

setwd("~/desktop/MSc/Practicals/LM Practical/LM Marked Practical")
swim <- read.csv("swim.csv", fileEncoding = "latin1")
str(swim)

cblue <- "#0077BB"
ccyan <- "#33BBEE"
cteal <- "#009988"
corange <- "#EE7733"
cred <- "#CC3311"
cmagenta <- "#EE3377"
cgrey <- "#BBBBBB"
ccol <- c(cblue, ccyan, cteal, corange, cred, cmagenta, cgrey)
palette(ccol)

# gender colours
swim$gcol[swim$sex == "F"] <- ccol[1]
swim$gcol[swim$sex == "M"] <- ccol[2]

# stroke colours
swim$scol[swim$stroke == "Freestyle"] <- ccol[3]
swim$scol[swim$stroke == "Butterfly"] <- ccol[4]
swim$scol[swim$stroke == "Backstroke"] <- ccol[5]
swim$scol[swim$stroke == "Breaststroke"] <- ccol[6]
swim$scol[swim$stroke == "Medley"] <- ccol[7]

m50 <- mean(swim$time[swim$dist == 50])
m100 <- mean(swim$time[swim$dist == 100])
m200 <- mean(swim$time[swim$dist == 200])
m400 <- mean(swim$time[swim$dist == 400])
swim$dev <- swim$time
swim$dev[swim$dist == 50] <- swim$dev[swim$dist == 50] - m50
swim$dev[swim$dist == 100] <- swim$dev[swim$dist == 100] - m100
swim$dev[swim$dist == 200] <- swim$dev[swim$dist == 200] - m200
swim$dev[swim$dist == 400] <- swim$dev[swim$dist == 400] - m400

# strk - order the strokes in a better way, from fastest to slowest, and then medley
strkvec <- c("Freestyle", "Butterfly", "Backstroke", "Breaststroke", "Medley")
swim$strk <- factor(swim$stroke, levels = strkvec)

# centre log(dist) at log(50) for interpretation
swim$logd50 <- log(swim$dist) - log(50)

# -----

# numerical exploration:
# - times increase with distance, and increase faster than linearly,
#   e.g. like d^alpha, where alpha > 1 (alpha might depend on sex/course/stroke)
# - men's times are faster than women's (we'd like to estimate how much faster)
# - short course times are faster than long course (more push-offs at end of pool
#   as twice as many lengths in a short course race)
# - could tabulate winning, or average, times for various events,
#   e.g. all events have 200m versions, could tabluate just these
# - freestyle definitely the fastest, breaststroke definitely the slowest,
#   for butterfly/backstroke/medley it depends, e.g. short butterfly races
#   sometimes faster than short backstroke, whereas long butterfly sometimes slower
#   than long backstroke - illustrated in the first 2x2 figure below

par(mfrow = c(1, 1))
plot(time ~ dist, data = swim, pch = 16, col = gcol)
legend("topleft", c("Female", "Male"), col = 1:2, pch = 16)
# variance increasing with distance

plot(log(time) ~ log(dist), data = swim, pch = 16, col = gcol)
legend("topleft", c("Female", "Male"), col = 1:2, pch = 16)

# pdf("plot1.pdf", width = 9, height = 9)
par(mfrow = c(2, 2))
with(swim[swim$sex == "F" & (swim$course == "Long"), ], {
  plot(dev ~ jitter(dist), pch = 16, col = scol, ylim = c(-35, 35),
       main = "Women, Long course")
  legend("bottomleft", rev(strkvec), col = 7:3, pch = 16)
})

with(swim[swim$sex == "M" & (swim$course == "Long"), ], {
  plot(dev ~ jitter(dist), pch = 16, col = scol, ylim = c(-35, 35),
       main = "Men, Long course")
  legend("bottomleft", rev(strkvec), col = 7:3, pch = 16)
})

with(swim[swim$sex == "F" & (swim$course == "Short"), ], {
  plot(dev ~ jitter(dist), pch = 16, col = scol, ylim = c(-35, 35),
       main = "Women, Short course")
  legend("bottomleft", rev(strkvec), col = 7:3, pch = 16)
})

with(swim[swim$sex == "M" & (swim$course == "Short"), ], {
  plot(dev ~ jitter(dist), pch = 16, col = scol, ylim = c(-35, 35),
       main = "Men, Short course")
  legend("bottomleft", rev(strkvec), col = 7:3, pch = 16)
})
# dev.off()

# the 2x2 plot above shows the effect of F/M and Long/Short,
# as well as the effect of different strokes
# - each dev ("deviation") is the time centered by subtracting
#   the mean for all races of that length
# - so moving top row to bottom, observe that devation values decrease,
#   short course times are faster
# - moving left column to right, devations also decrease (men's times faster)
#
# and e.g. women, long course: butterfly faster than backstroke at 100m,
# but these two strokes virtually the same at 200m

# -----

par(mfrow = c(1, 2))
plot(time ~ dist, data = swim)
mod <- lm(time ~ dist, data = swim)
abline(mod)

plot(log(time) ~ log(dist), data = swim)
mod <- lm(log(time) ~ log(dist), data = swim)
abline(mod)
# dist is very skewed, so take the log
# take both logs like a multiplicative model

swim.lm0 <- lm(time ~ (dist + sex + stroke + course)^2, data = swim)
par(mfrow = c(2, 2))
plot(swim.lm0)
# variance increasing with fitted value, as anticipated

par(mfrow = c(1, 1))
boxcox(time ~ log(dist) * sex * strk * course, data = swim,
       lambda = seq(-0.5, 0.5, length.out = 101))
# the preferred lamdba is not 0, i.e. not quite log(time),
# and log isn't in the confidence interval either, but log(time) is easiest to interpret

swim.lm1 <- lm(log(time) ~ log(dist) + sex + strk + course, data = swim)
par(mfrow = c(2, 2))
plot(swim.lm1)
anova(swim.lm1)
         
swim.lm2 <- lm(log(time) ~ (log(dist) + sex + strk + course)^2, data = swim)
plot(swim.lm2)
anova(swim.lm2)

swim.lm3 <- lm(log(time) ~ log(dist) + sex + stroke + course
               + log(dist):sex + log(dist):strk + sex:strk + sex:course + strk:course,
               data = swim)
plot(swim.lm3)
anova(swim.lm3)

# AIC
# step(swim.lm2) # gives swim.lm3

# BIC
# step(swim.lm2, k = log(446)) # omits one further interaction

# swim.lm3
# main effects: i.e. dist, sex, stroke, course all affect time (of course)
# interactions:
# log(dist):sex, the effect of distance is different for F/M
# log(dist):strk, the effect of distance is different for different strokes
# sex:strk, the effect of stroke is different for F/M
# sex:course, the effect of course is different for F/M
# strk:course, the effect of course is different for different strokes

# summary(swim.lm3)
# is not what I want to use
# instead use logd50 which gives meaningful interpretations to the overall intercept,
# and to the intercept-offsets for the categorical variables (sex and strk) that have
# interactions with distance

swim.lm4 <- lm(log(time) ~ logd50 + sex + strk + course
               + logd50:sex + logd50:strk + sex:strk + sex:course + strk:course,
               data = swim)
anova(swim.lm4)
summary(swim.lm4)

# swim.lm4a <- lm(log(time) ~ logd50 + sex + strk + course, data = swim)
# summary(swim.lm4a)

coef(swim.lm4)

# For a female swimmer, swimming freestyle, long course, 50 m race,
# the expected time is exp(3.193) = 24.4 seconds
# and taking the above as the baseline race:
# - if race length doubles, time is multiplied by 2^1.11
# - for a male swimmer, multiply time by exp(-0.105)
# - for butterfly multiply time by exp(0.05), etc for other strokes
# - for short course multiply time by exp(-0.011)
# - if dist doubles and a male swimmer, increase the factor to 2^(1.11+0.00985)
# - if dist doubles and stroke is butterfly, increase by
#   factor of exp(0.05) * 2^(1.11+0.0388)
# - for a male swimmer, butterfly, multiply time by exp(-0.105+0.05-0.008)
# etc
#
# evaluate some of these and discuss, e.g:
# exp(-0.105) = 0.9, the male time for the baseline race is 90% of that for female,
# and since male interaction coeffs are small in magnitutde compared to 0.9,
# most male times are estimated as about 90% of female times
#
# the effect of distance is like dist^1.11 for the baseline race,
# but if the stroke is changed to butterfly, this changes to d^(1.11+0.0388),
# indicating a faster increase, presumably butterfly is more fatiguing
#
# the multiplying effects of the individual strokes, at the baseline race, are
# freestyle exp(0)=1, butterfly exp(0.05)=1.05, backstroke exp(0.106)=1.11,
# breaststroke exp(0.226)=1.25, medley exp(0.149)=1.16
# i.e. the ordering of strokes expected from original analysis,
# now with estimates of how much slower they are than freestyle (for the baseline race)
#
# maybe not straighforward to understand the consequences of all the coefficient values,
# hence the suggestion of illusrating results with one or two plots (see below)
# and describing/discussing what the plots show
# - plots can show more, and more concisely, than can be described in a few sentences

# -----

# illustrative plots
#
# note that not all races exist at all distances/courses
# e.g. only freestyle/medley exist at 400m, simlarly only some 50m events exist
# so drawing the plots as below involves some extrapolation
# - some extrapolation may be reasonable, e.g. 50m long course backstroke
# - some may not, e.g. 400m butterfly/breaststroke 

# pdf("plot2.pdf", width = 9, height = 9)
par(mfrow = c(2, 2))
idx <- c(1, 6, 11, 16)
with(swim[swim$sex == "F" & (swim$course == "Long"), ], {
  plot(time ~ jitter(dist), pch = 16, col = scol,
       xlim = c(0, 420), ylim = c(0, 300), main = "Women, Long course")
  legend("topleft", rev(strkvec), col = 7:3, pch = 16, lty = 1)
  
  newdata <- data.frame(dist = rep(c(50, 100, 200, 400), each = 5), strk = rep(strkvec, 4),
                        sex = rep("F", 20), course = rep("Long", 20))
  newy <- exp(predict(swim.lm3, newdata))
  for (j in 0:4) {
    lines(newdata$dist[idx], newy[idx+j], col = ccol[3+j])
  }
})
with(swim[swim$sex == "M" & (swim$course == "Long"), ], {
  plot(time ~ jitter(dist), pch = 16, col = scol,
       xlim = c(0, 420), ylim = c(0, 300), main = "Men, Long course")
  legend("topleft", rev(strkvec), col = 7:3, pch = 16, lty = 1)
  
  newdata <- data.frame(dist = rep(c(50, 100, 200, 400), each = 5), strk = rep(strkvec, 4),
                        sex = rep("M", 20), course = rep("Long", 20))
  newy <- exp(predict(swim.lm3, newdata))
  for (j in 0:4) {
    lines(newdata$dist[idx], newy[idx+j], col = ccol[3+j])
  }
})
with(swim[swim$sex == "F" & (swim$course == "Short"), ], {
  plot(time ~ jitter(dist), pch = 16, col = scol,
       xlim = c(0, 420), ylim = c(0, 300), main = "Women, Short course")
  legend("topleft", rev(strkvec), col = 7:3, pch = 16, lty = 1)
  
  newdata <- data.frame(dist = rep(c(50, 100, 200, 400), each = 5), strk = rep(strkvec, 4),
                        sex = rep("F", 20), course = rep("Short", 20))
  newy <- exp(predict(swim.lm3, newdata))
  for (j in 0:4) {
    lines(newdata$dist[idx], newy[idx+j], col = ccol[3+j])
  }
})
with(swim[swim$sex == "M" & (swim$course == "Short"), ], {
  plot(time ~ jitter(dist), pch = 16, col = scol,
      xlim = c(0, 420), ylim = c(0, 300), main = "Men, Short course")
  legend("topleft", rev(strkvec), col = 7:3, pch = 16, lty = 1)
  
  newdata <- data.frame(dist = rep(c(50, 100, 200, 400), each = 5), strk = rep(strkvec, 4),
                        sex = rep("M", 20), course = rep("Short", 20))
  newy <- exp(predict(swim.lm3, newdata))
  for (j in 0:4) {
    lines(newdata$dist[idx], newy[idx+j], col = ccol[3+j])
  }
})
# dev.off()

# zoom in to see what's going on in more detail
# pdf("plot3.pdf", width = 9, height = 9)
par(mfrow = c(1, 1))
with(swim[swim$sex == "F" & (swim$course == "Short"), ], {
  plot(time ~ jitter(dist), pch = 16, col = scol,
       xlim = c(80, 220), ylim = c(50, 150), main = "Women, Short course")
  legend("topleft", rev(strkvec), col = 7:3, pch = 16, lty = 1, lwd = 2)
  
  newdata <- data.frame(dist = rep(c(50, 100, 200, 400), each = 5), strk = rep(strkvec, 4),
                        sex = rep("F", 20), course = rep("Short", 20))
  newy <- exp(predict(swim.lm3, newdata))
  for (j in 0:4) {
    lines(newdata$dist[idx], newy[idx+j], col = ccol[3+j], lwd = 2)
  }
})
# dev.off()

# could also try these plots with
# swim.lm3 <- lm(log(time) ~ log(dist) + sex + strk + course, data = swim)

# -----

# predictions

new <- data.frame(dist = c(400, 50, 400, 100),
                  strk = c("Freestyle", "Backstroke", "Butterfly", "Medley"),
                  sex = rep("F", 4),
                  course = rep("Long", 4))

exp(predict(swim.lm3, newdata = new, interval = "prediction"))

# RaceA - is ok for prediction: 400m long course, freestyle for women is a standard race
# B - involves some extrapolation, but maybe reasonable extrapolation as 50m short course
#     backstroke races do exist, and we've estimated the difference between short & long
# C - 400m butterfly races don't exist, butterfly times increase the fastest with
#     distance, fatigue is surely a factor, hence predicting a 400m race when 200m is
#     longest that actually exists seems inappropriate extrapolation
# D - we know that medley races are equal number of lengths of the four strokes,
#     this is not possible to have in a long course race of only 100m, so involves
#     extrapolation to experimental conditions that can't possibly exist - not appropriate
#
# we might also wonder what a "prediction" means here - if we are thinking of
# a future Olympics/championship then, as athletic standards are (almost) always
# improving, times in say 2020 are likely to be lower than in 2016 and we have
# no data to use to make allowance for this 
# - maybe we think of it being the average time, in a final, in 2016,
#   for similar elite athletes

# prediction calculation by hand is possible,
# using predict() is definitely better, reassuringly the answers are the same
n <- nrow(swim)
p <- swim.lm3$rank
b <- coef(swim.lm3)
v <- vcov(swim.lm3)

x <- c(1, log(400), rep(0, 20))
logt <- as.numeric(b %*% x)
sterr2 <- sqrt(as.numeric(t(x) %*% v %*% x) + summary(swim.lm3)$sigma^2)
exp(logt + c(0, -1, 1) * qt(0.975, df = n-p) * sterr2)


