require(MASS)
require(boot)
require(nptest)
require(sn)
require(pcaPP)
#require(mblm)  # Thiel-Sen estimator based on Kendall-tau 

# Skewed normal distribution
# package `sn`
# http://azzalini.stat.unipd.it/SN/Intro/intro.html

#######################################
require(gsl)
# Olkin and Pratt (1958)
#
# unbiased sample correlation coefficient
rho.olkin <- function(r, n){
  r*hyperg_2F1(0.5,0.5,(n-1)/2,1-r^2)
}

rho.olkin(0.3,22)

# unbiased sample R^2
r2.olkin <- function(r2, n, p){
  1 - (n-2)/(n-p)* (1-r2) * hyperg_2F1(1,1,(n-p+2)/2, 1-r2)
}

r2.olkin(0.1,50,3)

############################################################
# Conditional expectation based simulation
#
genCovariates <- function(N, mu, sd, alpha){
  A <- rbinom(N, size=1, p=0.5)
  X2 <- rbinom(N, size=1, p=0.4)
  
  X1 <- rep(NA, N)
  X1[X2==0] <- rsn(n=sum(1-X2), xi=mu1[2], omega=sd[2], alpha=alpha[2])
  X1[X2==1] <- rsn(n=sum(X2), xi=mu1[1], omega=sd[1], alpha=alpha[1])
  
  sim.dat <- data.frame(x1=X1, x1c=X1-mean(X1), x2=X2, tx=A)
  return(sim.dat)  
}

genOutcomes <- function(data, omega=10, alpha=-10, w=0.5){
  N <- nrow(data)
  mY1 <- with(data, 40 - 0.05*x1c + 2*x2)
  Y1 <- rnorm(N, mean=mY1, sd=7)
  mY2.0 <- (w*Y1+(1-w)*mY1)[data$tx==0]
  mY2.1 <- (50 + 0.3*(Y1-mean(Y1)) - 0.1*data$x1c + 2.0*data$x2)[data$tx==1]
  Y2 <- rep(NA, N)
  Y2[data$tx==0] <- rnorm(sum(1-data$tx), mean=mY2.0, sd=7)
  Y2[data$tx==1] <- rsn(sum(data$tx), xi=mY2.1, omega=omega, alpha=alpha) # skew-normal
#  Y2[data$tx==1] <- rnorm(sum(data$tx), mean=mY2.1, sd=6)
  data$y1 <- Y1
  data$y1c <- Y1 - mean(Y1)
  data$y2 <- Y2
  return(data)  
}

mu1 <- c(61, 63)
s1 <- c(10,10)
alpha <- c(10,10)
alpha.Y2 <- -10
omega.Y2 <- 10
w <- 0.48  # when skew-normal
# w <- 0.43  # when normal

# Estimating the "true" value of `k`
Ninf <- 100000
covInf.dat <- genCovariates(Ninf, mu1, s1, alpha)
k.true <- rep(NA, 100)
for (j in 1:100){
sim.dat <- genOutcomes(covInf.dat, w=w, alpha=alpha.Y2, omega=omega.Y2)
rho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
rho.0 <- with(sim.dat, cor(y1[tx==0], y2[tx==0]))
sd10 <- with(sim.dat, sd(y1[tx==0]))
sd20 <- with(sim.dat, sd(y2[tx==0]))

k.true[j] <- (rho.0/rho)*(sd20/sd10)
}
print(mean(k.true))

k <- round(mean(k.true),2)   # true value of `k`
k <- 1.0

set.seed(54321)
nsim <- 10000
beta.naive <- beta.rct <- beta.cr <- beta.a <- beta.b <- matrix(NA, nsim, 4)
beta10 <- beta20 <- matrix(NA, nsim, 3)

truth <- c(0.3, -0.1, 2.0) - c(w, (1-w)*(-0.05), (1-w)*2)

k.est <-  rep(NA, nsim)
m1rho.est <- matrix(NA, nsim, 2)

N <- 1000
cov.dat <- genCovariates(N, mu1, s1, alpha)

for (i in 1:nsim){
     
    sim.dat <- genOutcomes(cov.dat, w=w, alpha=alpha.Y2, omega=omega.Y2)
  
  srho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
#  rho <- srho
  rho <- rho.olkin(srho, N)
  krho <- k*rho

#  rho.0 <- with(sim.dat, cor(y1[tx==0], y2[tx==0]))
#  sd10 <- with(sim.dat, sd(y1[tx==0]))
#  sd20 <- with(sim.dat, sd(y2[tx==0]))
  
#  ans <- lm(y2-y1 ~ tx*y1c + tx*x1c + tx*x2, data=sim.dat)
  ans <- lm(y2 ~ tx*y1c + tx*x1c + tx*x2, data=sim.dat)
  beta.rct[i, ] <- coef(ans)[c(2,6,7,8)]
  
#  mod.naive <- lm(y2-y1 ~  y1c + x1c + x2, data = sim.dat, subset = tx==1)
  mod.naive <- lm(y2 ~  y1c + x1c + x2, data = sim.dat, subset = tx==1)
  mod1 <- lm(y1 ~  x1c + x2, data = sim.dat, subset = tx==1)

#  S <- (summary(mod1)$r.squared/(N-2))
  S.unadj <- summary(mod1)$r.squared
#  S <- S.unadj
  S <- r2.olkin(S.unadj, N, 2)
  #  k <- (rho.0/rho)*(sd20/sd10)   # Eq. 3.7 of the paper
  #  krho <- rho.0*(sd20/sd10)   # Eq. 7 of the paper

#  k.est[i] <- (rho.0/rho)*(sd20/sd10)

  beta.naive[i,] <- coef(mod.naive)
  
  m1rho <- 1.0

#  correction.1b <- 1 + (m1rho*S - krho) / (1-S)
  correction.1b <- (m1rho*S - krho) / (1-S)
  correction.xb <- (krho - m1rho)/(1-S)*coef(mod1)[-1]
  correction.b <- c(correction.1b, correction.xb)
  beta.b[i,] <- beta.naive[i,] + c(0, correction.b)
  
#  mod10 <- lm(y1 ~  x1 + x2, data = sim.dat, subset = tx==0)
#  mod20 <- lm(y2 ~  x1 + x2, data = sim.dat, subset = tx==0)
#  beta10[i,] <- coef(mod10)
#  beta20[i,] <- coef(mod20)
#  m1rho.est[i,] <- coef(mod20)[-1]/coef(mod10)[-1]
  
}

signif(cbind(colMeans(beta.naive), colMeans(beta.rct), colMeans(beta.b)),3)[-1,]

signif(cbind(apply(beta.naive, 2, sd), apply(beta.rct, 2, sd), apply(beta.b, 2, sd)),3)[-1,]



###########################

genCovariates1 <- function(N, mu, sd){
  A <- rbinom(N, size=1, p=0.5)

  X1 <- rnorm(N, mean=mu, sd=sd)

  sim.dat <- data.frame(x1=X1, x1c=X1-mean(X1), tx=A)
  return(sim.dat)  
}

genOutcomes1 <- function(data, w=0.4){
  N <- nrow(data)
  mY1 <- with(data, 40 - 0.05*x1c)
  Y1 <- rnorm(N, mean=mY1, sd=7)
  mY2.0 <- (w*Y1+(1-w)*mY1)[data$tx==0]
  mY2.1 <- (50 + 0.3*(Y1-mean(Y1)) - 0.1*data$x1c)[data$tx==1]
  Y2 <- rep(NA, N)
  Y2[data$tx==0] <- rnorm(sum(1-data$tx), mean=mY2.0, sd=5)
  Y2[data$tx==1] <- rnorm(sum(data$tx), mean=mY2.1, sd=6)
  data$y1 <- Y1
  data$y1c <- Y1 - mean(Y1)
  data$y2 <- Y2
  return(data)  
}

mu1 <- c(61, 63)
s1 <- c(10,10)
alpha <- c(10,10)
w <- 0.5 

# Estimating the "true" value of `k`
Ninf <- 10000
covInf.dat <- genCovariates1(Ninf, mu1, s1)
k.true <- rep(NA, 100)
for (j in 1:100){
  sim.dat <- genOutcomes1(covInf.dat, w=w)
  rho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
  rho.0 <- with(sim.dat, cor(y1[tx==0], y2[tx==0]))
  sd10 <- with(sim.dat, sd(y1[tx==0]))
  sd20 <- with(sim.dat, sd(y2[tx==0]))
  
  k.true[j] <- (rho.0/rho)*(sd20/sd10)
}
print(mean(k.true))

k <- round(mean(k.true),2)   # true value of `k`
#k <- 1.77
#k <- 1.4

#set.seed(54321)
nsim <- 10000
beta.naive <- beta.rct <- beta.cr <- beta.a <- beta.b <- matrix(NA, nsim, 3)
beta10 <- beta20 <- matrix(NA, nsim, 3)

k.est <-  rep(NA, nsim)
m1rho.est <- matrix(NA, nsim, 2)

N <- 200
cov.dat <- genCovariates1(N, mu1, s1)

for (i in 1:nsim){
  
  sim.dat <- genOutcomes1(cov.dat, w=w)
  
  rho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
  krho <- k*rho
  
  #  rho.0 <- with(sim.dat, cor(y1[tx==0], y2[tx==0]))
  #  sd10 <- with(sim.dat, sd(y1[tx==0]))
  #  sd20 <- with(sim.dat, sd(y2[tx==0]))
  
  #  ans <- lm(y2-y1 ~ tx*y1c + tx*x1c + tx*x2, data=sim.dat)
  ans <- lm(y2 ~ tx*y1c + tx*x1c, data=sim.dat)
  beta.rct[i, ] <- coef(ans)[c(2,5,6)]
  
  #  mod.naive <- lm(y2-y1 ~  y1c + x1c + x2, data = sim.dat, subset = tx==1)
  mod.naive <- lm(y2 ~  y1c + x1c, data = sim.dat, subset = tx==1)
  mod1 <- lm(y1 ~  x1c, data = sim.dat, subset = tx==1)
  
  S <- summary(mod1)$r.squared

  #  k <- (rho.0/rho)*(sd20/sd10)   # Eq. 3.7 of the paper
  #  krho <- rho.0*(sd20/sd10)   # Eq. 7 of the paper
  
  #  k.est[i] <- (rho.0/rho)*(sd20/sd10)
  
  beta.naive[i,] <- coef(mod.naive)
  
  m1rho <- 1.0
  
  #  correction.1b <- 1 + (m1rho*S - krho) / (1-S)
  correction.1b <- (m1rho*S - krho) / (1-S)
  correction.xb <- (krho - m1rho)/(1-S)*coef(mod1)[-1]
  correction.b <- c(correction.1b, correction.xb)
  beta.b[i,] <- beta.naive[i,] + c(0, correction.b)
  
  #  mod10 <- lm(y1 ~  x1 + x2, data = sim.dat, subset = tx==0)
  #  mod20 <- lm(y2 ~  x1 + x2, data = sim.dat, subset = tx==0)
  #  beta10[i,] <- coef(mod10)
  #  beta20[i,] <- coef(mod20)
  #  m1rho.est[i,] <- coef(mod20)[-1]/coef(mod10)[-1]
  
}

signif(cbind(colMeans(beta.naive), colMeans(beta.rct), colMeans(beta.b)),3)[-1,]

signif(cbind(apply(beta.naive, 2, sd), apply(beta.rct, 2, sd), apply(beta.b, 2, sd)),3)[-1,]

summary(k.est)

summary(m1rho.est)

###############################
# Simulation for Section 3
# Baseline versus change.  No covariates
#
# Scenario 1
mu <- c(0,10)
sigma <- matrix(NA, 2, 2)
sigma[1,1] <- 59.5
sigma[2,1] <- sigma[1,2] <- 20
sigma[2,2] <- 46

cormat <- cov2cor(sigma)
rho <- cormat[2,1]

mu0 <- c(0,0)
sigma0 <- matrix(NA, 2, 2)
k <- 1.4
r0 <- k*rho
sigma0[1,1] <- sigma[1,1]
sigma0[2,2] <- sigma0[1,1]
sigma0[2,1] <- sigma0[1,2] <- r0*sqrt(sigma0[1,1]*sigma0[2,2])

true.effect <- rho*(sqrt(sigma[2,2]/sigma[1,1]) - k)

n <- 100
tx <- rep(c(0,1), each=n)

nsim <- 10000
beta <- matrix(NA, nsim, 4)

for (i in 1:nsim){
y0 <- mvrnorm(n, mu0, sigma0)
y1 <- mvrnorm(n, mu, sigma)
y <- rbind(y0, y1)
data <- data.frame(y1=y[,1], y2=y[,2], tx=tx)
rho.est <- with(data, cor(y1[tx==1], y2[tx==1]))
rho.olk <- rho.olkin(rho.est, n)

mod.naive <- lm(y2 ~  y1, data = data, subset = tx==1)
mod.rct <- lm(y2 ~  y1*tx, data = data)
beta.naive <- coef(mod.naive)[-1]
beta.cor <- beta.naive - k*rho.est 
beta.cor2 <- beta.naive - k*rho.olk 
beta.rct <- coef(mod.rct)[4]
beta[i,] <- c(beta.naive,beta.rct,beta.cor,beta.cor2)
}

colMeans(beta)
print(true.effect)
apply(beta, 2, sd)


plot(density(beta[,2]), ylim=c(0, 7), lwd=2, lty=2, xlab="Effect of baseline", xlim=c(-0.6,0.7), main="")
lines(density(beta[,1]), col=1, lwd=2)
lines(density(beta[,4]), col=2, lwd=2)
abline(v=true.effect, lty=3, lwd=2)
legend(locator(1), legend=c("naive", "corrected", "rct"), col=c(1,2,1), lty=c(1,1,2), lwd=2)

print(rho)
[1] 0.3822897
> colMeans(beta)
[1]  0.3365906 -0.1988784 -0.1972358 -0.1995240
> print(true.effect)
[1] -0.1990711
> apply(beta, 2, sd)
[1] 0.08256905 0.11903943 0.05531573 0.05559830

######################################################
# Scenario 1
mu <- c(0,1)
sigma <- matrix(NA, 2, 2)
sigma[1,1] <- 1.0
sigma[2,1] <- sigma[1,2] <- 0.4
sigma[2,2] <- 0.7

cormat <- cov2cor(sigma)
rho <- cormat[2,1]

mu0 <- c(0,0)
sigma0 <- matrix(NA, 2, 2)
k <- 1.4
r0 <- k*rho
sigma0[1,1] <- sigma[1,1]
sigma0[2,2] <- sigma0[1,1]
sigma0[2,1] <- sigma0[1,2] <- r0*sqrt(sigma0[1,1]*sigma0[2,2])


cor <- (exp(sigma[1,2]) - 1)/sqrt((exp(sigma[1,1]) - 1)*(exp(sigma[2,2]) - 1))
cor0 <- (exp(sigma0[1,2]) - 1)/sqrt((exp(sigma0[1,1]) - 1)*(exp(sigma0[2,2]) - 1))
var1 <- exp(sigma[1,1]) - 1  # variance of log-normal
var2 <- exp(sigma[2,2]) - 1

print(cor0/cor)
k <- cor0/cor

true.effect <- 0.115  # (10000 simulations at a sample size of 50000)

n <- 1000
tx <- rep(c(0,1), each=n)

nsim <- 10000
beta <- matrix(NA, nsim, 4)
#k <- 1.27
k.est <- rep(NA,nsim)

for (i in 1:nsim){
  y0 <- exp(mvrnorm(n, mu0, sigma0))
  y1 <- exp(mvrnorm(n, mu, sigma))
  y <- rbind(y0, y1)
  data <- data.frame(y1=y[,1], y2=y[,2], tx=tx)
  rho.est <- with(data, cor(y1[tx==1], y2[tx==1]))
  rho0.est <- with(data, cor(y1[tx==0], y2[tx==0]))

  rho.olk <- rho.olkin(rho.est, n)
  
  mod.naive <- lm(y2 ~  y1, data = data, subset = tx==1)
  mod.rct <- lm(y2 ~  y1*tx, data = data)
  beta.naive <- coef(mod.naive)[-1]
  beta.cor <- beta.naive - k*rho.est 
  beta.cor2 <- beta.naive - k*rho.olk 
  beta.rct <- coef(mod.rct)[4]
  beta[i,] <- c(beta.naive,beta.rct,beta.cor,beta.cor2)
  k.est[i] <- (rho0.est/rho.est)
}

colMeans(beta)
apply(beta, 2, sd)
summary(k.est)
apply(beta, 2, summary)

plot(density(beta[,2]), ylim=c(0, 2.0), lwd=2, lty=2, xlab="Effect of baseline", xlim=c(-2,3), main="")
lines(density(beta[,4]), col=2, lwd=2)
lines(density(beta[,1]), col=1, lwd=2)
abline(v=0.116, lty=3, lwd=2)
legend(locator(1), legend=c("naive", "corrected", "rct"), col=c(1,2,1), lty=c(1,1,2), lwd=2)


#####  truth
# N <- 50000
# nsim <- 10000
> colMeans(beta)
[1] 0.6704863 0.1154432 0.1156311 0.1156263
> apply(beta, 2, sd)
[1] 0.02550106 0.03187582 0.01791788 0.01791785
> summary(k.est)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.271   1.459   1.489   1.489   1.518   1.842 
> apply(beta, 2, summary)
[,1]        [,2]        [,3]        [,4]
Min.    0.4198432 -0.13065011 -0.02709707 -0.02710114
1st Qu. 0.6552486  0.09526882  0.10445475  0.10444996
Median  0.6714411  0.11560313  0.11634402  0.11633934
Mean    0.6704863  0.11544317  0.11563107  0.11562630
3rd Qu. 0.6874277  0.13638002  0.12753814  0.12753337
Max.    0.7767804  0.26380314  0.17619867  0.17619398

###########################################################
######################################################

romberg <- function(x, y) {
  # first-order acceleration
  k <- length(x) 
  x.new <- x
  for (i in 2:k){
    x.new[i] <- 1/(1/y[i] - 1/y[i-1]) *(x[i]/y[i] - x[i-1]/y[i-1])
  }
  return(x.new)
}

aitken <- function(x, y) {
  # second-order acceleration
  k <- length(x) 
  x.new <- x
  for (i in 3:k){
    x.new[i] <- 1/(2/y[i] - 3/y[i-1] + 1/y[i-2]) *(2*x[i]/y[i] - 3*x[i-1]/y[i-1] + x[i-2]/y[i-2])
  }
  return(x.new)
}

#######################

mu <- c(0,1)
sigma <- matrix(NA, 2, 2)
sigma[1,1] <- 1.0
sigma[2,1] <- sigma[1,2] <- 0.4
sigma[2,2] <- 0.7

cormat <- cov2cor(sigma)
rho <- cormat[1,2]
  
mu0 <- c(0,0)
sigma0 <- matrix(NA, 2, 2)
k <- 1.4
r0 <- k*rho
sigma0[1,1] <- sigma[1,1]
sigma0[2,2] <- sigma0[1,1]
sigma0[2,1] <- sigma0[1,2] <- r0*sqrt(sigma0[1,1]*sigma0[2,2])


true.effect <- 0.1154  # (50000 simulations at a sample size of 100000)
# print(effect[j])

# [1] 0.1153889
#  sdeff[j]
# [1] 0.02294912


# n <- c(1000,2000,4000)
n <- c(714,1428,2856)   # total sample size =~ 5,000

nsim <- 50000
beta <- rep(NA, nsim)
effect <- sdeff <- rep(NA, length(n))

for (j in 1:length(n)){
tx <- rep(c(0,1), each=n[j])
for (i in 1:nsim){
  y0 <- exp(mvrnorm(n[j], mu0, sigma0))
  y1 <- exp(mvrnorm(n[j], mu, sigma))
  y <- rbind(y0, y1)
  data <- data.frame(y1=y[,1], y2=y[,2], tx=tx)

  mod.rct <- lm(y2 ~  y1*tx, data = data)
  beta[i] <- coef(mod.rct)[4]
}

effect[j] <- mean(beta)
sdeff[j] <- sd(beta)

print(effect[j])
}

ans <- romberg(effect, sdeff^2)
ans2 <- aitken(effect, sdeff^2)

print(ans)
print(ans2)

# original estimator
> print(effect)
[1] 0.2092416 0.1726499 0.1522682 0.1396406 0.1307195 0.1249110

# Romberg estimator
> print(ans)
[1] 0.2092416 0.1311310 0.1273675 0.1224372 0.1193710 0.1171804

# Aitken estimator
> print(ans2)
[1] 0.2092416 0.1726499 0.1223309 0.1146210 0.1157935 0.1140829

