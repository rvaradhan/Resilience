require(MASS)
require(sn)
require(gsl)

#######################################
# unbiased sample correlation coefficient (for normal distribution)
# Olkin and Pratt (1958)
#
rho.olkin <- function(r, n){
  r*hyperg_2F1(0.5,0.5,(n-1)/2,1-r^2)
}

# unbiased sample R^2 (for normal distribution)
r2.olkin <- function(r2, n, p){
  1 - (n-2)/(n-p)* (1-r2) * hyperg_2F1(1,1,(n-p+2)/2, 1-r2)
}

############################################################
# Simulation for Section 6.1 of the paper
# Post stressor versus pre-stressor.  
# No covariates
#
# Simulation I:  Scenario 1
#
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

set.seed(54321)

nsim <- 10000
beta <- matrix(NA, nsim, 3)

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
beta.cor <- beta.naive - k*rho.olk 
beta.rct <- coef(mod.rct)[4]
beta[i,] <- c(beta.naive,beta.rct,beta.cor)
}

print(true.effect)
colMeans(beta)
apply(beta, 2, sd)

######################################################
# Simulation I:  Scenario 2
#
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

true.effect <- 0.115  # (obtained from 10000 simulations at a sample size of 50000)

set.seed(123)
n <- 100
tx <- rep(c(0,1), each=n)

nsim <- 10000
beta <- matrix(NA, nsim, 3)
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
  beta.cor <- beta.naive - k*rho.olk 
  beta.rct <- coef(mod.rct)[4]
  beta[i,] <- c(beta.naive,beta.rct,beta.cor)
  k.est[i] <- (rho0.est/rho.est)
}

print(true.effect)
colMeans(beta)
apply(beta, 2, sd)

