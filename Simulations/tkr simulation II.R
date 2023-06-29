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
##
genCovariates <- function(N, mu, sd, alpha){
  A <- rbinom(N, size=1, p=0.5)
  X2 <- rbinom(N, size=1, p=0.4)
  
  X1 <- rep(NA, N)
  X1[X2==0] <- rsn(n=sum(1-X2), xi=mu1[2], omega=sd[2], alpha=alpha[2])
  X1[X2==1] <- rsn(n=sum(X2), xi=mu1[1], omega=sd[1], alpha=alpha[1])
  
  sim.dat <- data.frame(x1=X1, x1c=X1-mean(X1), x2=X2, tx=A)
  return(sim.dat)  
}

genOutcomes.rsn <- function(data, omega=10, alpha=-10, w=0.5){
  N <- nrow(data)
  mY1 <- with(data, 40 - 0.05*x1c + 2*x2)
  Y1 <- rnorm(N, mean=mY1, sd=7)
  mY2.0 <- (w*Y1+(1-w)*mY1)[data$tx==0]
  mY2.1 <- (50 + 0.3*(Y1-mean(Y1)) - 0.1*data$x1c + 2.0*data$x2)[data$tx==1]
  Y2 <- rep(NA, N)
  Y2[data$tx==0] <- rnorm(sum(1-data$tx), mean=mY2.0, sd=7)
  Y2[data$tx==1] <- rsn(sum(data$tx), xi=mY2.1, omega=omega, alpha=alpha) # skew-normal
  data$y1 <- Y1
  data$y1c <- Y1 - mean(Y1)
  data$y2 <- Y2
  return(data)  
}

genOutcomes.norm <- function(data, omega=10, alpha=-10, w=0.5){
  N <- nrow(data)
  mY1 <- with(data, 40 - 0.05*x1c + 2*x2)
  Y1 <- rnorm(N, mean=mY1, sd=7)
  mY2.0 <- (w*Y1+(1-w)*mY1)[data$tx==0]
  mY2.1 <- (50 + 0.3*(Y1-mean(Y1)) - 0.1*data$x1c + 2.0*data$x2)[data$tx==1]
  Y2 <- rep(NA, N)
  Y2[data$tx==0] <- rnorm(sum(1-data$tx), mean=mY2.0, sd=7)
  Y2[data$tx==1] <- rnorm(sum(data$tx), mean=mY2.1, sd=6)
  data$y1 <- Y1
  data$y1c <- Y1 - mean(Y1)
  data$y2 <- Y2
  return(data)  
}

################################################################
# Simulation II: Scenario A

mu1 <- c(61, 63)
s1 <- c(10,10)
alpha <- c(10,10)
alpha.Y2 <- -10
omega.Y2 <- 10
w <- 0.48  

# Estimating the "true" value of sensitivity parameter `k`
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

set.seed(54321)
nsim <- 5000
beta.naive <- beta.rct <- beta.cr <- beta.a <- beta.b <- matrix(NA, nsim, 4)
beta10 <- beta20 <- matrix(NA, nsim, 3)

truth <- c(0.3, -0.1, 2.0) - c(w, (1-w)*(-0.05), (1-w)*2)

N <- 1000
cov.dat <- genCovariates(N, mu1, s1, alpha)

for (i in 1:nsim){
     
    sim.dat <- genOutcomes.rsn(cov.dat, w=w, alpha=alpha.Y2, omega=omega.Y2)
  
  srho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
  rho <- rho.olkin(srho, N)
  krho <- k*rho

  ans <- lm(y2 ~ tx*y1c + tx*x1c + tx*x2, data=sim.dat)
  beta.rct[i, ] <- coef(ans)[c(2,6,7,8)]
  
  mod.naive <- lm(y2 ~  y1c + x1c + x2, data = sim.dat, subset = tx==1)
  mod1 <- lm(y1 ~  x1c + x2, data = sim.dat, subset = tx==1)

  S.unadj <- summary(mod1)$r.squared
  S <- r2.olkin(S.unadj, N, 2)

  beta.naive[i,] <- coef(mod.naive)
  
  m <- 1.0

  correction.1b <- (m - krho) / (1-S) - m
  correction.xb <- (krho - m)/(1-S)*coef(mod1)[-1]
  correction.b <- c(correction.1b, correction.xb)
  beta.b[i,] <- beta.naive[i,] + c(0, correction.b)

}

print(truth)

signif(cbind(colMeans(beta.naive), colMeans(beta.rct), colMeans(beta.b)),3)[-1,]

signif(cbind(apply(beta.naive, 2, sd), apply(beta.rct, 2, sd), apply(beta.b, 2, sd)),3)[-1,]

###########################
# Simulation II: Scenario B

set.seed(54321)

mu1 <- c(61, 63)
s1 <- c(10,10)
alpha <- c(10,10)
alpha.Y2 <- -10
omega.Y2 <- 10
w <- 0.48  

# Estimating the "true" value of sensitivity parameter `k`
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

k <- round(mean(k.true),1)  

nsim <- 5000
beta.naive <- beta.rct <- beta.cr <- beta.a <- beta.b <- matrix(NA, nsim, 4)
beta10 <- beta20 <- matrix(NA, nsim, 3)

truth <- c(0.3, -0.1, 2.0) - c(w, (1-w)*(-0.05), (1-w)*2)

N <- 1000
cov.dat <- genCovariates(N, mu1, s1, alpha)

for (i in 1:nsim){
  
  sim.dat <- genOutcomes.norm(cov.dat, w=w, alpha=alpha.Y2, omega=omega.Y2)
  
  srho <- with(sim.dat, cor(y1[tx==1], y2[tx==1]))
  rho <- rho.olkin(srho, N)
  krho <- k*rho
  
  ans <- lm(y2 ~ tx*y1c + tx*x1c + tx*x2, data=sim.dat)
  beta.rct[i, ] <- coef(ans)[c(2,6,7,8)]
  
  mod.naive <- lm(y2 ~  y1c + x1c + x2, data = sim.dat, subset = tx==1)
  mod1 <- lm(y1 ~  x1c + x2, data = sim.dat, subset = tx==1)
  
  S.unadj <- summary(mod1)$r.squared
  S <- r2.olkin(S.unadj, N, 2)

  beta.naive[i,] <- coef(mod.naive)
  
  m <- 1.0
  
  correction.1b <- (m - krho) / (1-S) - m
  correction.xb <- (krho - m1rho)/(1-S)*coef(mod1)[-1]
  correction.b <- c(correction.1b, correction.xb)
  beta.b[i,] <- beta.naive[i,] + c(0, correction.b)

}

print(truth)

signif(cbind(colMeans(beta.naive), colMeans(beta.rct), colMeans(beta.b)),3)[-1,]

signif(cbind(apply(beta.naive, 2, sd), apply(beta.rct, 2, sd), apply(beta.b, 2, sd)),3)[-1,]
