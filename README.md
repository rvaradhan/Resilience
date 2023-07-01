# Resilience
R Codes for the Biostatistics (2023) paper on identifying predictors of resilience. 

Reference for the manuscript:

Varadhan R, Zhu J, Bandeen-Roche, K.  Identifying predictors of resilience to stressors in single arm studies of pre-post change, Biostatistics, Accepted.

Functions defined in the package:
In the **simulations** folder:

> **"tkr simulation I.R"**:
> - rho.olkin(r, n): provides an unbiased estimator of rho given r and n, where r is a biased estimator of rho. (Section 3.1 Equation 3.13)
> - r2.olkin(r2, n, p): provides an unbiased estimator of r-square, where r2 is the coefficient of determination, p is the dimension of X.  (Section 4 Equation 4.31)

> **"tkr simulation II.R"**:
> - rho.olkin(r, n): provides an unbiased estimator of rho given r and n, where r is a biased estimator of rho. (Section 3.1 Equation 3.13)
> - r2.olkin(r2, n, p): provides an unbiased estimator of r-square, where r2 is the coefficient of determination, p is the dimension of X. (Section 4 Equation 4.31)
> - genCovariates(N, mu, sd, alpha): generates covariates, where alpha is the shape parameter in the skewed-normal distribution. (Section 6.2 Equation 6.35-6.37)
> - genOutcomes.rsn(data, omega=10, alpha=-10, w=0.5): generates outcomes by assuming skewed-normal conditional distribution of outcomes, where omega is the scale parameter and alpha is the shape parameter in the skewed-normal distribution, and w is the proportion parameter in calculating mu20. (Section 6.2 Equation 6.35-6.37)
> - genOutcomes.norm(data, omega=10, alpha=-10, w=0.5): generates outcomes by assuming normal conditional distribution of outcomes, where omega is the scale parameter and alpha is the shape parameter in the skewed-normal distribution, and w is the proportion parameter in calculating mu20.  (Section 6.2 Equation 6.35-6.37)

In the **Analysis** folder:

> **"pre-post-regression.R"**:
> - pkgTest(x): test if a required package has already been downloaded. 
> - prepost(formula, data, change=TRUE, k=c(1.0,1.5), nboot=200, ci.level=0.95, boot.method=c("perc", "norm", "basic", "bca")): the main function provides pre-post change regression analysis. The formula should be a `lm' formula, such as y2~y1+x1, where y2 is the post value, y1 is the pre value and need to be called before any other covariates. The "change" option is to choose if a pre-post change regression model should be estimated, by default it is set to TRUE. 
 		
Data in the package:
> **"tkr.RData"**:
> - Simulated data with 1800 observations and 6 variables. post.Y is the outcome come variable and pre.Y is the first covariate. 
