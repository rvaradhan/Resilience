# Resilience Validation Analysis #
# Jiafeng Zhu #
# 2024-12-11 #


################################################################################
## 1. Data Preparation
#$ Data from two parts: TKR, OAI. We filter data by age >= 60 so that they are 
#$ older adults, and age <= 80 so that the two have same age range. This
#$ makes the two datasets comparable. Age, BMI are scaled. 

## 1.a TKR data
library(psych)
library(dplyr)
tjr<-openxlsx::read.xlsx("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Data/FORCE_Data_for_JHU.xlsx")
tjr$death <- 0 
tjr$death[tjr$status == "Deceased"]<-1

tjr$pcs_n_1 <- !is.na(tjr$pre_pcs)
tjr$pcs_n_2 <- !is.na(tjr$post12m_pcs)

tjr$smoker <- ifelse(tjr$cig_smoker %in% "no" & tjr$past_smoker %in% "no", "Never smoke", ifelse(tjr$cig_smoker %in% "yes" | tjr$past_smoker %in% "yes", "Other", NA))
tjr$smoker[is.na(tjr$smoker)==T & (tjr$cig_smoker %in% "no" | tjr$past_smoker %in% "no")] <- "Never smoke"


tjr$age_5 <- (tjr$age-60)/5
tjr$bmi_5 <- (tjr$bmi-25)/5
tjr_sub <- tjr[tjr$pcs_n_1 == T & tjr$pcs_n_2 == T, ]

dim(tjr_sub)
describe(tjr_sub$age)
tjr_sub <- tjr_sub %>% # filter by age
  filter(
    age <= 80
  )

## 1.b OAI data
dat00 <- read.table("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Data/AllClinical00.txt", sep="|", header = T)
dat01 <- read.table("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Data/AllClinical01.txt", sep="|", header = T)
Enrollees <-  read.table("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Data/Enrollees.txt", sep="|", header = T)
dat00 <- dat00 %>% 
  dplyr::select(
    ID,
    V00HSPSS,
    V00HSMSS,
    P01BMI,
    V00AGE,
    V00MARITST,
    V00EDCV,
    V00PSMOKER,
    V00PIPE,
    V00PIPENEV,
    V00PIPENOW,
    V00SMOKE,
    V00SMKNEV,
    V00SMKNOW,
    V00COMORB,
    V00INCOME2
  )
dat01 <- dat01 %>% 
  dplyr::select(
    ID,
    V01HSPSS,
    V01HSMSS
  )

Enrollees <- Enrollees %>%
  dplyr::select(
    ID,
    V00COHORT,
    P02SEX,
    P02RACE
  )

final <- dat00 %>%
  full_join(
    .,
    dat01,
    by = c("ID"="ID")
  ) %>%
  full_join(
    .,
    Enrollees,
    by = c("ID"="ID")
  ) %>%
  filter(
    is.na(V00HSPSS)==F &
      is.na(V01HSPSS)==F &
      is.na(V00HSMSS)==F &
      is.na(V01HSMSS)==F,
    V00AGE >= 60,
    V00COHORT %in% "1: Progression"
  ) %>%
  mutate(
    age_5 = (V00AGE-60)/5,
    bmi_5 = (P01BMI-25)/5,
    gender = ifelse(P02SEX %in% "2: Female", "Female", "Male"),
    race_c1= ifelse(P02RACE %in% "1: White or Caucasian", "White",
                    ifelse(P02RACE %in% "2: Black or African American", "Black",
                           ifelse(P02RACE %in% "3: Asian", "Asian or Pacific Islander",
                                  "oth/dk/refused"))), 
    edu_c= ifelse(V00EDCV %in% c("0: Less than high school graduate",
                                 "1: High school graduate"), "High school or less", "Post high school or more"),
    marital_status= ifelse(V00MARITST %in% "1: Married", "married or living with someone as married",
                           ifelse(V00MARITST %in% "5: Never married", "never married","widowed, separated or divorced")),
    smoker= ifelse(V00PSMOKER %in% "0: Never", "Never smoke", "Other"),
    household_income_c= ifelse(V00INCOME2 %in% "1: < $50K", "<= $45,000",
                               ifelse(V00INCOME2 %in% "2: > $50K", ">= $45,001", "DK")),
    pre_mccom_index_c=ifelse(V00COMORB == 0, "=0",
                             ifelse(V00COMORB ==1, "=1",
                                    ifelse(V00COMORB %in% c(2,3,4,5), "=2-5",">=6"))),
    pre_pcs=V00HSPSS,
    post12m_pcs=V01HSPSS,
    pre_mcs=V00HSMSS,
    post12m_mcs=V01HSMSS, 
    study = 1,
    treat = 0,
    patient_id = ID
  ) %>%
  filter(
    is.na(pre_mccom_index_c)==F
  )

## 1.c Merge to get final data
tjr_sub <- tjr_sub %>%
  mutate(
    study = 0,
    treat = 1
  ) %>%
  filter(
    is.na(bmi_5)==F,
    is.na(race_c1)==F,
    is.na(edu_c)==F,
    is.na(marital_status)==F,
    is.na(smoker)==F,
    is.na(household_income_c)==F,
    is.na(pre_mccom_index_c)==F,
  )

final_data <- bind_rows(final, tjr_sub) %>%
  mutate(
    race_c2 = ifelse(race_c1 %in% "White", "White",
                     ifelse(race_c1 %in% "Black", "Black", "Other")),
    pre_mccom_index_c1 = ifelse(pre_mccom_index_c %in% c("=2-5",">=6"), ">=2", 
                                pre_mccom_index_c),
    pcs_change = post12m_pcs -pre_pcs,
    mcs_change = post12m_mcs -pre_mcs 
  ) %>%
  filter(
    is.na(pcs_change)==F,
    is.na(mcs_change)==F
  )


final_data <- final_data %>%
  mutate(
    gender = as.factor(gender),
    race_c2 = as.factor(race_c2),
    edu_c = as.factor(edu_c),
    marital_status = as.factor(marital_status),
    smoker = as.factor(smoker),
    household_income_c = as.factor(household_income_c),
    pre_mccom_index_c1 = as.factor(pre_mccom_index_c1)
  )

final_data$age = final_data$age_5 * 5 + 60


## A table 1 need to be provided here
describeBy(final_data$pre_pcs, group = final_data$treat)
describe(final_data$pre_pcs)

describeBy(final_data$post12m_pcs, group = final_data$treat)
describe(final_data$post12m_pcs)

describeBy(final_data$pre_mcs, group = final_data$treat)

describeBy(final_data$post12m_mcs, group = final_data$treat)

describeBy(final_data$age, group = final_data$treat)
describeBy(final_data$V00AGE, group = final_data$treat)
describe(final_data$age_5*5+60)

describeBy(final_data$age_5, group = final_data$treat)
describe(final_data$age_5)

describeBy(final_data$bmi, group = final_data$treat)
describeBy(final_data$P01BMI, group = final_data$treat)
describe(final_data$bmi_5*5+25)

describeBy(final_data$bmi_5, group = final_data$treat)
describe(final_data$bmi_5)


table(final_data$gender, final_data$treat)
table(final_data$gender)
table(final_data$pre_mccom_index_c1, final_data$treat)
table(final_data$pre_mccom_index_c1)

################################################################################
## 2. Matching two datasets by using twang package

## 2.a Match PCS and MCS separately
library(twang)
set.seed(123)

ps.OAI.pcs <- ps(treat~ pre_pcs  + age_5 + bmi_5 #+ gender + pre_mccom_index_c1
             #race_c2 + edu_c + marital_status + smoker + household_income_c +
             #pre_mccom_index_c1
             ,
             verbose=F,
             interaction.depth = 3,
             data=final_data, 
             shrinkage = 0.001,
             estimand = "ATT",
             stop.method = c("es.mean"),
             n.trees = 40000,
             bag.fraction = 0.5,
             n.minobsinnode = 50
)

plot(ps.OAI.pcs, type="b")
summary(ps.OAI.pcs)
bal.table(ps.OAI.pcs)


ps.OAI.mcs <- ps(treat~ pre_mcs + bmi_5 + age_5 # + gender + pre_mccom_index_c1
                 #race_c2 + edu_c + marital_status + smoker + household_income_c +
                 #pre_mccom_index_c1
                 ,
                 verbose=F,
                 interaction.depth = 3,
                 data=final_data, 
                 shrinkage = 0.001,
                 estimand = "ATT",
                 stop.method = c("es.mean"),
                 n.trees = 40000,
                 bag.fraction = 0.5,
                 n.minobsinnode = 50
)

plot(ps.OAI.mcs, type="b")
summary(ps.OAI.mcs)
bal.table(ps.OAI.mcs)

final_data$w.pcs<-get.weights(ps.OAI.pcs,stop.method="es.mean")
final_data$w.mcs<-get.weights(ps.OAI.mcs,stop.method="es.mean")

# calculate weighted covariance matrix
s_oai.mcs <- cov.wt(as.matrix(cbind(final_data$pre_mcs[final_data$treat==0],
                                    final_data$post12m_mcs[final_data$treat==0])),
                    wt = final_data$w.mcs[final_data$treat==0],
                    cor = T)

s_tkr.mcs <- cov.wt(as.matrix(cbind(final_data$pre_mcs[final_data$treat==1],
                                    final_data$post12m_mcs[final_data$treat==1])),
                    wt = final_data$w.mcs[final_data$treat==1],
                    cor = T)


# check distribution of variables
library(ggplot2)
p1 <- ggplot(final_data) +
  geom_density(aes(pre_pcs, group =as.factor(treat), colour = as.factor(treat) ), bw=5, alpha=0.5) +
  scale_colour_discrete(name = "Study", breaks=c(0,1),  labels = c("OAI", "TKR"))+ggtitle("PCS-Unweighted") +
  ylim(0, 0.05)

p2 <- ggplot(final_data) +
  geom_density(aes(pre_pcs, group =as.factor(treat), colour = as.factor(treat) ,weight = ps.OAI.pcs$w$es.mean.ATT),  bw=5, alpha=0.5) +
  scale_colour_discrete(name = "Study", labels = c("OAI", "TKR"))+ggtitle("PCS-Weighted")+
  ylim(0, 0.05)

p3 <- ggplot(final_data) +
  geom_density(aes(pre_mcs, group =as.factor(treat), colour = as.factor(treat) ), bw=5, alpha=0.5) +
  scale_colour_discrete(name = "Study", breaks=c(0,1),  labels = c("OAI", "TKR"))+ggtitle("MCS-Unweighted")+
  ylim(0, 0.05)

p4 <- ggplot(final_data) +
  geom_density(aes(pre_mcs, group =as.factor(treat), colour = as.factor(treat) ,weight = ps.OAI.mcs$w$es.mean.ATT),  bw=5, alpha=0.5) +
  scale_colour_discrete(name = "Study", labels = c("OAI", "TKR"))+ggtitle("MCS-Weighted")+
  ylim(0, 0.05)

library(grid)
library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

cs <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"),
                               p4 + theme(legend.position="none"),
                               nrow=2),
                   mylegend, ncol=2,widths=c(6, 1))

# ggsave("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/GSA present/cs_1.png",cs,height = 5, width = 7, dpi = 300)



# estimated effective sample size
# PCS
w1 <- final_data$w.pcs[final_data$treat==1]
w0 <- final_data$w.pcs[final_data$treat==0]
ess1 <- (sum(w1))^2/sum(w1^2)
ess1
length(w1)
ess0 <- (sum(w0))^2/sum(w0^2)
ess0

# MCS
w1 <- final_data$w.mcs[final_data$treat==1]
w0 <- final_data$w.mcs[final_data$treat==0]
ess1 <- (sum(w1))^2/sum(w1^2)
ess1
length(w1)
ess0 <- (sum(w0))^2/sum(w0^2)
ess0



# Define functions to calculate optimized k and m
findkandm1 <- function(par, a1, ax, rho, R2, coef_1){
  # a1 = naive - ps
  # ax = naive- ps
  f <- numeric(2)
  f[1] <- sum(ax * coef_1)/sum(coef_1^2) + 1 + a1 #m
  f[2] <- ((a1 + 1 - f[1]) * (1-R2) + f[1]) / rho #k
  return(c(f[2], f[1]))
}

# Redefine the prepost function to obtain intermediary parameters
prepost1 <- function (formula, data, change = TRUE, k = 1, m = 1, 
                      nboot = 10, ci.level = 0.95, boot.method = c("perc", "norm", 
                                                                   "basic", "bca"), ncores = 1,
                      weights ) 
{
  varbs <- labels(terms(formula))
  y1 <- varbs[1]
  npar <- length(varbs) + 1
  intcpt <- attr(terms(formula), "intercept")
  formula.text <- if (intcpt == 1) 
    paste(formula[[2]], "-", y1, "~", paste(varbs, collapse = " + "))
  else paste(formula[[2]], "-", y1, "~", paste(varbs, collapse = " + "), 
             "-1")
  formula.text1 <- formula.text
  formula.new <- if (change) 
    as.formula(formula.text1)
  else formula
  if (npar > 2) 
    formula.1 <- if (intcpt == 1) 
      paste(y1, "~", paste(varbs[-1], collapse = " + "))
  else paste(y1, "~", paste(varbs[-1], collapse = " + "), 
             "-1")
  mf <- model.frame(formula, data = data)
  
  rho <- cor(mf[,1], mf[,2])
  mod.naive <- lm(formula.new, data = data)
  beta.naive <- coef(mod.naive)
  if (npar > 2) {
    fml <- formula.1
    mod1 <- lm(formula.1, data = data)
    coef_1 <- coef(mod1)[-1]
    se <- summary(mod1)$coefficients[, 2]
    S <- summary(mod1)$r.squared
  }
  else S <- 0
  beta <- matrix(NA, length(coef(mod.naive)), length(k))
  for (j in 1:length(k)) {
    krho <- k[j] * rho
    correction.1b <- (m - krho)/(1 - S) - (m - 1 * change)
    if (npar > 2) {
      correction.xb <- (krho - m)/(1 - S) * coef(mod1)[-1]
      correction.b <- c(correction.1b, correction.xb)
    }
    else correction.b <- correction.1b
    beta[, j] <- beta.naive + c(0, correction.b)
  }
  
  
  rownames(beta) <- names(beta.naive)
  colnames(beta) <- paste("k", k, sep = "=")
  list(naive.beta = coef(summary(mod.naive)), corrected.beta = beta, 
       #CI = ci,
       rho = rho,
       r2 = S,
       #a1 = correction.1b
       #ax = correction.xb,
       fml=fml,
       coef_1 = coef_1,
       se = se
  )
}


## 2.b PCS
# Weighted regression
fit_wgt <- lm(pcs_change~as.factor(treat)*(pre_pcs + age_5 + bmi_5 + gender + as.factor(pre_mccom_index_c1)) ,
              final_data, weights = w.pcs)

fit_wgt1 <- lm(pcs_change~pre_pcs + age_5 + bmi_5 + gender + as.factor(pre_mccom_index_c1),
              final_data %>% filter(treat==0), weights = w.pcs)

s_oai.pcs <- cov.wt(as.matrix(cbind(final_data$pre_pcs[final_data$treat==0],
                                    final_data$post12m_pcs[final_data$treat==0])),
                    wt = final_data$w.pcs[final_data$treat==0],
                    cor = T)

s_tkr.pcs <- cov.wt(as.matrix(cbind(final_data$pre_pcs[final_data$treat==1],
                                    final_data$post12m_pcs[final_data$treat==1])),
                    wt = final_data$w.pcs[final_data$treat==1],
                    cor = T)

# Parameter k estimate - expected
k_exp.pcs.oai11 <- s_oai.pcs$cov[2,1]/s_tkr.pcs$cov[2,1] * sqrt(s_tkr.pcs$cov[2,2])/sqrt(s_oai.pcs$cov[1,1]) # sigma2/sigma1

fit0.pcs <- lm(pre_pcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
               data=final_data  %>% filter(treat==0), weights = w.pcs)

fit1.pcs <- lm(post12m_pcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
               data=final_data  %>% filter(treat==0), weights = w.pcs)

# Parameter m estimate - expected
m_exp.pcs <- sum(coef(fit1.pcs)[-1]*coef(fit0.pcs)[-1])/
  sum((coef(fit0.pcs)[-1])^2)



# PCS: Method Change = True
# Estimate optimized parameters
library(resilience)
set.seed(1234)
fit_prepost.pcs <- prepost1(post12m_pcs~pre_pcs + age_5 + bmi_5 + gender + pre_mccom_index_c1,
                            final_data  %>% filter(treat==1),
                            k=1,
                            m=0.5,
                            boot.method = "basic",
                            change = TRUE)

a1 <- fit_wgt$coefficients[3]
ax <- fit_wgt$coefficients[4:8]

opt_km.pcs <- findkandm1(a1=a1, 
                         ax=ax, rho=fit_prepost.pcs$rho, R2=fit_prepost.pcs$r2, 
                         coef_1=fit_prepost.pcs$coef_1)

fit_prepost_estimated <- prepost1(post12m_pcs~pre_pcs + age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                                  final_data %>% filter(treat==1),
                                  k=k_exp.pcs.oai11,
                                  m=m_exp.pcs,
                                  boot.method = "basic",
                                  change = TRUE)


coef_weight.pcs <- fit_wgt$coefficients
coef_prepost.pcs <- fit_prepost_estimated$corrected.beta 


#######################################################################
## 2.c MCS
# Weighted regression
fit_wgt <- lm(mcs_change~as.factor(treat)*(pre_mcs + age_5 + bmi_5 + gender + as.factor(pre_mccom_index_c1)) ,
              final_data, weights = w.mcs)

s_oai.mcs <- cov.wt(as.matrix(cbind(final_data$pre_mcs[final_data$treat==0],
                                    final_data$post12m_mcs[final_data$treat==0])),
                    wt = final_data$w.mcs[final_data$treat==0],
                    cor = T)

s_tkr.mcs <- cov.wt(as.matrix(cbind(final_data$pre_mcs[final_data$treat==1],
                                    final_data$post12m_mcs[final_data$treat==1])),
                    wt = final_data$w.mcs[final_data$treat==1],
                    cor = T)

# Parameter k estimate - expected
k_exp.mcs.oai11 <- s_oai.mcs$cov[2,1]/s_tkr.mcs$cov[2,1] * sqrt(s_tkr.mcs$cov[2,2])/sqrt(s_oai.mcs$cov[1,1]) # sigma2/sigma1

fit0.mcs <- lm(pre_mcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
               data=final_data  %>% filter(treat==0), weights = w.mcs)

fit1.mcs <- lm(post12m_mcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
               data=final_data  %>% filter(treat==0), weights = w.mcs)

# Parameter m estimate - expected
m_exp.mcs <- sum(coef(fit1.mcs)[-1]*coef(fit0.mcs)[-1])/
  sum((coef(fit0.mcs)[-1])^2)


# MCS: Method Change = True
# Estimate optimized parameters
library(resilience)
set.seed(1234)
fit_prepost.mcs <- prepost1(post12m_mcs~pre_mcs + age_5 + bmi_5 + gender + pre_mccom_index_c1,
                            final_data  %>% filter(treat==1),
                            k=1,
                            m=0.5,
                            boot.method = "basic",
                            change = TRUE)

a1 <- fit_wgt$coefficients[3]
ax <- fit_wgt$coefficients[4:8]

opt_km.mcs <- findkandm1(a1=a1, 
                         ax=ax, rho=fit_prepost.mcs$rho, R2=fit_prepost.mcs$r2, 
                         coef_1=fit_prepost.mcs$coef_1) # weighted OAI

fit_prepost_estimated <- prepost1(post12m_mcs~pre_mcs + age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                                  final_data %>% filter(treat==1),
                                  k=k_exp.mcs.oai11,
                                  m=m_exp.mcs,
                                  boot.method = "basic",
                                  change = TRUE)

coef_weight.mcs <- fit_wgt$coefficients
coef_prepost.mcs <- fit_prepost_estimated$corrected.beta 






###############################################################################
## 3. Bootstrap for k and m 

# Clean up background to speed up calculation
{
  rm(list=setdiff(ls(),"final_data"))
  library(parallel)
  library(doParallel)
  library(foreach)
  library(doSNOW)
  no_cores <- detectCores()
  
  cl <- makeCluster(no_cores-2, outfile="")  
  
  registerDoSNOW(cl)
  
  progress <- function(n) print(n)
  opts <- list(progress=progress)
  
  set.seed(1234)

  findkandm1 <- function(par, a1, ax, rho, R2, coef_1, wi){
    # a1 = naive - ps
    # ax = naive- ps
      f <- numeric(2)
      f[1] <- sum(ax * coef_1)/sum(coef_1^2) + 1 + a1 #m
      f[2] <- ((a1 + 1 - f[1]) * (1-R2) + f[1]) / rho #k
      return(c(f[2], f[1]))
  }
  
  prepost1 <- function (formula, data, change = TRUE, k = 1, m = 1, 
                        nboot = 10, ci.level = 0.95, boot.method = c("perc", "norm", 
                                                                       "basic", "bca"), ncores = 1,
                        weights ) 
  {
    varbs <- labels(terms(formula))
    y1 <- varbs[1]
    npar <- length(varbs) + 1
    intcpt <- attr(terms(formula), "intercept")
    formula.text <- if (intcpt == 1) 
      paste(formula[[2]], "-", y1, "~", paste(varbs, collapse = " + "))
    else paste(formula[[2]], "-", y1, "~", paste(varbs, collapse = " + "), 
               "-1")
    formula.text1 <- formula.text
    formula.new <- if (change) 
      as.formula(formula.text1)
    else formula
    if (npar > 2) 
      formula.1 <- if (intcpt == 1) 
        paste(y1, "~", paste(varbs[-1], collapse = " + "))
    else paste(y1, "~", paste(varbs[-1], collapse = " + "), 
               "-1")
    mf <- model.frame(formula, data = data)

    rho <- cor(mf[,1], mf[,2])
    mod.naive <- lm(formula.new, data = data)
    beta.naive <- coef(mod.naive)
    if (npar > 2) {
      fml <- formula.1
      mod1 <- lm(formula.1, data = data)
      coef_1 <- coef(mod1)[-1]
      se <- summary(mod1)$coefficients[, 2]
      S <- summary(mod1)$r.squared
    }
    else S <- 0
    beta <- matrix(NA, length(coef(mod.naive)), length(k))
    for (j in 1:length(k)) {
      krho <- k[j] * rho
      correction.1b <- (m - krho)/(1 - S) - (m - 1 * change)
      if (npar > 2) {
        correction.xb <- (krho - m)/(1 - S) * coef(mod1)[-1]
        correction.b <- c(correction.1b, correction.xb)
      }
      else correction.b <- correction.1b
      beta[, j] <- beta.naive + c(0, correction.b)
    }
    
    
    rownames(beta) <- names(beta.naive)
    colnames(beta) <- paste("k", k, sep = "=")
    list(naive.beta = coef(summary(mod.naive)), corrected.beta = beta, 
         #CI = ci,
         rho = rho,
         r2 = S,
         #a1 = correction.1b
         #ax = correction.xb,
         coef_1 = coef_1,
         se = se
    )
  }
  
  
  TKR <- final_data %>% filter(treat == 1) 
  OAI <- final_data %>% filter(treat == 0)
  t0 <- Sys.time()
  
  # 1000 bootstraps: separately for TKR and OAI
  boot_res <- foreach(i = 1:1000, .combine = 'rbind', .packages = c("twang", 
                                                              "dplyr",
                                                              "nleqslv",
                                                              "resilience"), .options.snow=opts)%dopar%{
    lst1 <- sample(1:nrow(TKR), nrow(TKR), replace = TRUE)
    lst2 <- sample(1:nrow(OAI), nrow(OAI), replace = TRUE)
    bootstrap_data <- bind_rows(TKR[lst1, ], OAI[lst2, ])
    if(TRUE){ # get estimates of minimal p-values after matching
      ps.OAI.pcs <- ps(treat~ pre_pcs + age_5 + bmi_5,
                   verbose=F,
                   interaction.depth = 3,
                   data=bootstrap_data, 
                   shrinkage = 0.001,
                   estimand = "ATT",
                   stop.method = c("es.mean"),
                   n.trees = 40000,
                   bag.fraction = 0.5,
                   n.minobsinnode = 50
      )
      pcs.min.pval = min(bal.table(ps.OAI.pcs)$es.mean.ATT$ks.pval,na.rm = TRUE)
      
      ps.OAI.mcs <- ps(treat~ pre_mcs + age_5 + bmi_5,
                       verbose=F,
                       interaction.depth =3,
                       data=bootstrap_data, 
                       shrinkage = 0.001,
                       estimand = "ATT",
                       stop.method = c("es.mean"),
                       n.trees = 40000,
                       bag.fraction = 0.5,
                       n.minobsinnode = 50
      )
      mcs.min.pval = min(bal.table(ps.OAI.mcs)$es.mean.ATT$ks.pval,na.rm = TRUE)
      
      bootstrap_data$w.pcs <- ps.OAI.pcs$w$es.mean.ATT
      bootstrap_data$w.mcs <- ps.OAI.mcs$w$es.mean.ATT
    }
    
    # PCS 
    { # Bootstrap 1: weighted regression
      s_oai.pcs <- cov.wt(as.matrix(cbind(bootstrap_data$pre_pcs[bootstrap_data$treat==0],
                                          bootstrap_data$post12m_pcs[bootstrap_data$treat==0])),
                          wt = bootstrap_data$w.pcs[bootstrap_data$treat==0],
                          cor = T)
      
      s_tkr.pcs <- cov.wt(as.matrix(cbind(bootstrap_data$pre_pcs[bootstrap_data$treat==1],
                                          bootstrap_data$post12m_pcs[bootstrap_data$treat==1])),
                          wt = bootstrap_data$w.pcs[bootstrap_data$treat==1],
                          cor = T)
      
      # expected k for pcs
      k_exp.pcs.oai11 <- s_oai.pcs$cov[2,1]/s_tkr.pcs$cov[2,1] * sqrt(s_tkr.pcs$cov[2,2])/sqrt(s_oai.pcs$cov[1,1]) # sigma2/sigma1
   
      fit0.pcs <- lm(pre_pcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                     data=bootstrap_data  %>% filter(treat==0), weights = w.pcs)
      
      fit1.pcs <- lm(post12m_pcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
       
      # expected m for mcs
      m_exp.pcs <- sum(coef(fit1.pcs)[-1]*coef(fit0.pcs)[-1])/
          sum((coef(fit0.pcs)[-1])^2)
    }
   
    {# Bootstrap 2: prepost function
      fit_prepost.pcs <- prepost1(post12m_pcs~pre_pcs + age_5 + bmi_5 + gender + pre_mccom_index_c1,
                              bootstrap_data  %>% filter(treat==1),
                              k=1,
                              m=0.5,
                              boot.method = "basic",
                              change = TRUE)
      
      bootstrap_data$change <- bootstrap_data$post12m_pcs - bootstrap_data$pre_pcs
      
      fit_wgt <- lm(change~as.factor(treat)*(pre_pcs + age_5 + bmi_5 + gender + pre_mccom_index_c1),
                    bootstrap_data, weights = w.pcs)
      
      a1 <- fit_wgt$coefficients[3]
      ax <- fit_wgt$coefficients[4:8]
      

      fit_pre <- lm(pre_pcs ~ age_5 + bmi_5, bootstrap_data %>% filter(treat==0), weights = w.pcs)
      
      
      opt_km.pcs <- findkandm1(a1=a1, 
                               ax=ax, rho=fit_prepost.pcs$rho, R2=fit_prepost.pcs$r2, 
                               coef_1=fit_prepost.pcs$coef_1)
      
      fit_prepost_estimated <- prepost1(post12m_pcs~pre_pcs + age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                                    bootstrap_data %>% filter(treat==1),
                                    k=k_exp.pcs.oai11,
                                    m=m_exp.pcs,
                                    boot.method = "basic",
                                    change = TRUE)

      coef_weight.pcs <- fit_wgt$coefficients
      coef_prepost.pcs <- fit_prepost_estimated$corrected.beta 
    }
    
    
    ## MCS
    { # Bootstrap 1: weighted regression
      s_oai.mcs <- cov.wt(as.matrix(cbind(bootstrap_data$pre_mcs[bootstrap_data$treat==0],
                                          bootstrap_data$post12m_mcs[bootstrap_data$treat==0])),
                          wt = bootstrap_data$w.mcs[bootstrap_data$treat==0],
                          cor = T)
      
      s_tkr.mcs <- cov.wt(as.matrix(cbind(bootstrap_data$pre_mcs[bootstrap_data$treat==1],
                                          bootstrap_data$post12m_mcs[bootstrap_data$treat==1])),
                          wt = bootstrap_data$w.mcs[bootstrap_data$treat==1],
                          cor = T)
      # expected k for mcs
      k_exp.mcs.oai11 <- s_oai.mcs$cov[2,1]/s_tkr.mcs$cov[2,1] * sqrt(s_tkr.mcs$cov[2,2])/sqrt(s_oai.mcs$cov[1,1]) # sigma2/sigma1
 
      fit0.mcs <- lm(pre_mcs~age_5 + bmi_5 + gender + pre_mccom_index_c1,
                     data=bootstrap_data  %>% filter(treat==0), weights = w.mcs)
      
      fit1.mcs <- lm(post12m_mcs~age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                     data=bootstrap_data  %>% filter(treat==0), weights = w.mcs)
      
      
      # expected m for mcs
      m_exp.mcs <- sum(coef(fit1.mcs)[-1]*coef(fit0.mcs)[-1])/
        sum((coef(fit0.mcs)[-1])^2)
      }
    {# Bootstrap 2: prepost function
      fit_prepost.mcs <- prepost1(post12m_mcs~pre_mcs + age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                              bootstrap_data %>% filter(treat==1),
                              k=1,
                              m=1,
                              boot.method = "basic",
                              change = TRUE)
      
      bootstrap_data$change <- bootstrap_data$post12m_mcs - bootstrap_data$pre_mcs
      fit_wgt <- lm(change~as.factor(treat)*(pre_mcs + age_5 + bmi_5 + gender + pre_mccom_index_c1),
                    bootstrap_data, weights = w.mcs)
      
      a1 <- fit_wgt$coefficients[3] 
      ax <- fit_wgt$coefficients[4:8]
      
      opt_km.mcs <- findkandm1(a1=a1, 
                                 ax=ax, rho=fit_prepost.mcs$rho, R2=fit_prepost.mcs$r2, 
                                 coef_1=fit_prepost.mcs$coef_1)
      
      fit_prepost_estimated <- prepost1(post12m_mcs~pre_mcs + age_5 + bmi_5 + gender + pre_mccom_index_c1, 
                                        bootstrap_data %>% filter(treat==1),
                                        k=k_exp.mcs.oai11,
                                        m=m_exp.mcs,
                                        boot.method = "basic",
                                        change = TRUE)
      
      
      coef_weight.mcs <- fit_wgt$coefficients
      coef_prepost.mcs <- fit_prepost_estimated$corrected.beta     
      }
    gc(verbose = T)
    data.frame(s_oai.pcs.prevar = s_oai.pcs$cov[1,1],
               s_oai.pcs.postvar = s_oai.pcs$cov[2,2],
               s_oai.pcs.cov = s_oai.pcs$cov[1,2],
               s_tkr.pcs.prevar = s_tkr.pcs$cov[1,1],
               s_tkr.pcs.postvar = s_tkr.pcs$cov[2,2],
               s_tkr.pcs.cov = s_tkr.pcs$cov[1,2],
               #pooled_var.pcs = pooled_var.pcs,
               #k_exp.pcs.tkr11 = k_exp.pcs.tkr11,
               k_exp.pcs.oai11 = k_exp.pcs.oai11,
               #k_exp.pcs.pooled = k_exp.pcs.pooled,
               m_exp.pcs = m_exp.pcs,
               k_optim.pcs = opt_km.pcs[1],
               m_optim.pcs = opt_km.pcs[2],
               pcs.min.pval = pcs.min.pval,
               mcs.min.pval = mcs.min.pval,
               coef_weight_b1.pcs = coef_weight.pcs[9],
               coef_weight_b2.pcs = coef_weight.pcs[10],
               coef_weight_b3.pcs = coef_weight.pcs[11],
               coef_weight_b4.pcs = coef_weight.pcs[12],
               coef_weight_b5.pcs = coef_weight.pcs[13],
               coef_weight_b6.pcs = coef_weight.pcs[14],
               coef_prepost_b1.pcs = coef_prepost.pcs[2],
               coef_prepost_b2.pcs = coef_prepost.pcs[3],
               coef_prepost_b3.pcs = coef_prepost.pcs[4],
               coef_prepost_b4.pcs = coef_prepost.pcs[5],
               coef_prepost_b5.pcs = coef_prepost.pcs[6],
               coef_prepost_b6.pcs = coef_prepost.pcs[7],
               s_oai.mcs.prevar = s_oai.mcs$cov[1,1],
               s_oai.mcs.postvar = s_oai.mcs$cov[2,2],
               s_oai.mcs.cov = s_oai.mcs$cov[1,2],
               s_tkr.mcs.prevar = s_tkr.mcs$cov[1,1],
               s_tkr.mcs.postvar = s_tkr.mcs$cov[2,2],
               s_tkr.mcs.cov = s_tkr.mcs$cov[1,2],
               #pooled_var.mcs = pooled_var.mcs, 
               #k_exp.mcs.tkr11 = k_exp.mcs.tkr11,
               k_exp.mcs.oai11 = k_exp.mcs.oai11,
               #k_exp.mcs.pooled = k_exp.mcs.pooled,
               m_exp.mcs = m_exp.mcs,
               k_optim.mcs = opt_km.mcs[1],
               m_optim.mcs = opt_km.mcs[2],
               coef_weight_b1.mcs = coef_weight.mcs[9],
               coef_weight_b2.mcs = coef_weight.mcs[10],
               coef_weight_b3.mcs = coef_weight.mcs[11],
               coef_weight_b4.mcs = coef_weight.mcs[12],
               coef_weight_b5.mcs = coef_weight.mcs[13],
               coef_weight_b6.mcs = coef_weight.mcs[14],
               coef_prepost_b1.mcs = coef_prepost.mcs[2],
               coef_prepost_b2.mcs = coef_prepost.mcs[3],
               coef_prepost_b3.mcs = coef_prepost.mcs[4],
               coef_prepost_b4.mcs = coef_prepost.mcs[5],
               coef_prepost_b5.mcs = coef_prepost.mcs[6],
               coef_prepost_b6.mcs = coef_prepost.mcs[7]
               )
  }
  t1 <- Sys.time()
  t1-t0
  stopCluster(cl)
}

#write.csv(boot_res, "/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Bootstrap Method Try/results_20241211.csv") 
  


###############################################################################
## 4. Results and plots
dat_s4 <- read.csv("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Bootstrap Method Try/results_20241211.csv") 

#dat_s4 <- boot_res
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

c1 <- c(dat_s4$k_exp.pcs.pooled, dat_s4$k_optim.pcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p1 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - k Pooled") + 
  xlim(1,2) + 
  ylim(0,5)
p1

c1 <- c(dat_s4$k_exp.pcs.oai11, dat_s4$k_optim.pcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p2 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - k OAI11") + 
  xlim(1,2) + 
  ylim(0,5)
p2

c1 <- c(dat_s4$k_exp.pcs.tkr11, dat_s4$k_optim.pcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p3 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - k TKR11") + 
  xlim(1,2) + 
  ylim(0,5)
p3

c1 <- c(dat_s4$k_exp.mcs.pooled, dat_s4$k_optim.mcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p4 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - k Pooled") + 
  xlim(0.4,1.4) + 
  ylim(0,5)
p4

c1 <- c(dat_s4$k_exp.mcs.oai11, dat_s4$k_optim.mcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p5 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - k OAI11") + 
  xlim(0.4,1.4) + 
  ylim(0,5)
p5

c1 <- c(dat_s4$k_exp.mcs.tkr11, dat_s4$k_optim.mcs)
c2 <- c(rep("k_exp", 1000), rep("k_optim", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p6 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - k TKR11") + 
  xlim(0.4,1.4) + 
  ylim(0,5)
p6
grid.arrange(p1,p2,p3,p4, p5, p6, nrow=2)



## Figure X for paper: k and m distributions for PCS and MCS

c1 <- c(dat_s4$k_exp.pcs.oai11, dat_s4$k_optim.pcs)
c2 <- c(rep("Estimated", 1000), rep("Optimal", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p1 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - k") + 
  xlab("k") +
  #xlim(1,2) + 
  #ylim(0,5) + 
  theme(legend.title = element_blank())
p1

c1 <- c(dat_s4$k_exp.mcs.oai11, dat_s4$k_optim.mcs)
c2 <- c(rep("Estimated", 1000), rep("Optimal", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p2 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - k") + 
  xlab("k") 
  #xlim(0.5,1.5) + 
  #ylim(0,5)
p2

c1 <- c(dat_s4$m_exp.pcs, dat_s4$m_optim.pcs) 
c2 <- c(rep("Estimated", 1000), rep("Optimal", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p3 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - m") + 
  xlab("m") 
  #xlim(0,2.5) + 
  #ylim(0,2)
p3

c1 <- c(dat_s4$m_exp.mcs, dat_s4$m_optim.mcs) 
c2 <- c(rep("Estimated", 1000), rep("Optimal", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p4 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - m") +
  xlab("m")
p4

mylegend <- g_legend(p1)
km_dist <- grid.arrange(arrangeGrob(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  nrow=2),
  mylegend, nrow=1, widths=c(6, 1))

ggsave("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Output/km_dist.png",
   km_dist, 
   height = 5, width = 7, dpi = 300)

## Figure X and X for paper: coefficients distributions

c1 <- c(dat_s4$coef_weight_b2.pcs, dat_s4$coef_prepost_b2.pcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p2 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - Age") +
  xlab("coef") +
  theme(legend.title = element_blank())
p2

c1 <- c(dat_s4$coef_weight_b3.pcs, dat_s4$coef_prepost_b3.pcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p3 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - BMI")+
  xlab("coef")
p3

c1 <- c(dat_s4$coef_weight_b4.pcs, dat_s4$coef_prepost_b4.pcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p4 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - Gender: Male")+
  xlab("coef")
p4

c1 <- c(dat_s4$coef_weight_b5.pcs, dat_s4$coef_prepost_b5.pcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p5 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - Comorbidity: =1")+
  xlab("coef")
p5

c1 <- c(dat_s4$coef_weight_b6.pcs, dat_s4$coef_prepost_b6.pcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p6 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("PCS - Comorbidity: >=2")+
  xlab("coef")
p6



mylegend <- g_legend(p2)
pcs_coef <- grid.arrange(arrangeGrob(
                         p2 + theme(legend.position="none"),
                         p3 + theme(legend.position="none"),
                         p4 + theme(legend.position="none"),
                         p5 + theme(legend.position="none"),
                         p6 + theme(legend.position="none"),
                         nrow=2),
             mylegend, nrow=1, widths=c(10, 1))

ggsave("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Output/PCS_coef.png",
       pcs_coef, 
       height = 5, width = 11, dpi = 300)

c1 <- c(dat_s4$coef_weight_b1.mcs, dat_s4$coef_prepost_b1.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p1 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - Pre")+
  xlab("coef") +
  theme(legend.title = element_blank())
p1

c1 <- c(dat_s4$coef_weight_b2.mcs, dat_s4$coef_prepost_b2.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p2 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - Age")+
  xlab("coef") +
  theme(legend.title = element_blank())
p2

c1 <- c(dat_s4$coef_weight_b3.mcs, dat_s4$coef_prepost_b3.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p3 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - BMI")+
  xlab("coef")
p3

c1 <- c(dat_s4$coef_weight_b4.mcs, dat_s4$coef_prepost_b4.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p4 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - Gender: Male")+
  xlab("coef")
p4

c1 <- c(dat_s4$coef_weight_b5.mcs, dat_s4$coef_prepost_b5.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p5 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - Comorbidity: =1")+
  xlab("coef")
p5

c1 <- c(dat_s4$coef_weight_b6.mcs, dat_s4$coef_prepost_b6.mcs)
c2 <- c(rep("Two-arm", 1000), rep("Single-arm Correction", 1000))
df <- data.frame(bind_cols(c1=c1,c2=c2))
p6 <- ggplot(df, aes(c1, colour = c2)) +
  geom_density() + 
  ggtitle("MCS - Comorbidity: >=2")+
  xlab("coef")
p6

mylegend <- g_legend(p2)
mcs_coef <- grid.arrange(arrangeGrob(
                                     p2 + theme(legend.position="none"),
                                     p3 + theme(legend.position="none"),
                                     p4 + theme(legend.position="none"),
                                     p5 + theme(legend.position="none"),
                                     p6 + theme(legend.position="none"),
                                     nrow=2),
                         mylegend, nrow=1, widths=c(10, 1))

ggsave("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Output/MCS_coef.png",
       mcs_coef, 
       height = 5, width = 11, dpi = 300)



basic_ci <- function(b0=0, vec=NULL){
  2*b0 - quantile(vec, c(0.025, 0.975))
}


# Table Coef
basic_ci(b0 = coef_weight.pcs[9], vec = dat_s4$coef_weight_b1.pcs)
basic_ci(b0 = coef_weight.pcs[10], vec = dat_s4$coef_weight_b2.pcs)
basic_ci(b0 = coef_weight.pcs[11], vec = dat_s4$coef_weight_b3.pcs)
basic_ci(b0 = coef_weight.pcs[12], vec = dat_s4$coef_weight_b4.pcs)
basic_ci(b0 = coef_weight.pcs[13], vec = dat_s4$coef_weight_b5.pcs)
basic_ci(b0 = coef_weight.pcs[14], vec = dat_s4$coef_weight_b6.pcs)

basic_ci(b0 = coef_prepost.pcs[2], vec = dat_s4$coef_prepost_b1.pcs)
basic_ci(b0 = coef_prepost.pcs[3], vec = dat_s4$coef_prepost_b2.pcs)
basic_ci(b0 = coef_prepost.pcs[4], vec = dat_s4$coef_prepost_b3.pcs)
basic_ci(b0 = coef_prepost.pcs[5], vec = dat_s4$coef_prepost_b4.pcs)
basic_ci(b0 = coef_prepost.pcs[6], vec = dat_s4$coef_prepost_b5.pcs)
basic_ci(b0 = coef_prepost.pcs[7], vec = dat_s4$coef_prepost_b6.pcs)

basic_ci(b0 = coef_weight.mcs[9], vec = dat_s4$coef_weight_b1.mcs)
basic_ci(b0 = coef_weight.mcs[10], vec = dat_s4$coef_weight_b2.mcs)
basic_ci(b0 = coef_weight.mcs[11], vec = dat_s4$coef_weight_b3.mcs)
basic_ci(b0 = coef_weight.mcs[12], vec = dat_s4$coef_weight_b4.mcs)
basic_ci(b0 = coef_weight.mcs[13], vec = dat_s4$coef_weight_b5.mcs)
basic_ci(b0 = coef_weight.mcs[14], vec = dat_s4$coef_weight_b6.mcs)

basic_ci(b0 = coef_prepost.mcs[2], vec = dat_s4$coef_prepost_b1.mcs)
basic_ci(b0 = coef_prepost.mcs[3], vec = dat_s4$coef_prepost_b2.mcs)
basic_ci(b0 = coef_prepost.mcs[4], vec = dat_s4$coef_prepost_b3.mcs)
basic_ci(b0 = coef_prepost.mcs[5], vec = dat_s4$coef_prepost_b4.mcs)
basic_ci(b0 = coef_prepost.mcs[6], vec = dat_s4$coef_prepost_b5.mcs)
basic_ci(b0 = coef_prepost.mcs[7], vec = dat_s4$coef_prepost_b6.mcs)


# Forest plot
library(tidyverse)
library(gt)
forest_data <- openxlsx::read.xlsx("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Output/forest_plot_data.xlsx", sheet = 1)
forest_data$Variable <- factor(forest_data$Variable, 
                               levels = c("Comorbidity:>=2","Comorbidity:=1", "Comorbidity:=0",
                                          "Gender:Male",  "Gender:Female",
                                          "BMI_5","Age_5", "Baseline"))
forest_data$Method <- factor(forest_data$Method, 
                             levels = c("Two-arm weighted", "Single-arm corrected"))
pcs_forest_data <- forest_data %>% filter(Function %in% "PCS")
mcs_forest_data <- forest_data %>% filter(Function %in% "MCS")

barCOLS = c("black","black")
dotCOLS = c("red","blue")


pcs <- ggplot(pcs_forest_data, aes(x=Variable, y=as.numeric(Mean), 
                                   ymin=as.numeric(Lower), ymax=as.numeric(Upper),
                                   col=Method, fill=Method)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.5), key_glyph = "path") +
  geom_hline(yintercept=0, lty=2, key_glyph = "path") +
  #specify position here too
  geom_point(size=1.5, shape=21, colour="white", stroke = 0.1,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values= barCOLS,
                    breaks = c("Single-arm corrected", 
                               "Two-arm weighted"))+
  scale_color_manual(values=dotCOLS,
                     breaks = c("Single-arm corrected", 
                                "Two-arm weighted"))+
  scale_x_discrete(name="Variable") +
  scale_y_continuous(name="Coefficient", limits = c(-4, 3)) +
  coord_flip() +
  ggtitle("PCS") +
  theme_minimal() 
pcs

mcs <- ggplot(mcs_forest_data, aes(x=Variable, y=as.numeric(Mean), 
                                   ymin=as.numeric(Lower), ymax=as.numeric(Upper),
                                   col=Method, fill=Method)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=1.5, shape=21, colour="white", stroke = 0.1,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values= barCOLS,
                    breaks = c("Single-arm corrected", 
                               "Two-arm weighted"))+
  scale_color_manual(values=dotCOLS,
                     breaks = c("Single-arm corrected", 
                                "Two-arm weighted"))+
  scale_x_discrete(name="Variable") +
  scale_y_continuous(name="Coefficient", limits = c(-4, 3)) +
  coord_flip() +
  ggtitle("MCS") +
  theme_minimal()
mcs


mylegend <- g_legend(pcs)
pcsmcs <- grid.arrange(arrangeGrob(
  pcs + theme(legend.position="none"),
  mcs + theme(legend.position="none"),
  nrow=1),
  mylegend, nrow=1, widths=c(10, 2))

ggsave("/Users/jzi7316/Library/CloudStorage/OneDrive-JohnsHopkins/COAH/Frailty/TJR_OAI/Output/tab3.png",
       pcsmcs, 
       height = 5, width = 11, dpi = 300)
