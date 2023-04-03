############################### Appendix #######################################
## This file contains the code used for Assignment 3 in Statistical Research
## Skills (MATH11188).
## Group Work:: E. Antypas (s2449453), S. Liu (s2337553),
## C. Giannikos (S2436019), Y. Gu(s2304572)

################################ Code ##########################################

## Creating dataset
data <- c(1, 1, 1, 15, 0, 3, 19, 54, 4, 19, 62, 464, 76, 703, 1006) 
## Local Authority
LA <- c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)
## Non-Government Organisation
NG <- c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0) 
## Police Force
PF <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)

## Government Organisation
GO <- c(1,1,0,0,1,1,0,0,1,1,0,0,1,1,0)

## Step-wise AIC procedure

## Model with all interactions
model.full <- glm(data~LA+NG+PF+GO
                  +LA:NG+LA:PF+LA:GO
                  +NG:PF+NG:GO+PF:GO, family=poisson(link="log"))

library(MASS)
model.select <- stepAIC(model.full, direction="both") 
## Summary
summary(model.select)

## P-value for goodness of fit
1-pchisq(summary(model.select)$deviance, summary(model.select)$df.residual)

## Residual plots
plot(model.select)


# Confidence Intervals
# Step 1: Extract the estimate and standard error of the intercept
coef <- summary(model.select)$coefficients[1,]
beta_0_hat<- coef["Estimate"]
beta_0_se <- coef["Std. Error"]

# Step 2: Calculate the estimate for N=exp(beta_0)+n
y_hat<- exp(beta_0_hat)
n <- sum(data)
N_hat <- y_hat+n

# Step 3: Calculate the standard error of N_hat
N_se <- y_hat* beta_0_se

# Step 4: Calculate the 95% confidence interval for y=exp(beta_0)
lb_ci <- N_hat -y_se*1.96 
ub_ci <- N_hat +y_se*1.96 
N_ci <- c(lb_ci,ub_ci)