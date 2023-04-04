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

## Model with all interactions
model.full <- glm(data~LA+NG+PF+GO
                  +LA:NG+LA:PF+LA:GO
                  +NG:PF+NG:GO+PF:GO, family=poisson(link="log"))

library(MASS)

# Model selection based on AIC (backward)
stepAIC(model.full, direction="both") 

beta_0_hat <- rep(0, 4)
beta_0_se <- rep(0, 4)
coef.data <- data.frame(beta_0_hat, beta_0_se)
model1 <- glm(data~LA+NG+PF+GO
              +LA:NG+LA:PF+LA:GO
              +NG:PF+NG:GO+PF:GO, family=poisson(link="log"))
coef.data$beta_0_hat[1] <- summary(model1)$coefficients[1,]["Estimate"]
coef.data$beta_0_se[1] <- summary(model1)$coefficients[1,]["Std. Error"]
model2 <- glm(data~LA + NG + PF + GO 
              + LA:NG + LA:PF + LA:GO + NG:PF + NG:GO, family=poisson(link="log"))
coef.data$beta_0_hat[2] <- summary(model2)$coefficients[1,]["Estimate"]
coef.data$beta_0_se[2] <- summary(model2)$coefficients[1,]["Std. Error"]
model3 <- glm(data~LA + NG + PF + GO 
              + LA:NG + LA:PF + NG:PF + NG:GO, family=poisson(link="log"))
coef.data$beta_0_hat[3] <- summary(model3)$coefficients[1,]["Estimate"]
coef.data$beta_0_se[3] <- summary(model3)$coefficients[1,]["Std. Error"]
model4 <- glm(data~LA + NG + PF + GO 
              + LA:NG + LA:PF + NG:GO, family=poisson(link="log"))
coef.data$beta_0_hat[4] <- summary(model4)$coefficients[1,]["Estimate"]
coef.data$beta_0_se[4] <- summary(model4)$coefficients[1,]["Std. Error"]

## P-value for goodness of fit
p.val <- rep(0, 4)
p.val[1] <- 1-pchisq(summary(model1)$deviance, summary(model1)$df.residual)
p.val[2] <- 1-pchisq(summary(model2)$deviance, summary(model2)$df.residual)
p.val[3] <- 1-pchisq(summary(model3)$deviance, summary(model3)$df.residual)
p.val[4] <- 1-pchisq(summary(model4)$deviance, summary(model4)$df.residual)
# estimates
y_hat.data <- rep(0, 4)
y_hat.data[1] <- exp(coef.data$beta_0_hat[1])
y_hat.data[2] <- exp(coef.data$beta_0_hat[2])
y_hat.data[3] <- exp(coef.data$beta_0_hat[3])
y_hat.data[4] <- exp(coef.data$beta_0_hat[4])
# standard error
N_se.data <- rep(0, 4)
N_se.data[1] <- y_hat.data[1]*coef.data$beta_0_se[1]
N_se.data[2] <- y_hat.data[2]*coef.data$beta_0_se[2]
N_se.data[3] <- y_hat.data[3]*coef.data$beta_0_se[3]
N_se.data[4] <- y_hat.data[4]*coef.data$beta_0_se[4]
# delta AIC
AIC.data <- rep(0, 4)
AIC.data[1] <- model1$aic-model4$aic
AIC.data[2] <- model2$aic-model4$aic
AIC.data[3] <- model3$aic-model4$aic

df <- cbind(y_hat.data, N_se.data, p.val, AIC.data)
colnames(df) <- c("Estimates","Standard error","P-value", "delta AIC")
rownames(df) <- c("full model","With interations LA:NG+LA:PF+LA:GO+NG:PF+NG:GO",
                  "With interations LA:NG+LA:PF+NG:PF+NG:GO",
                  "With interations LA:NG+LA:PF+NG:GO")
res.table<- as.table(df)
round(res.table, 3)
