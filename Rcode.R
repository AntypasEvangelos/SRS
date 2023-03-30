data <- c(1, 1, 1, 15, 0, 3, 19, 54, 4, 19, 62, 464, 76, 703, 1006) # Read in the data
LA <- c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0) # Read in the values for A (i values) for given data
NG <- c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0) # Read in the values for B (j values) for given data
PF <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1) # Read in the values for C (k values) for given data
GO <- c(1,1,0,0,1,1,0,0,1,1,0,0,1,1,0)

library(lmtest)
model.full <- glm(data~LA+NG+PF+GO
                  +LA:NG+LA:PF+LA:GO
                  +NG:PF+NG:GO+PF:GO, family=poisson(link="log"))
summary(model.full)

# compared with non interactions
model.reduced <- glm(data~LA+NG+PF+GO, family=poisson(link="log"))
lrtest(model.full, model.reduced)

# model without last 4 interactions
model.1 <- glm(data~LA+NG+PF+GO
               +LA:GO+NG:PF+NG:GO+PF:GO, family=poisson(link="log"))
lrtest(model.full, model.1)