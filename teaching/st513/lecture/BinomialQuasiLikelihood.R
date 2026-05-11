#Quasi-likelihood
library(Sleuth3)
help(package = Sleuth3)

# Case study 21.1.2
attach(case2102)
str(case2102)

#define dark variable to match textbook output
dark <- factor(Morph, levels = c("light", "dark"))

# fit a model
resp <- cbind(Removed, Placed - Removed)
glm1 <- glm(resp ~ dark*Distance, family = binomial)
summary(glm1)

# plot residuals
logit_hat <- glm1$linear.predictors
dev_resid <- residuals(glm1, type = "deviance")
plot(logit_hat, dev_resid, xlab = "Linear Predictors", ylab = "Deviance Residuals")

#testing beta_3
summary(glm1)
(dispersion <- 13.230/10)
(se3 <- sqrt(dispersion)*0.008085)
(t <- 0.027789/se3)
#two-sided p-value
2*pt(abs(t), df = 10, lower.tail = F)

#drop in deviance F test
glm2 <- glm(resp ~ dark + Distance, family = binomial)
glm1$deviance
glm2$deviance

(Fstat <- ((25.161 - 13.230)/1)/dispersion)
#p-value
pf(Fstat, df1 = 1, df2 = 10, lower.tail = F)

# use quasi-likelihood
quasiglm1 <- glm(resp ~ dark*Distance, family = quasibinomial)
summary(quasiglm1)

#note: dispersion is estimated with a different approach (appears to be the sum of squared Pearson residuals)
sum( residuals(glm1, "pearson")^2 )/10

# CI for interaction in model with 9 parameters
beta_ci <- 0.027739 + c(-1,1)*qnorm(0.975)*0.008097
(or_ci <- exp(10*beta_ci))
