library(Sleuth3)
help(package = Sleuth3)

# Case study 21.1.1
attach(case2101)
head(case2101)

# Plot empirical logits against Area
elogit <- log(Extinct/(AtRisk - Extinct))
plot(Area, elogit)

# Plot empirical logits against log(Area)
plot(log(Area), elogit)

#create two column response: (successes, size)
resp <- cbind(Extinct, AtRisk - Extinct)

# fit binomial logistic regression model
glm1 <- glm(resp ~ log(Area), family = binomial)
summary(glm1)

# get estimated logit values
logit_hat <- glm1$linear.predictors

# compute residuals
pear_resid <- residuals(glm1, type = "pearson")
dev_resid <- residuals(glm1, type = "deviance")

# Pearson and Deviance residuals are very similar
cbind(pear_resid, dev_resid)
plot(pear_resid, dev_resid)
cor(pear_resid, dev_resid)

#plot residuals vs fitted values
plot(logit_hat, pear_resid, xlab = "Linear Predictors", ylab = "Pearson Residuals")
plot(logit_hat, dev_resid, xlab = "Linear Predictors", ylab = "Deviance Residuals")

detach(case2101)
