library(Sleuth3)
help(package = Sleuth3)

# Case study 21.1.1
attach(case2101)
head(case2101)

#create two column response: (successes, size)
resp <- cbind(Extinct, AtRisk - Extinct)

# fit binomial logistic regression model
glm1 <- glm(resp ~ log(Area), family = binomial)

# Wald's test
summary(glm1)
(z <- -0.297/0.05485)
2*pnorm(abs(z), lower.tail = F)

# Wald's CI
-0.297 + c(-1,1)*qnorm(0.975)*0.05485

#drop in deviance test (for comparing with null model)
summary(glm1)
(LRT <- 45.338 - 12.062)
pchisq(LRT, df = 1, lower.tail = F)

# Deviance goodness-of-fit test
#by hand
summary(glm1)
dev <- 12.062
df <- 16
#note: df = n - p - 1, as we verify in the next line
nrow(case2101) - 2

#p-value
pchisq(dev, df = df, lower.tail = F)

#checking the size for each observation
range(case2101$AtRisk)

detach(case2101)
