# plot Poisson densities
plotit <- function(xs, lambda){
    probs <- dpois(x = xs, lambda = lambda)
    barplot(names = xs, height = probs, main = paste0("lambda = ", lambda))
}

par(mfrow = c(2,2))

plotit(1:15, 1)
plotit(1:15, 2)
plotit(1:15, 5)
plotit(1:15, 10)

par(mfrow=c(1,1))

#Case 22.1.1 Elephant data
library(Sleuth3)
attach(case2201)
str(case2201)

#empirical log plot
plot(Age, log(Matings + 0.5)) #adding 0.5 since some counts are 0

#fit log-linear model and check residuals
lm1 <- lm(log(Matings + 0.5) ~ Age) #note have to add 0.5 to avoid taking log(0)

plot(Age, Matings)
lines(Age, exp(lm1$fitted.values) - 0.5) #remember that the fitted values are in terms of log(Matings + 0.5)

plot(lm1$fitted.values, lm1$residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(a = 0, b = 0)

#fit Poisson regression model
glm1 <- glm(Matings ~ Age, family = poisson )
summary(glm1)

#compare log-linear and Poisson fits
library(ggplot2)
ggplot(data = case2201) + geom_point(aes(x = Age, y = Matings)) + 
    geom_line(aes(Age, exp(lm1$fitted.values) - 0.5, colour = "log-linear")) +
    geom_line(aes(Age, glm1$fitted.values, colour = "Poisson")) + 
    scale_color_manual(values = c("orange", "skyblue")) + 
    theme_classic()

#plot deviance residuals
log_hat <- glm1$linear.predictors
dev_resid <- residuals(glm1, type = "deviance")
plot(log_hat, dev_resid, xlab = "Linear Predictors", ylab = "Deviance Residuals")

#perform Wald test and construct CIs
summary(glm1)

#compare models with the drop-in-deviance test
glm2 <- glm(Matings ~ Age + I(Age^2), family = poisson )
anova(glm1, glm2, test = "LRT")

detach(case2201)
