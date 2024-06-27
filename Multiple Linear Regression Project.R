library(tidyverse)
library(broom)
theme_set(theme_classic())
housing <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\UCF\\STA4164\\housing_prices_dataset.csv")
housing[is.na(housing)] <- 0
housingmodeling <- lm(formula = housing$Price ~ housing$SquareFeet + housing$Bedrooms + housing$Bathrooms + housing$Neighborhood + housing$YearBuilt)
housingmodeling
model.diag.metrics <- augment(housingmodeling)

fullmodeling <- lm(formula = housing$Price ~ housing$SquareFeet + housing$Bedrooms + housing$Bathrooms + housing$Neighborhood + housing$YearBuilt)
fullmodeling


head(model.diag.metrics)
broom::glance(housingmodeling)


library(car)

par(mfrow = c(2, 2))
plot(housingmodeling)
plot(housingmodeling)
plot(housingmodeling, 1)
plot(housingmodeling, 2)
plot(housingmodeling, 3)
plot(housingmodeling, 4)
plot(housingmodeling, 5)

summary(housingmodeling)



library(tidyverse)
library(leaps)
library(MASS)

# Stepwise regression model
step.housingmodeling <- stepAIC(housingmodeling, direction = "both", 
                                trace = FALSE)
summary(step.housingmodeling)

#define intercept-only model
intercept_only <- lm(housing$Price ~ 1, data=housing)

#perform forward stepwise regression
housingforwarded <- step(intercept_only, direction='forward', scope=formula(housingmodeling), trace=0)

#view results of forward stepwise regression
housingforwarded$anova

broom::glance(housingforwarded)


#view final model
housingforwarded$coefficients

housingmodelingbackwardelims <- lm(formula = housing$Price ~ housing$SquareFeet + housing$Bedrooms + housing$Bathrooms)
housingmodelingbackwardelims
summary(housingmodelingbackwardelims)
broom::glance(housingmodelingbackwardelims)

modelresidualss = housingmodeling$residuals
qqnorm(modelresidualss)
qqline(modelresidualss)

plot(housing$Price, housing$SquareFeet, main="Price x SquareFeet", xlab="Price", ylab="Square Feet")
plot(housing$Price, housing$Bedrooms, main="Price x Bedrooms", xlab="Price", ylab="Bedrooms")
plot(housing$Price, housing$Bathrooms, main="Price x Bathrooms", xlab="Price", ylab="Bathrooms")
plot(housing$Price, housing$YearBuilt, main="Price x YearBuilt", xlab="Price", ylab="YearBuilt")

boxplot(housing$SquareFeet,
        main = "SquareFeet Boxplot",
        ylab = "Values",
        xlab = "SquareFeet",
        col = "blue",
        border = "black"
)

boxplot(housing$Bedrooms,
        main = "Bedrooms Boxplot",
        ylab = "Values",
        xlab = "Bedrooms",
        col = "blue",
        border = "black"
)

boxplot(housing$Bathrooms,
        main = "Bathrooms Boxplot",
        ylab = "Values",
        xlab = "Bathrooms",
        col = "blue",
        border = "black"
)

boxplot(housing$YearBuilt,
        main = "YearBuilt Boxplot",
        ylab = "Values",
        xlab = "YearBuilt",
        col = "blue",
        border = "black"
)

#create histogram of residuals
ggplot(data = housing, aes(x = housingmodeling$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

library(car)
vif(housingmodeling)
vif(housingmodelingbackwardelims)
vif(housingforwarded)

library(olsrr)

#calculate Mallows' Cp for each model
ols_mallows_cp(housingforwarded, housingmodeling)
ols_mallows_cp(housingmodelingbackwardelims, housingmodeling)
ols_mallows_cp(fullmodeling, housingmodeling)

library(olsrr)

#define the variables we want to include in the correlation matrix
dataz <- housing[ , c("SquareFeet", "Bedrooms", "Bathrooms", "YearBuilt")]

library(corrplot)

#create correlation matrix
cor(dataz)
corrplot(cor(dataz), method="color")

summary(housing)


anova(housingmodeling)
anova(housingmodelingbackwardelims)
anova(housingforwarded)

