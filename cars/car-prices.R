#!  /usr/bin/env Rscript

cars.file <- "cars.csv"

# import data as table
cars.df <- read.csv(cars.file, header=TRUE, sep=",", dec=".", na.strings = "?", stringsAsFactors=FALSE)
## keep only case without missing data
cars.df <- cars.df[complete.cases(cars.df), ]
## price in thousands
cars.df$price <- cars.df$price/1e3

## Plot summary information from model fitting `m`
summary.ci <- function(m)
{
    coefs <- summary(m)$coefficients
    predictors <- row.names(coefs)
    estimates <- coefs[, "Estimate"]
    errors <- coefs[, "Std. Error"]
    p0.05 <- estimates - 2*errors
    p0.95 <- estimates + 2*errors
    data.frame(predictor=predictors, estimate=estimates, error=errors, p0.05=p0.05, p0.95=p0.95)
}

## An example of no apparent dependency between variables
## plot(price ~ compression.ratio, data=cars.df)
## An example of apparent dependency between variables
## plot(price ~ horsepower, data=cars.df)

m.hp <- lm(price ~ 1 + horsepower, data=cars.df)
s.hp <- summary.ci(m.hp)

## Plot data
plot(price ~ horsepower, data=cars.df, ylab="price (k$)")
## Regression line ("average")
abline(a=s.hp[1,2], b=s.hp[2,2], col="blue", lwd=4)
## 90% confidence interval of regression coefficients
abline(a=s.hp[1,4], b=s.hp[2,4], col="gray30", lwd=2)
abline(a=s.hp[1,5], b=s.hp[2,5], col="gray30", lwd=2)


## Residuals (outcome - prediction)
res <- cars.df$price - (s.hp[1,2] +  s.hp[2,2]*cars.df$horsepower)
## Check: max(abs(res)) and mean(abs(res)) to get an idea of the accuracy of the prediction
## Plot residuals
plot(x = cars.df$horsepower, y = res, ylab="price residuals (k$)", xlab="horsepower")
## Regression line in residuals
abline(h=0)

## Variance explained R^2 = (var(outcome) - var(residuals))/var(outcome) = 1 - var(residuals)/var(outcome)
R.sq <- 1 - var(res)/var(cars.df$price)

## Centering data, to have a meaningful interpretation of the intercept
cars.df$horsepower.c <- cars.df$horsepower - mean(cars.df$horsepower)
m.hp.c <- lm(price ~ 1 + horsepower.c, data=cars.df)
s.hp.c <- summary.ci(m.hp.c)  ## Now the "intercept" is the estimated price when hp is average
plot(price ~ horsepower.c, data=cars.df, xlab="hp - mean(hp)", ylab="price (k$)")
abline(a=s.hp.c[1,2], b=s.hp.c[2,2], col="blue", lwd=4)

## The price seems also to depend on engine size
plot(price ~ engine.size, data=cars.df)
## Let's do a multivariate regression to combine both predictors
m.hp.cc <- lm(price ~ 1 + horsepower + engine.size, data=cars.df)
s.hp.cc <- summary.ci(m.hp.cc)
## R^2 has increased significantly! (use `summary` function with model)

## We cannot plot multivariate regression in one plot, but we can do
## multiple plots while fixing the other predictor There are called
## "counterfactuals" since they do not represent the actual data but
## only the prediction in hypothetical situations

## Let's put both plots in one panel
par(mfrow=c(1,2)) # 1 row 2 cols

## Fix engine.size to its mean value, and see the regression with horsepower
hps <- cars.df$horsepower
ess <- rep(mean(cars.df$engine.size), length(hps))
p.price <- s.hp.cc[1,2] + s.hp.cc[2,2]*hps + s.hp.cc[3,2]*ess
## Create the main plot without printing the dots (type n)
plot(x=hps, y=p.price, xlab="horsepower", ylab="predicted price", main=paste("engine size =", ess[1]), type="n")
## Print coordinates connected by lines
lines(x=hps, y=p.price, lwd=3, col="blue")

## Fix horsepower to its mean value, and see the regression with engine.size
ess <- cars.df$engine.size
hps <- rep(mean(cars.df$horsepower), length(ess))
p.price <- s.hp.cc[1,2] + s.hp.cc[2,2]*hps + s.hp.cc[3,2]*ess
## Create the main plot without printing the dots (type n)
plot(x=ess, y=p.price, xlab="engine size", ylab="predicted price", main=paste("horsepower =", hps[1]), type="n")
## Print coordinates connected by lines
lines(x=ess, y=p.price, lwd=3, col="red")


### Categorical variable as predictor

## Indicator variable: 1 iff turbo
cars.df$turbo  <- 0
cars.df$turbo[cars.df$aspiration == "turbo"]  <- 1

## Let's plot the data by showing different aspiration type
plot(price ~ engine.size, data=cars.df, col=as.factor(cars.df$aspiration))
legend(225, 10, unique(cars.df$aspiration), col=1:length(cars.df$aspiration), pch=1)
m.cc.asp <- lm(price ~ 1 + engine.size + turbo, data=cars.df)
s.cc.asp <- summary.ci(m.cc.asp)
## Regression line for aspiration == std (indicator variable turbo = 0)
abline(a=s.cc.asp[1,2], b=s.cc.asp[2,2], col="black", lwd=4)
## Regression line for aspiration == turbo (indicator variable urbo = 1)
abline(a=s.cc.asp[1,2]+s.cc.asp[3,2], b=s.cc.asp[2,2], col="red", lwd=4)
