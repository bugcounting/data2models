#!  /usr/bin/env Rscript

red.file <- "winequality-red.csv"
white.file <- "winequality-white.csv"

red.df <- read.csv(red.file, header=TRUE, sep=";", dec=".")
white.df <- read.csv(white.file, header=TRUE, sep=";", dec=".")

m.vacidity <- lm(quality ~ 1 + volatile.acidity, data = red.df)
m.all <- lm(quality ~ 1 + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
            data = red.df)


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

