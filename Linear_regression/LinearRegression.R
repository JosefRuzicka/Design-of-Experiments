# AAC 6: Regresión Lineal

# Johan Ávalos
# Josef Ruzicka
# Jonathan Céspedes


# Baseball data Linear Regression.
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData") 
load("mlb11.RData")

# 1)
cor(mlb11$runs, mlb11$at_bats)
plot(mlb11$runs, mlb11$at_bats)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# 2) 3)
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

# 4)
plot(mlb11$at_bats, mlb11$runs)
lines(mlb11$at_bats, mlb11$at_bats *  0.6305 - 2789.2429)

# 5)
plot(mlb11$runs ~ mlb11$homeruns)
m2 <- lm(runs ~ homeruns, data = mlb11)
abline(m2)
summary(m2)

# 6)
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

## linealidad
plot(m1$residuals ~ mlb11$at_bats) 
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

# 7)
## residuos casi normales
hist(m1$residuals)

qqnorm(m1$residuals) 
qqline(m1$residuals)

# 8) 9)
## variabilidad constante
plot(m1$residuals ~ m1$fitted.values) 
abline(h = 0, lty = 3)

# 10)
plot(mlb11$runs ~ mlb11$hits)
m3 <- lm(runs ~ hits, data = mlb11)
summary(m3)
abline(m3)

# 11) Se comparan m3 y m1. Los resultados se reportan en documento.

# 12)
## bat_avg
plot(mlb11$runs ~ mlb11$bat_avg)
m4 <- lm(runs ~ bat_avg, data = mlb11)
summary(m4)
abline(m4)
# R^2: 0.6561, slope: 5242.2

## strikeouts
plot(mlb11$runs ~ mlb11$strikeouts)
m5 <- lm(runs ~ strikeouts, data = mlb11)
summary(m5)
abline(m5)
# R^2: 0.1694, slope: -0.3141

## stolen_bases
plot(mlb11$runs ~ mlb11$stolen_bases)
m6<- lm(runs ~ stolen_bases, data = mlb11)
summary(m6)
abline(m6)
# R^2: 0.002914, slope: 0.1491 

## wins
plot(mlb11$runs ~ mlb11$wins)
m7<- lm(runs ~ wins, data = mlb11)
summary(m7)
abline(m7)
# R^2: 0.361, slope: 4.341

# 13)
## new_onbase
plot(mlb11$runs ~ mlb11$new_onbase)
m8<- lm(runs ~ new_onbase, data = mlb11)
summary(m8)
abline(m8)
# R^2: 0.8491

## new_slug
plot(mlb11$runs ~ mlb11$new_slug)
m9<- lm(runs ~ new_slug, data = mlb11)
summary(m9)
abline(m9)
# R^2: 0.8932

## new_obs
plot(mlb11$runs ~ mlb11$new_obs)
m10<- lm(runs ~ new_obs, data = mlb11)
summary(m10)
abline(m10)
# R^2: 0.9349

# 14)
## Linealidad
plot(m10$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

## Normalidad de residuales
hist(m10$residuals)

qqnorm(m10$residuals)
qqline(m10$residuals) # adds diagonal line to the normal prob plot

## Variabilidad constante
plot(m10$residuals ~ m10$fitted.values)
abline(h = 0, lty = 3) 