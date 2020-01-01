####################
## You work for Motor Trend, a magazine about the automobile 
## industry. Looking at a data set of a collection of cars, 
## they are interested in exploring the relationship between 
## a set of variables and miles per gallon (MPG) (outcome). 
## They are particularly interested in the following two 
## questions:
##
## 1. “Is an automatic or manual transmission better for MPG”
## 2. "Quantify the MPG difference between automatic and 
##      manual transmissions" 
####################

############
#### EDA ###
############

library(tidyverse)
library(car)
library(broom)

data("mtcars")
head(mtcars)
glimpse(mtcars)

table(mtcars$cyl) ## 4, 6, 8
table(mtcars$vs)  ## 0 (v-shaped), 1 (straight)
table(mtcars$am)  ## 0 (auto), 1 (manual)
table(mtcars$gear) ## number of forward gears 
table(mtcars$carb) ## number of carburetors 

####################
## Should be factors: 
## cyl, vs, am, gear, carb 
####################

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

str(mtcars)
summary(mtcars) ## no NAs 

mtcars %>% 
    ggplot(aes(mpg)) + geom_histogram()

mtcars %>% 
    ggplot(aes(am, mpg)) + geom_boxplot() + 
    labs(title = "MPG by Transmission", x = "Transmission (0 = Automatic, 1 = Manual)", 
         y = "MPG")

mtcars %>% 
    ggplot(aes(cyl, mpg)) + geom_boxplot() + 
    facet_wrap(~am) 
    ## auto with lower mpg 

mtcars %>% 
    ggplot(aes(disp, mpg)) + geom_point()
    ## lower disp -- auto and higher mpg
    ## higher disp -- manual and lower mpg
    ## negative correlation bet. disp and mpg

mtcars %>% 
    ggplot(aes(hp, mpg)) + geom_point()
    ## negative corr. bet. hp and mpg 

mtcars %>% 
    ggplot(aes(drat, mpg)) + geom_point()
    ## positive corr. bet. drat and mpg 
    ## auto with lower drat, mpg 
    ## manual with higher drat, mpg 

mtcars %>% 
    ggplot(aes(wt, mpg)) + geom_point()
    ## negativ corr. bet. wt and mpg 
    ## auto with higher wt, lower mpg 
    ## manual with lower wt, higher mpg 

mtcars %>% 
    ggplot(aes(qsec, mpg)) + geom_point()
    ## positive corr. bet. qsec and mpg 

mtcars %>% 
    ggplot(aes(vs, mpg)) + geom_boxplot() + 
    facet_wrap(~am)

mtcars %>% 
    ggplot(aes(gear, mpg)) + geom_boxplot() + 
    facet_wrap(~am)
    ## auto with lower gears, mpg 
    ## manual with more gears, higher mpg 

mtcars %>% 
    ggplot(aes(carb, mpg)) + geom_boxplot() + 
    facet_wrap(~am)

cor(mtcars[-c(2, 8:11)]) ## correlation matrix 
    ## mpg NOT strongly correlated with qsec 
    ## (+) with drat, qsec
    ## (-) with disp, hp, wt 

mtcars %>% 
    ggplot(aes(am, disp)) + geom_boxplot()

mtcars %>% 
    ggplot(aes(am, hp)) + geom_boxplot()

mtcars %>% 
    ggplot(aes(am, drat)) + geom_boxplot()

mtcars %>% 
    ggplot(aes(am, wt)) + geom_boxplot()

mtcars %>% 
    ggplot(aes(am, qsec)) + geom_boxplot()

### Interaction plots ### 

interaction.plot(mtcars$am, mtcars$cyl, mtcars$mpg) ## yes 
interaction.plot(mtcars$am, mtcars$hp, mtcars$mpg) 
interaction.plot(mtcars$am, mtcars$qsec, mtcars$mpg) ## yes
interaction.plot(mtcars$am, mtcars$vs, mtcars$mpg) ## almost parallel
interaction.plot(mtcars$am, mtcars$carb, mtcars$mpg) ## yes

##############
### MODELS ###
##############

cyl <- summary(lm(mpg~cyl, data = mtcars))$r.squared # sig., R2 = 0.7325
disp <- summary(lm(mpg~disp, data = mtcars))$r.squared # sig., R2 = 0.7183
hp <- summary(lm(mpg~hp, data = mtcars))$r.squared # sig., R2 = 0.6024 
drat <- summary(lm(mpg~drat, data = mtcars))$r.squared # sig., R2 = 0.464
wt <- summary(lm(mpg~wt, data = mtcars))$r.squared # sig., R2 = 0.7528
qsec <- summary(lm(mpg~qsec, data = mtcars))$r.squared # sig., R2 = 0.1753
vs <- summary(lm(mpg~vs, data = mtcars))$r.squared #sig., R2 = 0.4409
am <- summary(lm(mpg~am, data = mtcars))$r.squared # sig., R2 = 0.3598
gear <- summary(lm(mpg~gear, data = mtcars))$r.squared # sig., R2 = 0.4292
carb <- summary(lm(mpg~carb, data = mtcars))$r.squared # some sig., R2 = 0.4445

rsquareds <- data.frame(cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
rsquareds <- gather(rsquareds, "Variable", "R-Squared")

## strongest = wt, hp, disp, cyl 

summary(aov(mpg ~ cyl*am, data = mtcars)) ## int. not sig. 
summary(aov(mpg ~ disp*am, data = mtcars)) ## int. sig. 
summary(aov(mpg ~ hp*am, data = mtcars)) ## int. not sig. 
summary(aov(mpg ~ drat*am, data = mtcars)) ## int. not sig. 
summary(aov(mpg ~ wt*am, data = mtcars)) ## int. sig. 
summary(aov(mpg ~ qsec*am, data = mtcars)) ## int. not sig. 
summary(aov(mpg ~ vs*am, data = mtcars)) ## interaction not sig. 
summary(aov(mpg ~ gear*am, data = mtcars)) ## no interaction?
summary(aov(mpg ~ carb*am, data = mtcars)) ## int. not sig. 

## SIG. --> disp, wt

####################
## Model selection: 
## Nested models
####################

fit1 <- lm(mpg ~ am, data = mtcars)
summary(fit1) ## sig., adj. R2 = 0.3385 
## automatic = 17.15 
## manual = 17.15 + 7.25 

fit2 <- update(fit1, mpg ~ am * wt)
vif(fit2)
anova(fit1, fit2)
summary(fit2) ## adj. R2 = 0.8151

fit3 <- update(fit1, mpg ~ am * wt + am * disp)
vif(fit3)
anova(fit2, fit3) ## not necessary
summary(fit3) ## adj. R2 = 0.8337 

cor(mtcars$disp, mtcars$wt)

fit4 <- update(fit1, mpg ~ am * wt + disp)
vif(fit4)
anova(fit2, fit4) ## necessary
summary(fit4) ## adj. R2 = 0.8399

fit5 <- update(fit1, mpg ~ am * wt + disp + cyl)
vif(fit5)
anova(fit4, fit5) ## not necessary
summary(fit5) ## adj. R2 = 0.8488 

fit6 <- update(fit1, mpg ~ am * wt + disp + hp)
vif(fit6)
anova(fit4, fit6) ## not necessary
summary(fit6) ## adj. R2 = 0.8473

fit7 <- update(fit1, mpg ~ am * wt + disp + drat)
vif(fit7)
anova(fit4, fit7) ## not necessary
summary(fit7) ## adj. R2 = 0.8346

fit8 <- update(fit1, mpg ~ am * wt + disp + qsec)
vif(fit8)
anova(fit4, fit8) ## necessary
summary(fit8) ## adj. R2 = 0.8759

fit9 <- update(fit1, mpg ~ am * wt + disp + qsec + vs)
vif(fit9)
anova(fit8, fit9) ## not necessary
summary(fit9) ## adj. R2 = 0.871

fit10 <- update(fit1, mpg ~ am * wt + disp + qsec + gear)
vif(fit10)
anova(fit8, fit10) ## not necessary
summary(fit10) ## adj. R2 = 0.8711

fit11 <- update(fit1, mpg ~ am * wt + disp + qsec + carb)
vif(fit11)
anova(fit8, fit11) ## not necessary
summary(fit11) ## adj. R2 = 0.8542


### MODEL ### 

model <- lm(mpg ~ am * wt  + disp + qsec, data = mtcars)
tidy(model)

model2 <- lm(mpg ~ am * wt  + disp + qsec -1, data = mtcars)
tidy(model)

coefs <- summary(model2)$coefficients
coefs[1,1] + c(-1, 1) * qt(.975, df = model2$df) * coefs[1, 2]
coefs[2,1] + c(-1, 1) * qt(.975, df = model2$df) * coefs[2, 2]
coefs[3,1] + c(-1, 1) * qt(.975, df = model2$df) * coefs[3, 2]
coefs[5,1] + c(-1, 1) * qt(.975, df = model2$df) * coefs[5, 2]


model.res <- resid(model)

mtcars$predicted <- predict(model)
mtcars$residuals <- residuals(model)

mtcars %>% 
    ggplot(aes(predicted, residuals)) + geom_point() + 
    geom_hline(yintercept = 0, linetype = "dotted") + 
    labs(title = "Residuals", x = "Predicted Values",
         y = "Residuals")

library(ggfortify)
autoplot(model, which = 1:2, label.size = 2)
