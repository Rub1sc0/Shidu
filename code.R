library(tidyverse)
library(readxl)
library(splines2)

# Load data
dat <- read_excel("101.xlsx")
colnames(dat) = c("sex", "age", "BMI", "education", "memory", "leftDC", 
                   "rightDC", "cortisol")
dat <- dat |> mutate(
          sex = case_when(
            sex == "女" ~ "Female",
            sex == "男" ~ "Male")
        ) |> mutate(sex = factor(sex),
                    DC = (leftDC + rightDC) / 2)


# EDA
# view three histograms together
par(mfrow = c(3, 1))
hist(dat$leftDC)
hist(dat$rightDC)
hist(dat$DC, main = "Histogram of mean DC", xlab = "Mean DC")
# plot scatter plots across each variable
pairs(subset(dat, select = c(leftDC, rightDC, DC))) 
cor(dat$leftDC, dat$rightDC)
# strong correlation between leftDC and rightDC (r = 0.64), so we can use the 
# average of the two as the outcome variable


# Choose covariates
summary(lm(rightDC ~ cortisol + sex + age + BMI + memory + education, data = dat))
summary(lm(leftDC ~ cortisol + sex + age + BMI + memory + education, data = dat))
summary(lm(DC ~ cortisol + sex + age + BMI + memory + education, data = dat))
# Find importance values of covariates
library(vip)
vip(lm(DC ~ cortisol + sex + age + BMI + memory + education, data = dat))
# based on plot, BMI, sexMale, memory, cortisol are important covariates
summary(lm(DC ~ sex + BMI + memory + cortisol, data = dat))

summary(lm(DC ~ sex + BMI + cortisol, data = dat))

# stepwise selection
mod_full <- lm(DC ~ sex + BMI + I(BMI^2) + as.factor(memory) + cortisol +
                 I(cortisol^2), data = dat)
summary(step(mod_full, direction = "both"))

# Model selection
mod1 <- lm(DC ~ sex + BMI + memory + cortisol, data = dat)
mod2 <- lm(DC ~ sex + BMI + I(BMI^2) + as.factor(memory) + cortisol +
             I(cortisol^2) + I(cortisol^3), data = dat)
mod3 <- lm(DC ~ sex + BMI + I(BMI^2) + as.factor(memory) +
             bSpline(cortisol, df = 6), data = dat)
mod4 <- lm(DC ~ sex + BMI + I(BMI^2) + memory +
             bSpline(cortisol, df=6), data = dat)
mod5 <- lm(DC ~ sex + BMI + I(BMI^2) + memory + cortisol, data = dat)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
anova(mod1, mod2)
AIC_tab <- data.frame(mod1 = AIC(mod1), mod2 = AIC(mod2), mod3 = AIC(mod3), 
                      mod4 = AIC(mod4), mod5 = AIC(mod5))
rownames(AIC_tab) <- "AIC"
AIC_tab

par(mfrow = c(1,1))
library(car)
influencePlot(mod1)
influenceIndexPlot(mod1)
dat[c(39,91,23,59),]

dat$Y <- rep(1, nrow(dat))
dat$Y[which(dat$DC >= mean(dat$DC))] <- 1
dat$Y[which(dat$DC < mean(dat$DC))] <- 0

mod_lg1 <- glm(Y ~ cortisol + sex + age + BMI + memory + education, 
             family=binomial(),data = dat)
summary(mod_lg1)

# model selection
mod_lg2 <- glm(Y ~ cortisol + sex + BMI + memory, family=binomial(),data = dat)
mod_lg3 <- glm(Y ~ cortisol + I(cortisol^2) + sex + BMI + I(BMI^2) + 
                 as.factor(memory), family=binomial(),data = dat)
mod_lg4 <- glm(Y ~ cortisol + I(cortisol^2) + sex + BMI + I(BMI^2) + memory, 
               family=binomial(),data = dat)

summary(mod_lg2)$coef
summary(mod_lg3)
summary(mod_lg4)
AIC_tab <- data.frame(mod2 = AIC(mod_lg2), mod3 = AIC(mod_lg3), mod4 = AIC(mod_lg4))
rownames(AIC_tab) <- "AIC"
AIC_tab

# Model diagnostics
influencePlot(mod_lg2)
influenceIndexPlot(mod_lg2)
dat[c(74, 16, 59),]


#goodness of fit
library(ResourceSelection)
options(digits=7)
# Hosmer-Lemeshow Test 
hoslem.test(mod_lg2$y,fitted(mod_lg2),g=5)



