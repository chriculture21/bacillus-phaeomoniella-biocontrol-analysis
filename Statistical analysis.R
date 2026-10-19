#Load libraries
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(car)
library(DHARMa)
library(nlme)
library(readxl)
library(ggplot2)

#load data
df <- read_excel("directory/Data.xlsx", 
                 col_types = c("text", "text", "numeric"))

##Prepare the data
df$Treatment <- factor(df$Treatment)
df$Block     <- factor(df$Block)
df$Treatment <- relevel(df$Treatment, ref = "Pch")   # Pch as reference Exp group 1 and Control for Exp group 2

##Fit the model
#assuming equal variances
m_eq <- lme(log(AUDPC+1) ~ Treatment, random = ~1 | Block, data = df, method = "REML")

summary(m_eq)

##Normality of residuals
#Inspect the plots first
qqnorm(resid(m_eq))
qqline(resid(m_eq))
hist(resid(m_eq), breaks = 15)

#Formal tests
shapiro.test(resid(m_eq))      # W and p-value

##Homogeneity of variance
#Inspect the plots
plot(m_eq)                                   # residuals vs fitted
boxplot(resid(m_eq) ~ df$Treatment)

#formal tests
leveneTest(resid(m_eq) ~ df$Treatment, center = median)  # Brown–Forsythe version
fligner.test(resid(m_eq) ~ df$Treatment)                 # robust to non-normality
check_heteroscedasticity(m_eq)                           # Breusch–Pagan-type test

##Overall treatment effect
anova(m_eq)

##Dunnet's test
set.seed(123)
emm <- emmeans(m_eq, ~ Treatment)
dun <- contrast(emm, method = "trt.vs.ctrl", ref = "Control", adjust = "mvt")
summary(dun, infer = c(TRUE, TRUE))

##-----------------------------------------------------------------------------
#assuming unequal variances
m_het <- lme(log(AUDPC + 1) ~ Treatment, random = ~1 | Block, data = df,
             method = "REML", weights = varIdent(form = ~1 | Treatment))

summary(m_het)

##for the unequal variances model
r_het <- resid(m_het, type = "normalized")

# Normality
qqnorm(r_het)
qqline(r_het)
shapiro.test(r_het)
check_normality(r_het)

# Homogeneity
plot(m_het, resid(., type = "normalized") ~ fitted(.))
boxplot(r_het ~ df$Treatment)
leveneTest(r_het ~ df$Treatment, center = median)
fligner.test(r_het ~ df$Treatment)

anova(m_het)

##run comparisons
set.seed(123)   # "mvt" uses simulation, so this makes the p-values reproducible

emm <- emmeans(m_eq, ~ Treatment)
dun <- contrast(emm, method = "trt.vs.ctrl", ref = "Pch", adjust = "mvt")
summary(dun, infer = c(TRUE, TRUE))

summary(emm, type = "response")
