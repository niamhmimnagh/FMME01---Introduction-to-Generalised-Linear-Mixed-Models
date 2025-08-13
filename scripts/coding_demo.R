###########################################
# Soil PH
###########################################

# Load required packages
library(lme4)      # for fitting mixed-effects models
library(ggplot2)   # for plotting
library(dplyr)     # for data wrangling
library(lattice)   # for caterpillar plots
library(sjPlot)    # for nice plotting of model predictions
library(ggplot2)

# 1. Import data -------------------------------------------

url<-"https://raw.githubusercontent.com/niamhmimnagh/FMME01---Introduction-to-Generalised-Linear-Mixed-Models/refs/heads/main/data/soilPH.txt"
soilPH<-read.table(url, header=TRUE)
head(soilPH)

# Check range to confirm unbounded real values
range(soilPH$pH_change)

# Plot
ggplot(soilPH, aes(temperature, pH_change, colour = site)) +
  geom_point(alpha = 0.6) 

# 2. Fit models -------------------------------------------

# 2.1 Fixed-effect model 
m_fixed <- lm(pH_change ~ temperature, data = soilPH)
summary(m_fixed)

# 2.2 Random-intercept model
m_rand_int <- lmer(pH_change ~ temperature + (1 | site), data = soilPH)
summary(m_rand_int)

# 2.3 Random-slope model (temperature effect varies by site)
m_rand_slope <- lmer(pH_change ~ temperature + (temperature | site), data = soilPH)


m_rand_slope <- lmer(
  pH_change ~ temperature + (temperature | site),
  data = soilPH,
  control = lmerControl(optimizer = "bobyqa"))


summary(m_rand_slope)


# 3. Model comparisons ------------------------------------

# Compare using AIC
AIC(m_fixed, m_rand_int, m_rand_slope)



###########################################################
# 4. Intra-class correlation (ICC) ------------------------
###########################################################
# ICC = proportion of variance explained by grouping factor

varcomp <- as.data.frame(VarCorr(m_rand_int))
icc <- varcomp$vcov[1] / (varcomp$vcov[1] + varcomp$vcov[2])
icc


# 5. Inspect random effects -------------------------------

# Caterpillar plot for random intercepts
dotplot(ranef(m_rand_int, condVar = TRUE))

# Caterpillar plot for random intercepts and slopes
dotplot(ranef(m_rand_slope, condVar = TRUE))

# Extract correlation between intercept and slope (random slope model)
attr(VarCorr(m_rand_slope)$site, "correlation")


# 6. Visualising varying intercepts and slopes ------------

# Predictions for each site from random-intercept model
# make a grid for every site across the temperature range
soilPH$site<-factor(soilPH$site)
nd <- expand.grid(
  temperature = seq(min(soilPH$temperature), max(soilPH$temperature), length.out = 100),
  site = levels(soilPH$site)
)

# include the random intercepts for site
nd$fit <- predict(m_rand_int, newdata = nd, re.form = ~(1|site))

library(ggplot2)
ggplot(nd, aes(temperature, fit, colour = site)) +
  geom_line() +
  labs(y = "Predicted pH_change")

# Predictions for each site from random-slope model
nd$fit <- predict(m_rand_slope, newdata = nd, re.form = ~(temperature | site))

library(ggplot2)
ggplot(nd, aes(temperature, fit, colour = site)) +
  geom_line() +
  labs(
    y = "Predicted pH_change"
  )

###########################################
# Frog Presence
###########################################

## Packages
library(tidyverse)
library(lme4)          # glmer()
library(broom)         # tidy() for glm
library(broom.mixed)   # tidy() for glmer
=library(ggeffects)     # effect curves

## ------------------------------------------------------------------
url<-"https://raw.githubusercontent.com/niamhmimnagh/FMME01---Introduction-to-Generalised-Linear-Mixed-Models/refs/heads/main/data/frog.txt"
frog<-read.table(url, header=TRUE)
head(frog)

## 1) Plain logistic regression (no random effects)
m_glm <- glm(presence ~ depth, data = frog, family = binomial)

## 2) GLMM: random intercept for region
m_ri  <- glmer(presence ~ depth + (1 | region), data = frog, family = binomial,
               control = glmerControl(optimizer = "bobyqa"))

## 3) GLMM: random intercepts & random slopes for depth by region
m_rs  <- glmer(presence ~ depth + (depth | region), data = frog, family = binomial,
               control = glmerControl(optimizer = "bobyqa"))

## ---- Model comparison
AIC(m_glm, m_ri, m_rs)

## ---- Fixed effects (odds ratios with 95% CI)
fx_glm <- broom::tidy(m_glm, conf.int = TRUE, exponentiate = TRUE)
fx_ri  <- broom.mixed::tidy(m_ri, effects = "fixed", conf.int = TRUE,
                            conf.method = "Wald", exponentiate = TRUE)
fx_rs  <- broom.mixed::tidy(m_rs, effects = "fixed", conf.int = TRUE,
                            conf.method = "Wald", exponentiate = TRUE)

fx_glm
fx_ri
fx_rs


## ---- Random-effects variance
vc_ri <- as.data.frame(VarCorr(m_ri))
vc_rs <- as.data.frame(VarCorr(m_rs))

vc_ri
vc_rs




