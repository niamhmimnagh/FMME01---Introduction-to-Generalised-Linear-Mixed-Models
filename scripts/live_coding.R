url<-"https://raw.githubusercontent.com/niamhmimnagh/FMME01---Introduction-to-Generalised-Linear-Mixed-Models/refs/heads/main/data/soilPH.txt"
soilPH<-read.table(url, header=TRUE)
head(soilPH)

table(soilPH$site)

range(soilPH$temperature)
# temperature values dont contain 0
# intercept - the estimated soil ph change when temperature = 0 
# model is extrapolating - its making a guess at change in soil ph when temperature is 0

range(soilPH$pH_change)
# with the normal distribution - response can be anywhere on the real line


# Visualising the relationship between temperature and ph change
library(ggplot2)

soilPH$site<-factor(soilPH$site)

ggplot(soilPH, aes(temperature, pH_change, colour=site))+
  geom_point()+
  theme_minimal()


# fixed effect model
lm_fixed<-lm(pH_change~temperature, data=soilPH)
summary(lm_fixed)

# residuals = observed - fitted
# is the median centred at 0? this indicates centred well
# looking at Q1 and Q3: the middle 50% of our residuals are roughly between -1.11 and 1.03 ph-change units
# min and max tell if there possible outliers: might be moderate outliers - might need to look at residual plots

# coefficients:
# beta0 estimate: 0.13816 - this tells us the expected ph change when temperature is 0
# at 0 degrees the model predicts an increase of 0.14 in ph
# because we dont have 0 in our temperatures - this is not a meaningful

# beta1 estimate 0.237: the expected change in our response, for a 1 degree celsius increase in temperature
# Ho: temperature does not have a significant effect on soil ph change
# Ha: temperature does have a significant effect on soil ph change
# p-value: probability of obtainin t=14.5 if Ho was true: reject the null hypothesis
# conclude that temperature does have a significant effect on soil ph change

# Residual standard error: typical size of residuals in the units of the outcome (ph change)
# residual standard error of almost 2 is quite large on the ph scale: 
#  because we are not accounting for group structure

# multiple r squared/adjusted: proportion of variance in the response explained by temperature
# both roughly 14.5%
# adjusted r squared: adjusts for the number of predictors

# F statistic: does the model with temp as a predictor fit better than an intercept model?
# Ho: temperature does not significant improve prediction compared to the intercept model
# Ha: temperature does improve things
# small p-value: reject Ho - conclude that temperature is a significant predictor.

# Random intercept model
library(lme4)
m_random_intercept<-lmer(pH_change~temperature+(1|site), data=soilPH)
summary(m_random_intercept)

# REML criterion for convergence: used for comparing models with the same fixed effects 
# but different random effect structures
# smaller is better

# residuals are again centred around 0
# min/max values are not as large as they were before: using random intercepts is improving
# model prediction

# fixed effects:
# intercept: mean response when temperature is 0
# beta1: effect of temperature: the estimated increase in soil ph change, for a 1 unit increase in temperature
# large t-value: reject Ho and conclude that temperature has a significant effect on soil ph change


# random effects:
# intercept: when reporting use standard deviation (between sites)
# standard deviation for the site level random intercept: 1.58
# random intercept measures the variation that sites experience from the baseline change (fixed intercept)
# at a given temperature, the baseline ph change for sites differs by roughly 1.58 units from our fixed 
# our fixed intercept is 0.138, so a deviation of around 1.58 units is quite large

# residual variance: within-site variation remains after youve accounted for your predictors and your random intercepts
# for 2 observations that are in the same site (within site) and at the same temperature, the typical
# difference we would expect in their response value is about 0.74 units


# because sd of the residual is smaller than the sd of the random intercept, this tells us
# most of the variation is being explained by the site level differences, and not just random scatter

# correlation of the fixed effects:
# correlation between the fixed intercept and slope:
# negative: if the model had a higher intercept it would have a lower slope in response
# typically can be ignored unless its near plus or minus 1

# Random Slopes model
m_rand_slope<-lmer(pH_change~temperature+(temperature|site), data=soilPH)

# the model wasnt able to find a good parameter set
# random slope models add extra parameters - 
# 1. centering predictors  - scale(temperature, center=TRUE)
# 2. remove correlation between slope and intercept: (temperature||site)<- assume that slope and the intercept
# are not correlated
# 3. use a more robust optimiser

m_rand_slope<-lmer(pH_change~temperature+(temperature|site), data=soilPH,
                   control=lmerControl(optimizer='bobyqa'))
summary(m_rand_slope)
# 3045 - REML criterion at convergence is smaller than the one we saw for the random intercepts

# fixed effects: 
# intercept = 0.138: predicted change in soil ph when your temperature is 0 degrees
# if temp was centered you wouldnt be interpreting in terms of 0 degrees, you'd be looking at mean temp

# temperature: 0.237: if temp increased by 1 degree, you'd see an average increase in response of 0.237
# large t value: temperature is a significant predictor of response

# random effects: 
# site level random intercept: typical difference in baseline (intercept) soil ph change when temp=0
# interpreting at the level of the standard deviation - because its using the response units
# sd 1.18: sites baseline response typically differs by plus or minus 1.18 units 
# comparing with the random intercepts: some of the variance we thought was due to 
# differing baselines is now being attributed to differing slopes

# random slope for temperature: 
# the difference in how the response changes with temperature between different sites
# typical site-specific slope deviation is plus or minus 0.044 from the overall slope (beta1)

# residual variance: the scatter you'd see within sites after accounting for your random and fixed effects
# consistent with improved fit when allowing for slope variation

# slope-intercept correlation (random effects): 0.44
# sites that have a higher baseline are  more likely to also have a more positive slope

# compare with AIC values:
AIC(lm_fixed, m_rand_slope, m_random_intercept)
# AIC: random slope model tells us for random slopes over the random intercept/fixed effects models

# caterpillar plots
dotplot(ranef(m_rand_slope), condVar=TRUE)
