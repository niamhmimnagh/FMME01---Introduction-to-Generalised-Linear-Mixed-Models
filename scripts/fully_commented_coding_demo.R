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
soilPH<-read.table(url, header=TRUE) # header=true tells it that the first row contains variable names
head(soilPH)
table(soilPH$site) # this tells us how many values are in our sites column,  and how many times each appears in the dataset
# so we have 50 sites, and each of them are sampled 25 times, which indicates to us that probably some form of random effect should be used
# for site, because samples that are taken from the same site are likely to be related to one another

range(soilPH$temperature) # the soil Ph change is measured at temperatures between 10 and 20 degrees
# whats important here is that 0 degrees is not in our dataset, so when models start telling us the 
# estimate for the intercept which is the response at 0 degrees, we have to take that with a grain of salt

range(soilPH$pH_change) # this tells us the change in soilPH relative to some baseline value
# we could have just looked at the soilph readings themselves rather than the changes
# but i wanted to show you that this kind of model is able to take continuous values anywhere on the real number line
# so we're using the changes in ph so we can work with some negative values
# so the ph change tells us that for some sites, soil ph has dropped by roughly 2.1 so its become more acidic
#, while in others it has increased by 10.7, so it has become much more alkaline


# Plot to examine the relationship between temperature and ph change
# and to assess visually whether we think a random intercept or slope might make sense
# so what do we see here? 
# we have temperature on the x-axis, and soil ph change on the y axis, and we have 50 sites, so 50 different colours of points
# because we have a lot of sites, its hard to tell if ph changes at different rates for different sites
# if you pick one colour and keep track of it across the x axis, it seems some of them might have slightly 
# steeper slopes than others, but nothing huge, nothing obvious
# what does seem obvious though is that sites definitely have different baselines
# if we take this dark blue site on the bottom, and compare it to this yellow site at the top, if you fit
# a single line witha single intercept to try and explain both of these sites, you'd probably get a line somewhere
# in the middle that doesnt actually explain either of them

soilPH$site <- factor(soilPH$site)  # ensure discrete

ggplot(soilPH, aes(temperature, pH_change, colour = site)) +
  geom_point() +
  scale_colour_manual(values = grDevices::rainbow(50)) +
  theme_minimal()

# so that's something to keep in mind
# that being said, fitting a single line is exactly what we're going to start out with

# 2. Fit models -------------------------------------------

# 2.1 Fixed-effect model 
# This is a simple linear regression of soil pH change against temperature, 
# ignoring the fact that observations come from multiple sites.
m_fixed <- lm(pH_change ~ temperature, data = soilPH)
summary(m_fixed)

#
# Residuals:
# What it tells us: A 5-number summary of model errors (observed - fitted). 
# Median ≈ 0 indicates residuals are centered well.
# The middle 50% of residuals lie roughly between -1.11 and +1.03 pH_change units, 
#  Extremes (≈ -5.38 and +6.08) are about 3.1–3.5 times the residual SD (see RSE below),
# hinting at a few moderate outliers.
# Good/Bad? Mixed—centered residuals are good; the tails suggest some outliers or heterogeneity that
# may merit a residual plot check.
#
# Coefficients:
#   (Intercept) = 0.13816 (SE = 0.24982), t = 0.553, p = 0.58
# What it tells us: Expected pH_change when temperature = 0.
#   At 0°C, the model predicts ~+0.14 change in pH; 
#   If 0°C is outside the observed temperatures, this intercept is mostly a mathematical anchor 
# and not scientifically meaningful.
#
#   temperature = 0.23765 (SE = 0.01633), t = 14.553, p < 2e-16 ***
#   The slope—expected change in pH_change per 1° increase in temperature.
#    For each +1°C, pH_change increases by ~0.238 units. 
# Rough 95% CI ≈ 0.238 ± 1.96*0.0163 ≈ (0.206, 0.270),
# indicating a precise, positive association. 
# For +10°C, that’s ~+2.38 units on the pH_change scale.If pH_change is in pH units, this is a large effect
#
#
# Residual standard error (RSE) = 1.735 on 1248 df:
#   Typical size of residuals (observed-predicted values) in outcome units.
#   On average, predictions miss by ~1.74 units of pH_change. 
#  Whether that is “large” depends on the scale/variability of pH_change in this study.
#
# Multiple R-squared = 0.1451; Adjusted R-squared = 0.1444:
#  Proportion of variance explained by temperature alone (~14.5%).
#  Temperature explains a meaningful but modest share of variability. 
# Many other factors likely influence pH_change.
# Adjusted R^2 ≈ R^2 because there’s only one  predictor and a large N—little penalty for model complexity.
# Good/Bad? Mixed—good evidence of an effect (useful for inference), but limited predictive power with this single predictor (weak for standalone prediction).
#
# F-statistic = 211.8 on 1 and 1248 DF, p-value < 2.2e-16:
#   Overall test that the model with temperature fits better than an intercept-only model.
#   Strongly rejects the null; temperature improves fit substantially over no predictor.
#   Good/Bad? Good—confirms the model adds explanatory value.



# 2.2 Random-intercept model
m_rand_int <- lmer(pH_change ~ temperature + (1 | site), data = soilPH)
summary(m_rand_int)

# so now that we've added a random intercept for site to account for clustering, how are things doing?

# REML criterion at convergence: 3046.1
#   Primarily useful for comparing REML-fitted models with the same fixed effects structure but different random effects.
#    Neutral—on its own, this number has no “good” or “bad” value; only meaningful in model comparisons. (smaller is better)
#
# Scaled residuals (Min/1Q/Median/3Q/Max = -3.0353, -0.6532, -0.0388, 0.6219, 3.6376):
#  Standardized residuals (residual / residual SD). 
# Median ≈ 0 indicates centering; spread is < |4|, suggesting no extreme outliers.
#   50% of scaled residuals are between roughly -0.65 and +0.62 (good symmetry, low skew). 
# Largest deviations (~±3) are within a reasonable range for normality.
#   Good/Bad? Good—no alarming skew or outliers; residuals appear well-behaved.
#
# Fixed intercept = 0.138 pH_change:
#  pH_change when temperature = 0°C for an “average” site (if you werent taking into account random intercepts).
# At 0°C, the model predicts +0.138 units change in pH for an average site. This is small and statistically non-significant, so it’s mostly a mathematical anchor unless 0°C is in range.
#
# Random intercept variance = 2.5049, SD = 1.5827 pH_change units:
# so we talk about our variances here, but its generally the standard deviation you'll be reporting
# remember the variance is just the standard deviation squared.
# we look at the standard deviation because its in the same units as our response, ph change;
# the random intercept measures the variation between sites in the baseline ph change
# before we take temperature into account, so at a given temperature, sites average ph change differs by roughly 1.58 units.
#
# Residual variance = 0.5507, SD = 0.7421 pH_change units:
#   Measures within-site variation remaining after accounting for temperature and site intercepts.
#   For two observations from the same site at the same temperature, typical differences are ±0.74 units of pH_change.
#   This is much smaller than the between-site SD (1.58), so most variation is explained by site differences, not random scatter.
#
# where is most of our variation coming from?
# compare the residual sd and the random intercept standard deviation. the standard deviation for the random
# intercept is roughly twice the size of the standard deviation for the residual
# This means most of the unexplained variability in pH_change after accounting for temperature is due to systematic differences between sites, not random scatter within sites.
# That’s why site identity is considered a major source of variation — it’s dominating the variance budget.

# Fixed slope (temperature) = 0.23765 (SE = 0.00699), t ≈ 34.0:
#   For each +1°C, predicted pH_change increases by ~0.238 units, same slope across all sites.
#   This effect is extremely precise and highly significant—very strong evidence for a positive temperature effect.
#
# Correlation of Fixed Effects (Intercept vs. temperature) = -0.422:
#   This is the correlation between the fixed-effect coefficient estimates, not between random effects.
#   It comes from the covariance matrix of the fixed effects and reflects how changes in one estimate are associated with changes in the other during model fitting.
#   A moderate negative value (-0.422) means that if the estimated intercept were slightly higher,
# the estimated slope would tend to be slightly lower (and vice versa) to produce a similar overall fit.
#   This usually happens when predictors are uncentered: here, the intercept is defined at 
# temperature = 0, which may be far from the average temperature, creating a trade-off between intercept and 
# slope in the estimation process.
#   Typically, this value can be ignored unless it is close to ±1, in which case it may indicate
# multicollinearity or estimation instability.
#   Knowing this helps with model diagnostics: high correlation might suggest centering or 
# rescaling predictors to improve interpretability and reduce estimation dependence.


# 2.3 Random-slope model (temperature effect varies by site)
m_rand_slope <- lmer(pH_change ~ temperature + (temperature | site), data = soilPH)
# Warning: "Model failed to converge with max|grad| = 0.145577 (tol = 0.002)"
# In plain terms, the fit didn’t settle at a clean optimum—some parameters 
# (often the random-effects variances/correlations) are poorly identified.
#
# Why this happens (especially with random-slope models):
#   Random slopes add extra variance and correlation parameters (intercept variance, slope variance, and intercept–slope correlation per grouping factor).
#   If there isn’t enough information within each site (e.g., too few observations or too little within-site variation in temperature), the slope variance and/or the intercept–slope correlation become hard to estimate.
#   Uncentered predictors aggravate this: when temperature isn’t centered, random intercepts and random slopes become highly correlated, making the likelihood surface flat/ridged.
#   
# Practical fixes:
#   Center temperature (globally or within site): soilPH$temperature_c <- scale(soilPH$temperature, center = TRUE, scale = FALSE)
#   Try a simpler random-effects structure:
#     Remove the intercept–slope correlation: (1 | site) + (0 + temperature_c | site)
#     If still unstable, try only a random intercept: (1 | site)
#   Use a more robust optimizer / more iterations:
#   The warning reflects difficulty estimating the added random-slope parameters, typically due to limited within-site information and/or uncentered predictors causing strong intercept–slope dependence.
#   Centering, simplifying the random-effects structure, and tuning the optimizer are the first steps to resolve it.


m_rand_slope <- lmer(
  pH_change ~ temperature + (temperature | site),
  data = soilPH,
  control = lmerControl(optimizer = "bobyqa"))


summary(m_rand_slope)

# REML criterion at convergence = 3022.7
# usable to compare models that have the SAME fixed-effects structure.
# Compared to the random-intercept model (REML ~3046.1 with same fixed effects), this model fits better (lower 3022.7).
# Good—supports including random slopes for temperature across sites.


#
# Fixed effects:
# Intercept = 0.1382 (SE = 0.1978), t = 0.698
# Predicted pH_change at temperature = 0°C for an average site (site RE = 0).
# Neutral—lack of significance is not a concern. 
# Center temperature (e.g., at its mean) if you want the intercept to be the
# predicted pH_change at an average temperature, which is often more interpretable.
# scale(temperature, center=T)
#
# Temperature (fixed slope) = 0.2377 (SE = 0.00923), t = 25.74
#   Meaning: Average effect of a 1°C increase on pH_change across sites.
#   Here: Strong, precise, positive association (~+0.238 pH units per °C). 
#   Random slopes allow sites to deviate around this mean.
#
# Random effects (site-level):
# Intercept variance = 1.4038 (SD = 1.1848 pH units)
# Between-site differences in baseline pH_change (at temperature = 0°C).
# Sites’ baseline pH_change typically differ by ~±1.18 units (1 SD) at the reference temp.
# This is smaller than in the random-intercept-only model (SD ~1.58), indicating part of what looked like baseline differences
#               was actually slope heterogeneity now captured by random slopes.
#
#   Temperature slope variance = 0.0019 (SD = 0.0436 pH units per °C)
#   Meaning: Sites differ in how strongly pH_change responds to temperature.
#   Typical site-specific slope deviation is ±0.044 from the overall slope.
#   Slopes vary across sites but the variation is modest—most sites still have, fairly similar temperature effect.
#
#  Intercept–slope correlation (random effects) = +0.44
#  Across sites, higher baselines tend to coincide with steeper temperature responses.
#   Sites that start higher in pH_change at 0°C also increase more rapidly with temperature, on average.
#
# Residual variance = 0.5332 (SD = 0.7302 pH units)
#   Within-site scatter after accounting for fixed effects and site-specific intercepts/slopes.
#   Typical prediction error within a site is ~0.73 units; slightly smaller than in the random-intercept model (~0.74),
#   consistent with improved fit when allowing slope variation.
#
#
# Correlation of Fixed Effects: temperature vs. intercept = -0.142
#   Meaning: Correlation between the fixed-effect estimates (not the random effects). Small and negative here.
#   Here: Mild dependence due to uncentered predictor; centering temperature typically reduces this further and makes the intercept clearer.
#   Good/Bad? Good/benign—nowhere near ±1; not a red flag.



# 3. Model comparisons ------------------------------------

# Compare using AIC
AIC(m_fixed, m_rand_int, m_rand_slope)

# Model comparison with AIC (same fixed effects across models)
#
# Why use AIC instead of the raw "REML criterion at convergence"?
#   AIC = 2k - 2 log(L) balances fit and complexity (k = number of estimated parameters shown in 'df').
#   The raw REML criterion reflects fit only; without the 2k penalty it will tend to favor more complex models.
#   With REML fits, AIC values are comparable when the fixed-effects structure is the same (as here: intercept + temperature in all models).
#   If you were comparing models with different fixed effects, refit with ML (REML = FALSE) and then compare AIC.
#
# Interpreting the 'df' column (number of parameters counted by AIC):
#  m_fixed (df=3): intercept, temperature slope, residual variance (sigma^2).
#  m_rand_int (df=4): above + site intercept variance.
#  m_rand_slope (df=6): above + site slope variance + intercept–slope correlation.
#
# Ranking by AIC (lower is better) and strength of evidence:
# Best: m_rand_slope (AIC = 3034.698).
# ΔAIC(m_rand_int) = 3054.082 − 3034.698 = 19.384  → strong evidence for random slopes over random-intercept-only.
# ΔAIC(m_fixed)    = 4928.547 − 3034.698 = 1893.849 → decisive evidence against the fixed-effects-only model.
# Rule of thumb: ΔAIC > 10 = essentially no support for the larger-Δ model; thus m_rand_slope is overwhelmingly preferred.
#



# 5. Inspect random effects -------------------------------

# Caterpillar plot for random intercepts
dotplot(ranef(m_rand_int, condVar = TRUE))

# Caterpillar plot for random intercepts and slopes
dotplot(ranef(m_rand_slope, condVar = TRUE))

# What the code does:
# dotplot function creates a caterpillar plot of random effects, sorted by value, with horizontal error bars
# we use ranef to extract the random effects from our random slope model
# and condVar=true asks lme4 to also  give us the error bars
#
# How to read the caterpillar plot (two panels: (Intercept) and temperature):
# Panel: (Intercept)
# on the left we have the random intercept - each dot is a sites random intercept deviation from the fixed intercept
# the horizontal bar gives the 95% confidence interval for that deviation
# Bars differ in length because some sites have more/less information (replicates,
# spread of temperature, etc.). Less information → larger SE → longer bar → stronger
# shrinkage toward 0.
# Interpretation of this plot: There is wide between-site spread in baselines
# (many dots far from 0), with several sites’ bands not overlapping 0 (clear baseline
# differences) and others overlapping 0 (baseline indistinguishable from average).
#
# Panel: temperature
# Each dot = a site’s random-slope deviation from the overall fixed slope.
# Interpretation of this plot: Dots cluster very near 0 with short, mostly overlapping
# bars, indicating small between-site slope variability—consistent with the model’s
# random-slope SD (~0.044 pH units per °C). Most sites’ deviations include 0, meaning
# their temperature response is close to the overall average slope; a few are slightly
# higher/lower but the variation is modest.
#
# Important nuances:
# These intervals are for the RANDOM EFFECTS (deviations), not the fixed effects. A bar
# crossing 0 does NOT “invalidate” the temperature effect; it just says that site’s
# deviation from the average effect may be ~0 given the data.


# 6. Visualising varying intercepts and slopes ------------
# We want to make a plot that shows the different intercepts and slopes for all of our sites, how do we do that?
# First, we need to ensure that our grouping variable site is categorical, if we use a numeric grouping variable it'll throw up
# an error when we come to make our predictions

soilPH$site<-factor(soilPH$site)

# next we're going to build a prediction grid across temperature and sites
# we want a smooth prediction curve for each site across the full observed temperature range
# so we create 100 evenly spaced temperatures for each site level so we can draw continuous lines
# you could use the data we fitted the model with, but if your raw temperatures are unevenly sampled, you
# wont get nice smooth lines
# so we're creating a new dataframe with temperature and site combinations
# and what expand.grid does is it creates a combination for every level of temperature with every level of site
nd <- expand.grid(
  temperature = seq(min(soilPH$temperature), max(soilPH$temperature), length.out = 100),
  site = levels(soilPH$site))
head(nd)

# Then we're going to make predictions on this new data from our random slopes model.
# the re.form function tells the model what we want to see. 
# if we set re.form = NA, this would give only the fixed effects line
# we're telling it here that we want the random intercepts and slopes for each site, 
# so each site gets its own baseline and random tilt
nd$fit <- predict(m_rand_slope, newdata = nd, re.form = ~(temperature | site))

#
# 4) Plot one line per site:
# we're using ggplot to create a nice plot, the data we're using is our new dataset
# then in our aesthetics we specify that on the x axis we have temperature, and on the 
# y axis we have our fitted values, or the predicted ph change
# and we want to colour each line according to its site
# we use the geom-line option to specify we want a line plot
library(ggplot2)
ggplot(nd, aes(temperature, fit, colour = site)) +
  geom_line() +theme_bw()

#
# How to interpret the resulting figure (given your fitted model):
# Strong common trend: Most lines rise with temperature (positive average slope).
# Baseline differences: Vertical separation of lines at any fixed temperature reflects
# site-specific intercepts (baseline pH_change differences).
# Slope differences: Differences in steepness reflect site-specific responsiveness; in
# your model these are modest (small random-slope SD), so lines will look similar, just
# not perfectly parallel.










###########################################
# Frog Presence
###########################################

## Packages
library(tidyverse)
library(lme4)          # glmer()
library(broom)         # tidy() for glm
library(broom.mixed)   # tidy() for glmer
library(ggeffects)     # effect curves

## ------------------------------------------------------------------
url<-"https://raw.githubusercontent.com/niamhmimnagh/FMME01---Introduction-to-Generalised-Linear-Mixed-Models/refs/heads/main/data/frog.txt"
frog<-read.table(url, header=TRUE)
head(frog)

# we're going to look at our regions first - so we can see that regions appear multiple times each
# each are sampled multiple times
# but they don't all have the same amount of data - some have more samples than others
# none of them have very small number of samples, but its worth noting that regions with smaller numbers of samples
# might experience more shrinking than sites with larger numbers of samples
# their estimates might be pulled more toward the mean, but thats unlikely to be the case here, as each region does have at least 25 observations
table(frog$region)

# its also worth checking if the sites are sampled multiple times as well. 
# here we have sites within regions, so if sites were also sampled multiple times, we would be looking at something
# called nested random effects, where one effect is contained within another effect
# that's not the case here, and we didnt have time to cover that today, but its worth keeping in mind for
# your data that you can have random effects within random effects
table(frog$site)

range(frog$depth) # depth is measured in metres, so it goes from about 10cm to around 2 and a half metres.
# that all looks fine, no nonsensical things like negative depths

table(frog$presence) # so we have 722 sites with no frogs and 542 sites with frogs. grand.



## 1) Plain logistic regression (no random effects)
# we're going to fit a logistic regression with no random effects - specifying the family is binomial and the link is logit
# technically specifying the logit link is not necessary as it is the default for the binomial family, but its a good habit to be in
# so that you always know exactly what you're fitting
m_glm <- glm(presence ~ depth, data = frog, family = binomial(link='logit'))
summary(m_glm)

#
# Coefficients:
# (Intercept) = -1.1800 (SE = 0.1270), z = -9.290, p < 2e-16
# Notes: depth=0 isn’t observed (min ≈ 0.10 m); the intercept is mainly a reference point. 
# Center depth (e.g., subtract mean) if you want the intercept to represent probability at an average depth.
log_odds<--1.18 # log odds (logit) of frog presence when depth is 0m

odds<-exp(log_odds)
odds # odds of frog presence when depth is 0m

probability<-odds/(1+odds) # probability of frog presence when depth is 0m - 23.5%
probability

#   depth = +0.6838 (SE = 0.0849), z = 8.055, p = 7.94e-16
log_odds<-0.6838 # each 1m increase in depth adds 0.684 to the log-odds of presence
odds_ratio<-exp(log_odds)
odds_ratio # 1.98 - this is the multiplicative effect
# each 1m increase in depth, almost doubles the odds of frog presence

probability<-odds_ratio/1+odds_ratio # not correct. why?
# exp(beta0): odds term
# exp(beta1): odds ratio term - it tells us how the odds change for a 1m increase in depth, not what the absolute odds are at a certain value of x

# To get a probability you need the ABSOLUTE ODDS at a specific depth, which depend
# on BOTH beta0 and beta1 (and the depth value).
#
# How to compute probability correctly:
# Compute the linear predictor η(depth) = beta0 + beta1 * depth, then apply the logistic
# transform p(x) = exp(beta0+beta1x) / (1 + exp(beta0+beta1x)) 
#
# Using your fitted coefficients:
beta0 <- -1.1800
beta1 <-  0.6838

probability_0.1 <- plogis(beta0 + beta1 * 0.1)  # ≈ 0.2476 (24.8%)
probability_1   <- plogis(beta0 + beta1 * 1.0)  # ≈ 0.3784 (37.8%)
probability_2   <- plogis(beta0 + beta1 * 2.0)  # ≈ 0.5468 (54.7%)


# Null deviance = 1726.6 on 1263 df
# Residual deviance = 1658.4 on 1262 df
# Null deviance: fit with intercept only. Residual deviance: fit with depth included.
# Drop in deviance: 1726.6 − 1658.4 = 68.2 on 1 df → likelihood-ratio p-value is tiny (< 1e-15):
# depth materially improves fit vs. intercept-only.





## 2) GLMM: random intercept for region
m_ri  <- glmer(presence ~ depth + (1 | region), data = frog, family = binomial)
summary(m_ri)


# Fit / information criteria:
#   AIC = 1509.4, BIC = 1524.8, logLik = -751.7, -2*logLik = 1503.4
#   AIC/BIC balance fit and complexity (lower = better). Compared with the plain GLM (AIC ≈ 1662.4),
#   adding a region random intercept substantially improves fit, consistent with regional clustering.

# Fixed effects:
#   (Intercept) = -1.4066 (SE = 0.2158), z = -6.52, p = 7.13e-11
log_odds<--1.406 # log odds of frog presence at 0m depth
odds<-exp(log_odds)
odds # odds of frog presence at 0m depth
probability<-odds/(1+odds)
probability # 19.7% probability of frog presence at depth=0m
#  Note 0 m is extrapolated (min depth ≈ 0.10 m);
#
#   depth = +0.8182 (SE = 0.0961), z = 8.51, p < 2e-16 
log_odds<-0.8182 # each 1m increase in depth adds 0.8182 to the log-odds of presence
odds_ratio<-exp(log_odds)
odds_ratio # 2.27: each 1m increase in depth increases the odds of presence by 2.27
# cant find increase in probability in the same way -  we need to look at probability at certain values
beta0<--1.406
beta1<-0.818
prob_0.1<-plogis(beta0+beta1*0.1)
prob_1<-plogis(beta0+beta1*1)
prob_1-prob_0.1 # increase in probability of 15%


# Random effects (region intercept):
#   Variance = 1.033 (SD = 1.016) on the logit 
# we know from the fixed effect that the intercept is -1.406, but the intercept differs across regions by typically 1.016.
# and on the odds scale the baseline intercept tells us that the odds of frog presence at 0 depth was 0.245
# but due to random intercepts, regions can have baseline odds of:
exp(1.016) # 2.76 times the overall baseline - remember because on the odds scale we're working in multiplicative values
# The random-intercept SD of 1.016 indicates SUBSTANTIAL between-region baseline differences in frog presence.
# Regions one SD above average have ~2.76× the odds of presence (at any given depth) compared to an average region;
# and regions with odds below the average can have their odds divided by 2.76 
#
# Correlation of Fixed Effects: depth vs. intercept = -0.593
# Correlation between the FIXED-effect estimates (not random effects). Moderately negative due to
# using uncentered depth (intercept defined at depth = 0). Centering depth typically reduces this
# correlation and makes the intercept more interpretable.






## 3) GLMM: random intercepts & random slopes for depth by region
m_rs  <- glmer(presence ~ depth + (depth | region), data = frog, family = binomial)
summary(m_rs)


# Fit / information criteria:
#   AIC = 1485.0, BIC = 1510.7, logLik = -737.5, -2*logLik = 1475.0, df.resid = 1259
#   Lower AIC than the random-intercept GLMM (≈1509.4) → ΔAIC ≈ 24 favors allowing region-specific slopes.
#   Takeaway: Regions differ not only in baseline presence but also in how presence changes with depth.
#

# Fixed effects (population-average line):
#   • (Intercept) = −1.4509 (SE 0.2532), z = −5.73, p = 1.0e−08
log_odds<--1.45 # log odds (logit) frog presence at 0m depth for an average region
odds<-exp(log_odds)
odds # odds of frog presence at 0m depth
probability<-odds/(1+odds)
probability # probability of frog presence at 0m
plogis(-1.4509) # when we're using the plogis function for the slope, we need to include beta0 and beta1, but because
# here we're looking at when depth=0, we just use beta0

# depth = +0.8404 (SE 0.1678), z = 5.01, p = 5.45e−07
log_odds<-0.8404 # the icnrease in log-odds of frog presence per 1m increase in depth
odds_ratio<-exp(log_odds) 
odds_ratio # 2.32 - for each 1m increase in depth, the odds increase by a multiplicative factor of 2.3, or the odds are multiplied by 2.3

beta0<--1.45
beta1<-0.84
prob_1<-plogis 
prob_1 # REMEMBER: this is not the increase in probablity per metre, this is just the probability at 1m depth


# Random effects (by region):
# (Intercept) variance = 1.6034  (SD = 1.2663, logit units)
# the fixed effect intercept is -1.45 (on the log-odds scale), but this can vary across regions by 1.27

# depth slope variance = 0.6874 (SD = 0.8291, logit units per metre)
# the strength of the depth effect varies notably among regions
# the typical deviation from the fixed effect slope is plus or minus 0.829 logit units per m
# the change in log odds per 1m of depth can deviate by plus or minus 0.829.
# the change in the log odds of frog presence per unit depth is 0.84, so a deviation of 0.829 is substantial
# some regions see weaker or even negative depth effects, while others will see much steeper positive effects

#         
# Intercept–slope correlation (random effects) = −0.61
# Meaning: Regions with higher baselines tend to have shallower depth responses (and vice versa).

# Correlation of Fixed Effects: depth vs. intercept = −0.720
# This is the correlation between the fixed-effect estimates (not random effects).
# It is moderately negative because depth is uncentered (intercept defined at depth = 0).
# Typically ignorable unless near ±1; centering depth reduces it and clarifies the intercept.
# # how close to 1 is too close? typically less than 0.5 is ignorable, between 0.5 and 0.8 you would consider
# recentering your variable to improve interpretability
# 

## ---- Model comparison
AIC(m_glm, m_ri, m_rs)


# 6. Visualising varying intercepts and slopes ------------


frog$region<-factor(frog$region)

nd <- expand.grid(
  depth = seq(min(frog$depth), max(frog$depth), length.out = 100),
  region = levels(frog$region))
head(nd)

nd$fit <- predict(m_rs, newdata = nd, re.form = ~(depth | region))

library(ggplot2)
ggplot(nd, aes(depth, fit, colour = region)) +
  geom_line() +theme_bw()






