# Soil PH
set.seed(1)

n_sites <- 50
n_obs_per_site <- 25
N <- n_sites * n_obs_per_site

# Site ID
site <- factor(rep(seq_len(n_sites), each = n_obs_per_site))

# Raw temperature in °C:
# Balanced design: each site experiences a range of temperatures from 10°C to 20°C
temp_grid <- seq(10, 20, length.out = n_obs_per_site)
temperature <- rep(temp_grid, times = n_sites)

# True parameters 
beta0      <- 0.0    # mean pH change at 0°C (just a mathematical baseline)
beta1      <- 0.25   # slope: change in pH_change per 1°C increase in temperature
sd_int     <- 0.80   # SD of random intercepts
sd_slope   <- 0.05   # SD of random slopes
rho_is     <- 0.30   # correlation between intercepts & slopes
sigma_eps  <- 0.70   # residual SD

# Covariance matrix for random effects
Sigma <- matrix(c(sd_int^2,           rho_is*sd_int*sd_slope,
                  rho_is*sd_int*sd_slope, sd_slope^2),
                nrow = 2, byrow = TRUE)

# Simulate site-level random intercepts and slopes
re_mat <- MASS::mvrnorm(n_sites, mu = c(0, 0), Sigma = Sigma)
b0_site <- re_mat[, 1]  # random intercepts
b1_site <- re_mat[, 2]  # random slopes

# Map random effects to observations
rand_intercept <- b0_site[site]
rand_slope     <- b1_site[site]

# Simulate response: soil pH change (real-valued, can be negative or positive)
pH_change <- beta0 + rand_intercept + (beta1 + rand_slope) * temperature +
  rnorm(N, mean = 0, sd = sigma_eps)

soilPH <- data.frame(site, temperature, pH_change)

# Check range 
range(soilPH$pH_change)
# Frog Presence/Absence
set.seed(1)
J <- 40                                   # regions
n_j <- sample(25:40, J, replace = TRUE)   # sites per region
sigma_intercept <- 0.9                     # between-region SD (baseline occupancy)
sigma_slope     <- 0.6                     # between-region SD (depth effect)
rho             <- 0.35                    # corr(intercept, slope)
Sigma <- matrix(c(sigma_intercept^2, rho*sigma_intercept*sigma_slope,
                  rho*sigma_intercept*sigma_slope, sigma_slope^2), 2, 2)

beta_0 <- qlogis(0.45)  # population baseline (on logit scale, ~45% at mean depth)
beta_1 <- 1.0           # population depth effect (per metre, before centering)

sim <- map_dfr(1:J, function(j){
  a <- MASS::mvrnorm(1, mu = c(0,0), Sigma = Sigma)   # (a0j, a1j)
  tibble(
    region = paste0("R", j),
    site   = paste0("R", j, "_", seq_len(n_j[j])),
    depth  = runif(n_j[j], 0.1, 2.5)
  ) |>
    mutate(depth_c = depth - mean(depth),                    # centre across all sites
           eta     = (beta_0 + a[1]) + (beta_1 + a[2]) * depth_c,
           p       = plogis(eta),
           presence = rbinom(n(), 1, p))
})

dat <- sim
frog <- dat |>
  mutate(region = factor(region))
head(frog)
write.table(frog, file = "frog.txt", sep = "\t", row.names = FALSE, quote = FALSE)
