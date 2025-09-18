
# Random Effects Visualisation --------------------------------------------

library(ggplot2)
library(dplyr)

set.seed(123)

# Simulate grouped data
n_sites <- 6
n_per_site <- 20

# Site-level intercepts
site_intercepts <- rnorm(n_sites, mean = 0, sd = 2)
site_slopes <- rnorm(n_sites, mean = 0.1, sd = 0.15) # weaker within-site slopes

data <- expand.grid(site = factor(1:n_sites),
                    obs = 1:n_per_site) %>%
  mutate(
    x = runif(n_sites * n_per_site, 0, 10),
    intercept = site_intercepts[as.numeric(site)],
    slope = site_slopes[as.numeric(site)],
    y = intercept + slope * x + rnorm(n_sites * n_per_site, sd = 1)
  )

# Add a pooled slope effect to make the combined data strongly positive
data <- data %>%
  mutate(y = y + 0.4 * x) # strong pooled slope

# Plot
ggplot(data, aes(x = x, y = y, colour = site)) +
  geom_point(size = 2, alpha = 0.7) +
  # Within-site trend lines
  geom_smooth(method = "lm", se = FALSE, size = 0.8) +
  # Pooled trend line
  geom_smooth(aes(group = 1), colour = "black", size = 1.2, method = "lm", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Predictor X",
    y = "Response Y"
  )



# Class Scores ------------------------------------------------------------

library(ggplot2)
library(dplyr)

set.seed(42)

# Simulate data
n_classes <- 8
students_per_class <- 12

# True fixed effect (overall mean score)
fixed_mean <- 70

# Random intercepts for each class (natural variation)
class_effects <- rnorm(n_classes, mean = 0, sd = 5)

# Generate student scores
data <- expand.grid(class = factor(1:n_classes),
                    student = 1:students_per_class) %>%
  mutate(
    class_intercept = class_effects[as.numeric(class)],
    score = fixed_mean + class_intercept + rnorm(n(), mean = 0, sd = 5)
  )

# Overall mean line + per-class mean lines
ggplot(data, aes(x = class, y = score)) +
  geom_jitter(width = 0.15, alpha = 0.6, colour = "grey30") +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "blue") +
  geom_hline(yintercept = fixed_mean, colour = "red", size = 1.2, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Class",
    y = "Score"
  )


# Random Intercept Model --------------------------------------------------

library(ggplot2)
library(dplyr)

set.seed(1)

# ---- Parameters ----
beta0     <- 2          # overall intercept
beta1     <- 0.6        # fixed slope (common to all groups)
sigma_mu  <- 1.2        # SD of random intercepts
sigma_eps <- 1.0        # residual SD

n_groups    <- 6
n_per_group <- 25

# ---- Group-level random effects ----
groups <- tibble(
  group = factor(seq_len(n_groups)),
  mu_j  = rnorm(n_groups, 0, sigma_mu)
)

# Expanded data with observations per group
dat <- groups %>%
  tidyr::uncount(weights = n_per_group) %>%
  mutate(
    x   = runif(n(), 0, 10),
    eps = rnorm(n(), 0, sigma_eps),
    y   = beta0 + mu_j + beta1 * x + eps
  )

# Lines & labels for plotting
group_lines <- groups %>%
  mutate(
    intercept   = beta0 + mu_j,          # β0 + μj
    slope       = beta1,
    # Nice labels to show on the plot
    lbl = sprintf("group %s:\nμ[j]=%+.2f,\nβ0+μ[j]=%+.2f",
                  group, mu_j, intercept)
  )

# Where to place labels (near x=0 so they align with intercepts)
label_pos <- group_lines %>%
  transmute(
    group,
    x = 0.3,
    y = intercept + slope * 0.3, # point on each line near x~0
    lbl
  )

# Population line label
pop_lbl <- sprintf("Population line:\nβ0 = %.2f, β1 = %.2f", beta0, beta1)

# ---- Plot ----
ggplot(dat, aes(x, y, colour = group)) +
  # Raw data
  geom_point(alpha = 0.6, size = 2) +
  
  # Group-specific parallel lines: (β0 + μj) + β1 * x
  geom_abline(data = group_lines,
              aes(intercept = intercept, slope = slope, colour = group),
              linewidth = 0.9, show.legend = FALSE) +
  
  # Intercept markers at x = 0 for each group line
  geom_point(data = group_lines,
             aes(x = 0, y = intercept, colour = group),
             size = 3, show.legend = FALSE) +
  
  # Labels for each group's μj and (β0 + μj)
  geom_label(data = label_pos,
             aes(x = x, y = y, label = lbl, colour = group),
             fill = "white", label.size = 0.25, label.padding = unit(0.15, "lines"),
             show.legend = FALSE) +
  
  # Population (fixed-effects) line and label
  geom_abline(intercept = beta0, slope = beta1,
              linewidth = 1.2, linetype = "dashed", colour = "black") +
  annotate("label",
           x = 8.5, y = beta0 + beta1 * 8.5,
           label = pop_lbl,
           hjust = 1, vjust = -0.3,
           colour = "black", fill = "white", label.size = 0.25) +
  
  labs(
    subtitle = expression(
      "Model:  " * Y[ij] == beta[0] + mu[j] + beta[1] * x[ij] + epsilon[ij] ~ ",  with  " ~ mu[j] %~% N(0, sigma[mu]^2)
    ),
    x = "x",
    y = "y"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.subtitle = element_text(size = 11))

# Random Slopes Model -----------------------------------------------------

# ---- Random slopes visual: temperature vs abundance by site ----
set.seed(42)

library(dplyr)
library(ggplot2)

# Simulate sites ordered south -> north
n_sites   <- 8
n_per_site <- 25
sites <- tibble(
  site = factor(paste0("S", 1:n_sites)),
  lat_index = scale(1:n_sites, center = TRUE, scale = TRUE)  # proxy for south(-) to north(+)
)

# True (global) intercept and slope
beta0 <- 20                   # baseline abundance at temp = 0
beta1 <-  2.0                 # average temp effect

# Random effects:
#  - intercepts vary by site
#  - slopes vary by site, and we make northern sites have stronger positive slopes
sd_intercept <- 6
sd_slope     <- 1.0
north_boost  <- 1.2           # how much slope increases per SD of 'northness'

sites <- sites %>%
  mutate(
    b0_site = rnorm(n_sites, 0, sd_intercept),                       # random intercepts
    b1_site = rnorm(n_sites, 0, sd_slope) + north_boost * lat_index  # random slopes with north boost
  )

# Expand to observations and simulate temperatures + abundance
dat <- sites %>%
  tidyr::uncount(n_per_site) %>%
  group_by(site, lat_index, b0_site, b1_site) %>%
  mutate(
    temp = runif(n(), min = -5, max = 25),                           # temperature range
    eps  = rnorm(n(), 0, 4),                                         # residual noise
    abundance = (beta0 + b0_site) + (beta1 + b1_site) * temp + eps
  ) %>%
  ungroup()

# Overall (fixed-effect) line for reference
overall_fit <- lm(abundance ~ temp, data = dat)

# ---- Plot: points + per-site regression lines + overall line ----
ggplot(dat, aes(temp, abundance, color = site)) +
  geom_point(alpha = 0.5) +
  # per-site lines (random slopes)
  geom_smooth(method = "lm", se = FALSE) +
  # overall fixed-effect line in black dashed
  geom_abline(
    intercept = coef(overall_fit)[1],
    slope     = coef(overall_fit)[2],
    linewidth = 1,
    linetype  = 2,
    color     = "black"
  ) +
  labs(
    x = "Temperature (°C)", y = "Bird abundance (index)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")



# Slope-Intercept Correlation ---------------------------------------------

# ------------------------------------------------------------
# Visualizing intercept–slope correlation in random slopes models
#   - Left: positive correlation (lines fan upward)
#   - Right: negative correlation (lines form an X pattern)
# ------------------------------------------------------------
set.seed(123)

library(MASS)     # for mvrnorm
library(dplyr)
library(tidyr)
library(ggplot2)

# Global fixed effects (overall line)
beta0 <- 10      # overall intercept
beta1 <-  1.5    # overall slope

# Random-effect SDs
sd_intercept <- 2.5
sd_slope     <- 1.0

# Choose correlations for the two scenarios
rhos <- c("Positive ρ =  +0.7" = 0.7,
          "Negative ρ =  −0.7" = -0.7)

n_groups   <- 10      # number of groups
x_seq      <- seq(-2, 2, length.out = 50)

simulate_scenario <- function(rho, label) {
  # Covariance matrix for (b0, b1)
  D <- matrix(c(sd_intercept^2, rho*sd_intercept*sd_slope,
                rho*sd_intercept*sd_slope, sd_slope^2), nrow = 2)
  # Sample group-level random effects
  b_mat <- MASS::mvrnorm(n_groups, mu = c(0,0), Sigma = D)
  colnames(b_mat) <- c("b0", "b1")
  
  tibble(group = factor(paste0("G", seq_len(n_groups))),
         b0 = b_mat[, "b0"],
         b1 = b_mat[, "b1"]) |>
    crossing(x = x_seq) |>
    mutate(
      # Group-specific line: (beta0 + b0) + (beta1 + b1) * x
      y = (beta0 + b0) + (beta1 + b1) * x,
      scenario = label
    )
}

dat <- purrr::imap_dfr(rhos, simulate_scenario)

# Overall (fixed-effect) line for reference
overall <- tibble(x = x_seq,
                  y = beta0 + beta1 * x)

# Plot
ggplot(dat, aes(x, y, group = group, color = group)) +
  geom_line(alpha = 0.9, linewidth = 1) +
  geom_line(data = overall, aes(x, y),
            inherit.aes = FALSE, linetype = 2, linewidth = 1, color = "black") +
  facet_wrap(~ scenario) +
  labs(
    x = "Predictor (x)", y = "Response (y)", color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


# Caterpillar plot --------------------------------------------------------

library(lme4); library(lattice)

## m2 is your model, e.g. glmer(presence ~ depth_c + cover_c + (1 + depth_c | region),
##                              family = binomial, data = frogs)
dotplot(ranef(m2, condVar = TRUE), scales = list(x = list(relation = "free")))

