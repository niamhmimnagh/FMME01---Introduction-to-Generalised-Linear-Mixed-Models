# FMME01---Introduction to Generalised Linear Mixed Models
This repository contains all materials for the Introduction to Generalised Linear Mixed Models (GLMMs) course. It includes datasets, example scripts, and lecture slides used throughout the sessions.

📂 Repository Structure
```text
FMME01---Introduction-to-Generalised-Linear-Mixed-Models/
│
├── data/                  # Datasets used in the course
│   ├── frog.txt/.csv       # Presence/absence of frogs with environmental predictors (binary/logistic)
│   ├── soil_ph.txt/.csv    # Soil pH measurements with grouping variables (Gaussian/identity)
│   └── ... other datasets ...
│
├── scripts/               # R code for demos and data creation
│   ├── simulate_data.R     # Reproducible code to generate datasets
│   ├── coding_demo.R       # Coding demonstrations shown in lectures
│   └── ... additional scripts ...
│
├── slides.pdf              # PDF of the lecture slides
│
└── README.md               # This file
```
📖 Course Overview

This course introduces the GLMM framework, extending GLMs to account for hierarchical or grouped data using random effects. We cover when and why to use mixed models, fitting in R, and interpreting both fixed and random effects.

Topics covered:

    Recap of GLMs

    The need for mixed models (pseudo-replication, clustering, repeated measures)

    Components of a GLMM: fixed effects, random effects, link functions, distributions

    Common GLMMs:

        Logistic mixed models (binary data with grouping)

        Gaussian mixed models (continuous outcomes with grouping)

    Fitting GLMMs in R (lme4::glmer, lme4::lmer)

    Interpretation of fixed and random effects

📊 Data

The data/ folder contains simulated datasets used for exercises and demonstrations:

    frog.txt/.csv — Presence/absence of frogs (0/1) with predictors such as habitat type, water quality, and a site-level random effect (logistic link).

    soil_ph.txt/.csv — Soil pH measurements with predictors and a random effect for sampling location (Gaussian/identity link).

💻 Scripts

The scripts/ folder contains reproducible R code for:

    simulate_data.R — Code used to generate the datasets stored in data/.

    coding_demo.R — Step-by-step coding demonstrations (data simulation, model fitting, predictions, plots).

    Additional helper scripts for specific sessions.

📑 Slides

    slides.pdf contains the lecture slides used in the course (theory, worked examples, and interpretation guidance).

🔧 Getting Started

Clone this repository:

git clone https://github.com/niamhmimnagh/FMME01---Introduction-to-Generalised-Linear-Mixed-Models.git

Open in R/RStudio (recommended).

Install required packages (used across demos; install as needed):

    install.packages(c(
      "lme4",         # mixed-effects models (lmer, glmer)
      "ggplot2",      # plotting
      "dplyr",        # data wrangling
      "lattice",      # caterpillar (dotplot) for random effects
      "sjPlot",       # pretty model plots
      "tidyverse",    # pipes + general utilities (also includes ggplot2/dplyr)
      "broom",        # tidy() for glm
      "broom.mixed",  # tidy() for glmer
      "ggeffects"     # effect curves
    ))

Run the demos:

    Open files in scripts/ and run top-to-bottom to reproduce examples.

    Load datasets from data/:

    # Tab-delimited example
    frog <- read.delim("data/frog.txt", header = TRUE)


    Or read directly from GitHub raw links if preferred.

🤝 Contributing

Issues and pull requests are welcome (typos, improvements to code comments, additional examples).
