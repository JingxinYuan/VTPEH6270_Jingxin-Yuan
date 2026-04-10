# Part 1: Research question and simulation basis
# Research question:
# Does gender influence CKD mortality rate among adults aged 60+ years in China?

# Plausible relationship:
# CKD mortality rates among adults aged 60+ in China appear consistently higher in males than in females across all observed years. It suggests a stable gender effect where mortality is greater in males than in females, with both groups following a similar temporal trend over time.


# Part 2: Load data
# Packages
library(tidyverse)
library(knitr)

# Set working directory
setwd("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Data/Processed data")

# Load file
data_ckd = read.csv("IHME-GBD_CKD_Processed.csv")

# filter data
data_ckd_old <- data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name == "Deaths"
  ) %>%
  select(year, sex_name, val)

# Examine data structure
str(data_ckd_old)

# Create variable description table
var_table <- tibble(
  Variable_Name = c("year", "sex_name", "val"),
  Variable_Type = c("Discrete", "Categorical", "Continuous"),
  R_Class = c(class(data_ckd_old$year),
              class(data_ckd_old$sex_name),
              class(data_ckd_old$val)),
  Description = c(
    "Observation year",
    "Gender (Male or Female)",
    "CKD mortality rate per 100,000 population"
  )
)
var_table


# Part 3: Parameter setup
param_table <- data.frame(
  Parameter = c("Baseline mortality rate","Gender effect","Noise","Sample size"),
  Symbol = c("μ","δ","ε","n"),
  Description = c(
    "Average CKD mortality rate among females",
    "Difference in mortality rate between males and females",
    "Random variability in mortality rate",
    "Number of simulated observations"
  )
)

knitr::kable(param_table)


# Part 4: Simulation
# Simulation basis
# Since female mean mortality is about 43.3, so setting baseline as 43
simulate_ckd <- function(effect_size, noise, sample_size){
  gender <- rep(c("Female", "Male"), each = sample_size)
  baseline <- 43
  
  mortality <- c(
    rnorm(sample_size, mean = baseline - effect_size / 2, sd = noise),
    rnorm(sample_size, mean = baseline + effect_size / 2, sd = noise)
  )
  
  data.frame(
    gender = gender,
    mortality = mortality
  )
}


# Simulation output
summarize_sim <- function(sim_data){
  sim_data |>
    dplyr::group_by(gender) |>
    dplyr::summarise(
      mean_mortality = mean(mortality),
      sd_mortality = sd(mortality),
      n = dplyr::n(),
      .groups = "drop"
    )
}


# Simulation function
run_sim <- function(effect_size, noise, sample_size){
  sim_data <- simulate_ckd(effect_size, noise, sample_size)
  summary <- summarize_sim(sim_data)
  list(
    data = sim_data,
    summary = summary
  )
}

sim_example <- run_sim(effect_size = 5, noise = 2, sample_size = 100)
sim_example$summary


# Simulation Automation
effect_sizes <- seq(1, 10, length.out = 10)
sample_sizes <- seq(20, 200, length.out = 10)
noise_levels <- c(1, 3, 5)

results <- list()

for(e in effect_sizes){
  for(s in sample_sizes){
    for(n in noise_levels){
      results[[length(results)+1]] <- run_sim(e,n,s)
    }
  }
}


# Part 5: Visulization
# Reconstruct parameters in the same order
param_grid <- expand.grid(
  noise = noise_levels,
  sample_size = sample_sizes,
  effect_size = effect_sizes
)

# Extract estimated difference between males and females from each simulation
viz_df <- do.call(rbind, lapply(seq_along(results), function(i){
  x <- results[[i]]
  s <- x$summary
  female_mean <- s$mean_mortality[s$gender == "Female"]
  male_mean <- s$mean_mortality[s$gender == "Male"]
  
  data.frame(
    effect_size = param_grid$effect_size[i],
    noise = param_grid$noise[i],
    sample_size = param_grid$sample_size[i],
    estimated_difference = as.numeric(male_mean - female_mean)
  )
}))

summary_viz <- viz_df |>
  dplyr::group_by(noise, sample_size, effect_size) |>
  dplyr::summarise(
    mean_est = mean(estimated_difference),
    sd_est = sd(estimated_difference),
    .groups = "drop"
  )

# Plot
p5 <- ggplot(viz_df, aes(x = sample_size, y = estimated_difference, color = effect_size)) +
  geom_jitter(width = 5, height = 0, alpha = 0.4, size = 1.5) +
  facet_grid(~ noise, labeller = label_both) +
  labs(
    title = "Simulation of estimated gender differences in \nCKD mortality across effect sizes, sample sizes, and noise",
    x = "Sample size per group",
    y = "Estimated sample mean difference \n in mortality rate (Male - Female)",
    color = "True effect size"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1.5, "lines")
  )

ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Simulation results across effect sizes, sample sizes and noise.png", plot = p5)