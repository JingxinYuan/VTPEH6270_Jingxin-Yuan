# Packages
library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(knitr)

# 1 Material & Methods
# 1.1 Data and Processing
# 1.1.1 Load data
# Set working directory
setwd("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Data/Original data")

# Load file
data_ckd = read.csv("IHME-GBD_CKD_20260130.csv")

# Preview the data
head(data_ckd)

# 1.1.2 Data Cleaning (NA & Abnormal value)
# Check of NA
n_na <- sum(is.na(data_ckd))
cat("The amount of NA is",n_na,end='\n')

# Check available years
sort(unique(data_ckd$year))

# Check of Abnormal Value
# Define IQR measure to detect abnormal value
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x < lower_bound | x > upper_bound)
}

# Detected column
numeric_cols <- c("val", "upper", "lower")

# Detect
outliers_df <- data_ckd %>%
  mutate(across(all_of(numeric_cols), detect_outliers))

outlier_summary <- colSums(outliers_df[numeric_cols], na.rm = TRUE)
print(outlier_summary)

# Replace abnormal value with median value
median_vals <- apply(data_ckd[numeric_cols], 2, median, na.rm = TRUE)

data_ckd[numeric_cols] <- lapply(numeric_cols, function(col) {
  data_ckd[[col]] <- ifelse(outliers_df[[col]], median_vals[col], data_ckd[[col]])
})

# Output processed data
write.csv(data_ckd, "C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Data/Processed data/IHME-GBD_CKD_Processed.csv")

# 1.1.3 Variable Overview
# Mutate class of important data column
data_ckd = data_ckd %>%
  mutate(
    year = as.integer(year),
    val = as.numeric(val),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  )

# Create a variable table
data_variable = names(data_ckd)
data_class = sapply(data_ckd,class)
data_type = sapply(data_ckd, function(x){
  if (is.numeric(x)) {
    if (all(x %% 1 == 0, na.rm = TRUE)) "discrete"
    else "continuous"
  } else {
    "categorical"
  }
})

# Combine to one table
data_ckd_table = data.frame(
  variable = data_variable,
  class = data_class,
  type = data_type
)

# Output 10 head rows of variable table
data_ckd_table = head(data_ckd_table,10)
kable(data_ckd_table, caption = "Table 1. Variable overvie (first 10 variables)")

# 1.1.4 Data Subset
unique(data_ckd$measure_name)
data_ckd_old = data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name == "Deaths"
  ) %>%
  select(year,sex_name,val)

# 1.2 Statistical Analysis
# 1.2.1 Assumption Check
# Split data
male_data <- data_ckd_old %>% filter(sex_name == "Male") %>% pull(val)
female_data <- data_ckd_old %>% filter(sex_name == "Female") %>% pull(val)

# Shapiro test
shapiro.test(male_data)
shapiro.test(female_data)

# QQ plots
qqnorm(male_data, main = "QQ Plot for Male")
qqline(male_data)
qqnorm(female_data, main = "QQ Plot for Female")
qqline(female_data)

# 1.3 Simulation
# 1.3.1 Parameter setup
param_table <- data.frame(
  Parameter = c("Baseline mortality rate","Gender effect","Noise","Sample size"),
  Symbol = c("$\\mu$","$\\delta$","$\\epsilon$","n"),
  Description = c(
    "Average CKD mortality rate among females",
    "Difference in mortality rate between males and females",
    "Random variability in mortality rate",
    "Number of simulated observations"
  )
)

knitr::kable(param_table)

# 1.3.2 Simulation basis
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


# 1.3.3 Simulation output
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


# 1.3.4 Simulation function
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


# 1.3.5 Simulation Automation
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

# 2 Results
# 2.1 Exploratory Analysis
# 2.1.1 Distribution Checking
# Histogram on the mortality burden of CKD of 60+ years people in China
# Histogram
mean_d <- mean(data_ckd_old$val, na.rm = TRUE)
median_d <- median(data_ckd_old$val, na.rm = TRUE)

vlines <- data.frame(
  stat = c("Mean", "Median"),
  xint = c(mean_d, median_d)
)

p1 <- ggplot(data_ckd_old, aes(x = val)) +
  geom_histogram(bins = 25, fill = "grey") +
  geom_vline(
    data = vlines,
    aes(xintercept = xint, linetype = stat)
  ) +
  scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "dotted")) +
  theme_classic() +
  labs(
    title = "Distribution of CKD Mortality Rate of\n 60+ Years Old People in China (2013–2023)",
    x = "Mortality rate (per 100,000)",
    y = "Count",
    linetype = "Statistic",
    caption = "Figure 1. Histogram of CKD Mortality rate in China with mean and median lines."
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Distribution on the burden of CKD of 60+ years people in China.png", plot = p1)

# Show plot
p1

# 2.1.2 Grouped Summary Statistics
summary_table <- data_ckd_old %>%
  group_by(sex_name) %>%
  summarise(
    Mean = mean(val),
    SD = sd(val),
    Median = median(val),
    Min = min(val),
    Max = max(val)
  )
kable(summary_table, digits = 2)

# 2.1.3 Mortality difference
data_ckd_old_diff <- data_ckd_old %>%
  pivot_wider(names_from = sex_name, values_from = val) %>%
  mutate(
    mortality_difference = Male - Female
  ) %>%
  select(year, mortality_difference)

data_ckd_old <- data_ckd_old %>%
  left_join(data_ckd_old_diff, by = "year")
head(data_ckd_old)

# 2.1.4 Comparative visualization on mortality
p2 <- ggplot(data_ckd_old, aes(x = year, y = val, color = sex_name)) +
  geom_line(size = 1.2) +
  labs(
    title = "CKD Mortality Rate in China (Age 60+ Years)",
    x = "Year",
    y = "Mortality Rate per 100,000",
    color = "Gender"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Gender trend on mortality of CKD of 60+ years people in China.png", plot = p2)

# Show plot
p2

# 2.1.5 Visualize mortality difference between male and female
p3 <- ggplot(data_ckd_old, aes(x = year, y = mortality_difference)) +
  geom_col(fill = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Sex Difference in CKD Mortality (Male − Female)",
    x = "Year",
    y = "Mortality Rate Difference (per 100,000)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Mortality difference between gender of CKD of 60+ years people in China.png",plot = p3)

# Show plot
p3

# 2.2 Statistical Results
# 2.2.1 T-test
t_test_result <- t.test(val ~ sex_name, data = data_ckd_old)
t_test_result

# 3 Simulation Results
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
p4 <- ggplot(viz_df, aes(x = sample_size, y = estimated_difference, color = effect_size)) +
  geom_jitter(width = 5, height = 0, alpha = 0.4, size = 1.5)+
  facet_grid(~ noise, labeller = label_both) +
  labs(
    title = "Simulation of estimated gender differences in \nCKD mortality 
    across effect sizes, sample sizes, and noise",
    x = "Sample size per group",
    y = "Estimated sample mean difference \n in mortality rate (Male - Female)",
    color = "True effect size"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1.5, "lines")
  )

ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Simulation results across effect sizes, sample sizes and noise.png", plot = p4)

# Show Plot
p4