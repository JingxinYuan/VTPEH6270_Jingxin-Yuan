# Packages
library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(knitr)

# Part 1: Load data
# Set working directory
setwd("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Data/Original data")

# Load file
data_ckd = read.csv("IHME-GBD_CKD_20260130.csv")

# Preview the data
head(data_ckd)


# Part 2: Data Cleaning and Checking
# Check if it contains NA
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


# Part 3: Variable Overview
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


# Part 4: Distribution Checking
# Data Visualiztion
# Histogram on the burden of CKD of 60+ years people in China
# Create Scubset
unique(data_ckd$measure_name)

data_ckd_plot = data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name == "DALYs (Disability-Adjusted Life Years)"
  )

# Histogram
mean_d <- mean(data_ckd_plot$val, na.rm = TRUE)
median_d <- median(data_ckd_plot$val, na.rm = TRUE)

vlines <- data.frame(
  stat = c("Mean", "Median"),
  xint = c(mean_d, median_d)
)

p1 <- ggplot(data_ckd_plot, aes(x = val)) +
  geom_histogram(bins = 25, fill = "grey") +
  geom_vline(
    data = vlines,
    aes(xintercept = xint, linetype = stat)
  ) +
  scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "dotted")) +
  theme_classic() +
  labs(
    title = "Distribution of CKD DALYs Rate of\n 60+ Years Old People in China (2013–2023)",
    x = "DALYs rate (per 100,000)",
    y = "Count",
    linetype = "Statistic",
    caption = "Figure 1. Histogram of CKD DALYs rate in China with mean and median lines."
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Distribution on the burden of CKD of 60+ years people in China.png",plot = p1)


# Part 5: Broad trend exploration
#Create subset
data_ckd_scatterplot <- data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name %in% c("Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths")
  )
# Scatterplot on the burden of CKD of 60+ years people in China
p2 <- ggplot(data_ckd_scatterplot, aes(x = year, y = val, color = sex_name)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~ measure_name, ncol = 1, scales = "free_y") +
  theme_minimal() +
  scale_x_continuous(breaks = 2013:2023) +
  labs(
    title = "Trends in CKD Prevalence, DALYs and Death Rate of \n 60+ Years Old People in China (2013–2023)",
    x = "Year",
    y = "Rate (per 100,000)",
    color = "Sex",
    caption = "Figure 2. Trends of CKD prevalence, DALYs and death rates of \n 60+ Years Old People in China from 2013 to 2023, stratified \n by sex. Shaded areas represent the 95% confidence interval."
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Scatterplot on the burden of CKD of 60+ years people in China.png",plot = p2)


# Part 6: Focused subset for mortality and gender
# Filter 60+ years old adults
data_ckd_old <- data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name == "Deaths"
  ) %>%
  select(year,sex_name,val)

# Grouped summary statistics
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

# Mortality difference
data_ckd_old_diff <- data_ckd_old %>%
  pivot_wider(names_from = sex_name, values_from = val) %>%
  mutate(
    mortality_difference = Male - Female
  ) %>%
  select(year, mortality_difference)

data_ckd_old <- data_ckd_old %>%
  left_join(data_ckd_old_diff, by = "year")
head(data_ckd_old)

# Comparative visualization on mortality
p3 <- ggplot(data_ckd_old, aes(x = year, y = val, color = sex_name)) +
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
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Gender trend on mortality of CKD of 60+ years people in China.png",plot = p3)

# Visualize mortality difference between male and female
p4 <- ggplot(data_ckd_old, aes(x = year, y = mortality_difference)) +
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
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Mortality difference between gender of CKD of 60+ years people in China.png",plot = p4)