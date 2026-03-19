# Packages
library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(knitr)

# Set working directory
setwd("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_CP05_Jingxin-Yuan/Data/Original data")

# Load file
data_ckd = read.csv("IHME-GBD_CKD_20260130.csv")

# Preview the data
head(data_ckd)

#Check if it contains NA
n_na <- sum(is.na(data_ckd))
cat("The amount of NA is",n_na,end='\n')

#Check available years
sort(unique(data_ckd$year))

#Check of Abnormal Value
#Define IQR measure to detect abnormal value
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x < lower_bound | x > upper_bound)
}

#Detected column
numeric_cols <- c("val", "upper", "lower")

#Detect
outliers_df <- data_ckd %>%
  mutate(across(all_of(numeric_cols), detect_outliers))

outlier_summary <- colSums(outliers_df[numeric_cols], na.rm = TRUE)
print(outlier_summary)

#Replace abnormal value with median value
median_vals <- apply(data_ckd[numeric_cols], 2, median, na.rm = TRUE)

data_ckd[numeric_cols] <- lapply(numeric_cols, function(col) {
  data_ckd[[col]] <- ifelse(outliers_df[[col]], median_vals[col], data_ckd[[col]])
})

#Output processed data
write.csv(data_ckd, "C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_CP05_Jingxin-Yuan/Data/Processed data/IHME-GBD_CKD_Processed.csv")

