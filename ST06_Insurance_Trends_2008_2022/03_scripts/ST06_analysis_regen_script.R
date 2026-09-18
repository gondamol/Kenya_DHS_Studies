# Load required libraries
library(tidyverse)

# Load the backup CSV file as a data frame
data <- read.csv("C:/Users/HFD 2/Research/02_Studies/ST06_Insurance_Trends_2008_2022/02_data_notes/ST06_combined_analytic.csv", header=TRUE)

# Inspect the structure of the data
str(data)

# Save as .rds format for future compatibility
saveRDS(data, file="02_Studies/ST06_Insurance_Trends_2008_2022/07_derived_data/st06_analysis_outputs.rds")

# Validate the newly saved .rds file
reloaded_data <- readRDS("02_Studies/ST06_Insurance_Trends_2008_2022/07_derived_data/st06_analysis_outputs.rds")
str(reloaded_data)
