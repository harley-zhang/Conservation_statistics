# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Read in raw/input CSVs
output_summary_2022 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2022/2022_output_summary.csv")
output_summary_2023 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2023/2023_output_summary.csv")

# Step 2: 


# Step XX: Write output to CSV
write.csv(stand_summary, file = "/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_summary.csv", row.names = FALSE)
