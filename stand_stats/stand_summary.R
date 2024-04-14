# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Read in raw/input CSVs
summary_2022 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2022/2022_summary.csv")
summary_2023 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2023/2023_summary.csv")
stand_plots <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_plots.csv")

# Step 2: Convert data frames all to <character>
summary_2022 <- mutate_all(summary_2022, as.character)
summary_2023 <- mutate_all(summary_2023, as.character)

# Step 3: Merge data frames
summary_merged <- bind_rows(summary_2022, summary_2023)

# Step 4: Assign stands to each plot in the merged data frame
summary_merged <- left_join(summary_merged, stand_plots, by = "plot") %>%
  mutate(stand = if_else(is.na(stand), NA_character_, stand)) %>%
  select(stand, everything())


# Step XX: Write output to CSV
write.csv(stand_summary, file = "/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_summary.csv", row.names = FALSE)
