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

# Step 5: Basal area per acre (in)
average_basal_area_per_acre_in <- summary_merged %>%
  filter(!is.na(stand), basal_area_per_acre_in != "No live adult trees present") %>%
  group_by(stand) %>%
  mutate(basal_area_per_acre_in = as.double(basal_area_per_acre_in)) %>%
  summarise(average_basal_area_per_acre_in = round(mean(basal_area_per_acre_in), 2))

# Step 6: Average DBH (in)
average_dbh_in <- summary_merged %>%
  filter(!is.na(stand), average_dbh_in != "No live adult trees present") %>%
  group_by(stand) %>%
  mutate(average_dbh_in = as.double(average_dbh_in)) %>%
  summarise(average_dbh_in = round(mean(average_dbh_in), 2))

# Step 7: Average height (ft)
average_height_ft <- summary_merged %>%
  filter(!is.na(stand), average_height_ft != "No live adult trees present") %>%
  group_by(stand) %>%
  mutate(average_height_ft = as.double(average_height_ft)) %>%
  summarise(average_height_ft = round(mean(average_height_ft), 2))

# Step 7: Dominant tree species




# Step XX: Write output to CSV
write.csv(stand_summary, file = "/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_summary.csv", row.names = FALSE)
