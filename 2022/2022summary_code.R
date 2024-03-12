# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Read in cleanedUnderstory.csv and cleanedOverstory
understory_data <- read.csv("/Users/harley/Documents/Trinchera_summary/cleanedUnderstory.csv")
overstory_data <- read.csv("/Users/harley/Documents/Trinchera_summary/cleanedOverstory.csv")

# Step 2: Remove rows in overstory_data where living_dead is not "L"
overstory_data <- overstory_data %>% filter(living_dead == "L")

# Step 3: Create output_summary and calculate basal_area_per_acre_in
output_summary <- overstory_data %>%
  mutate(basal_area = 0.005454 * (dbh/2.54)^2) %>%
  group_by(plot) %>%
  summarize(
    basal_area_per_acre_in = round(sum(basal_area) * 5, 2),
    dominant_tree_species = names(sort(table(species), decreasing = TRUE))[1],
    average_dbh_in = round(mean(dbh)/2.54, 2),
    average_height_ft = round(mean(total_height_m) * 3.28084, 2)
  )

# Step 4: Determine regeneration presence in understory_data
regeneration_summary <- understory_data %>%
  group_by(plot) %>%
  summarize(regeneration_presence = ifelse(all(species == "none"), "Regeneration absent", "Regeneration present"))

# Step 5: Merge regeneration_summary with output_summary based on plot
output_summary <- left_join(output_summary, regeneration_summary, by = "plot")


# Step 6: Replace the abbreviated species names with their real names
output_summary$dominant_tree_species <- case_when(
  output_summary$dominant_tree_species == "PSME" ~ "Douglas fir",
  output_summary$dominant_tree_species == "PIPO" ~ "Ponderosa pine",
  output_summary$dominant_tree_species == "POTR" ~ "Aspen",
  output_summary$dominant_tree_species == "JUSC" ~ "Rocky mountain juniper",
  output_summary$dominant_tree_species == "ABCO" ~ "White fir",
  output_summary$dominant_tree_species == "PIEN" ~ "Engelmann spruce",
  output_summary$dominant_tree_species == "PIFL" ~ "Limber pine",
  output_summary$dominant_tree_species == "UNK" ~ "Unknown",
  output_summary$dominant_tree_species == "PIED" ~ "Colorado pinyon",
  TRUE ~ "Other"
)

# Step 7: Export output_summary as CSV
write.csv(output_summary, file = "/Users/harley/Documents/Trinchera_summary/output_summary.csv", row.names = FALSE)
