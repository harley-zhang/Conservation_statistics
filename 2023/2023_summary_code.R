# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Read in the CSV file
input_data <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/2023/cleanedMerged_Forestry23_030524_2.csv")

# Step 2: Calculate basal area per acre
basal_area <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(new_plot_key) %>%
  summarise(basal_area_per_acre_in = round(sum(0.005454 * (dbh_cm_tree/2.54)^2) * 5, 2))

# Step 3: Calculate average dbh in inches
average_dbh <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(new_plot_key) %>%
  summarise(average_dbh_in = round(mean(dbh_cm_tree) / 2.54, 2))

# Step 4: Calculate average height in feet
average_height <- input_data %>%
  filter(alive_or_dead == "living", !is.na(total_height_m_tree)) %>%
  group_by(new_plot_key) %>%
  summarise(average_height_ft = round(mean(total_height_m_tree) * 3.28084, 2))

# Step 5: Check for regeneration presence
regeneration_presence <- input_data %>%
  filter(alive_or_dead == "living", !is.na(size_class)) %>%
  group_by(new_plot_key) %>%
  summarise(regeneration_presence = ifelse(any(size_class == "sapling" | size_class == "seedling"), "Regeneration present", "No regeneration"))

# Step 6: Check for insect presence
insect_damage_presence <- input_data %>%
  filter(!is.na(insect_presence)) %>%
  group_by(new_plot_key) %>%
  summarise(insect_damage_presence = ifelse(any(insect_presence == 1), "Insect damage present", "No insect damage"))

# Step 7: Check for browse presence
browse_damage_presence <- input_data %>%
  filter(!is.na(browse_presence)) %>%
  group_by(new_plot_key) %>%
  summarise(browse_damage_presence = ifelse(any(browse_presence == 1), "Browse present", "No browse present"))

# Step 8: Find the dominant tree species for each plot
dominant_tree_species <- input_data %>%
  filter(alive_or_dead == "living", size_class == "tree", !is.na(tree_species)) %>%
  group_by(new_plot_key) %>%
  summarise(dominant_tree_species = {
    species_counts <- table(tree_species)
    max_count <- max(species_counts)
    most_common_species <- names(species_counts)[species_counts == max_count]
    percent_frequency <- max_count / length(tree_species) * 100
    species_name <- switch(tolower(most_common_species[1]),
                           "PSME" = "Douglas fir",
                           "PIPO" = "Ponderosa pine",
                           "POTR" = "Aspen",
                           "JUSC" = "Rocky Mountain juniper",
                           "PIED" = "Colorado pinyon",
                           "ABCO" = "White fir",
                           "PIFL" = "Limber pine",
                           "ACGL" = "acgl_")
    if (percent_frequency <= 50) {
      second_most_common_species <- names(sort(table(tree_species), decreasing = TRUE))[2]
      second_max_count <- table(tree_species)[second_most_common_species]
      second_percent_frequency <- second_max_count / length(tree_species) * 100
      second_species_name <- switch(tolower(second_most_common_species),
                                    "PSME" = "Douglas fir",
                                    "PIPO" = "Ponderosa pine",
                                    "POTR" = "Aspen",
                                    "JUSC" = "Rocky Mountain juniper",
                                    "PIED" = "Colorado pinyon",
                                    "ABCO" = "White fir",
                                    "PIFL" = "Limber pine",
                                    "ACGL" = "acgl_")
      percent_frequency <- format(round(percent_frequency, 2), nsmall = 2)
      second_percent_frequency <- format(round(second_percent_frequency, 2), nsmall = 2)
      paste(species_name, " (", percent_frequency, "%), ", 
            second_species_name, " (", second_percent_frequency, "%)", sep = "")
    } else {
      percent_frequency <- format(round(percent_frequency, 2), nsmall = 2)
      paste(species_name, " (", percent_frequency, "%)", sep = "")
    }
  })

# Merge all outputs into one dataframe
output_statistics <- Reduce(function(x, y) merge(x, y, by = "new_plot_key", all = TRUE), 
                            list(basal_area, average_dbh, 
                                 average_height, regeneration_presence, 
                                 insect_damage_presence, browse_damage_presence, 
                                 dominant_tree_species))

# Write the output to CSV
write.csv(output_statistics, file = "/Users/harley/Documents/output_statistics.csv", row.names = FALSE)
