# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Read in the CSV
input_data <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/2022/2024Updated_2022_ForestMont_MergedData.csv")

#### TREE STATISTICS ####

# Step 2: Basal area per acre (in)
basal_area <- input_data %>%
  filter(living_dead == "L", !is.na(dbh)) %>%
  group_by(plot) %>%
  summarise(basal_area_per_acre_in = round(sum(0.005454 * (dbh/2.54)^2) * 5, 2))

# Step 3: Average DBH (in)
average_dbh <- input_data %>%
  filter(living_dead == "L", !is.na(dbh)) %>%
  group_by(plot) %>%
  summarise(average_dbh_in = round(mean(dbh) / 2.54, 2))

# Step 4: Average height (ft)
average_height <- input_data %>%
  filter(living_dead == "L", !is.na(total_height)) %>%
  group_by(plot) %>%
  summarise(average_height_ft = round(mean(total_height) * 3.28084, 2))

# Step 5: Dominant tree species
dominant_tree_species <- input_data %>%
  filter(living_dead == "L", size_class == "tree", !is.na(species)) %>%
  group_by(plot) %>%
  summarise(dominant_tree_species = {
    species_counts <- table(species)
    max_count <- max(species_counts)
    most_common_species <- names(species_counts)[species_counts == max_count]
    percent_frequency <- max_count / length(species) * 100
    species_name <- switch(most_common_species[1],
                           "ABCO" = "White fir",
                           "ABLA" = "Subalpine fir",
                           "ACGL" = "Rocky Mountain maple",
                           "JUSC" = "Rocky Mountain juniper",
                           "PIED" = "Colorado pinyon",
                           "PIEN" = "Engelmann spruce",
                           "PIFL" = "Limber pine",
                           "PIPO" = "Ponderosa pine",
                           "POTR" = "Aspen",
                           "PSME" = "Douglas fir")
    if (percent_frequency <= 50) {
      second_most_common_species <- names(sort(table(species), decreasing = TRUE))[2]
      second_max_count <- table(species)[second_most_common_species]
      second_percent_frequency <- second_max_count / length(species) * 100
      second_species_name <- switch(second_most_common_species,
                                    "ABCO" = "White fir",
                                    "ABLA" = "Subalpine fir",
                                    "ACGL" = "Rocky Mountain maple",
                                    "JUSC" = "Rocky Mountain juniper",
                                    "PIED" = "Colorado pinyon",
                                    "PIEN" = "Engelmann spruce",
                                    "PIFL" = "Limber pine",
                                    "PIPO" = "Ponderosa pine",
                                    "POTR" = "Aspen",
                                    "PSME" = "Douglas fir")
      percent_frequency <- format(round(percent_frequency, 2), nsmall = 2)
      second_percent_frequency <- format(round(second_percent_frequency, 2), nsmall = 2)
      paste(species_name, " (", percent_frequency, "%), ", 
            second_species_name, " (", second_percent_frequency, "%)", sep = "")
    } else {
      percent_frequency <- format(round(percent_frequency, 2), nsmall = 2)
      paste(species_name, " (", percent_frequency, "%)", sep = "")
    }
  })

#### REGENERATION STATISTICS ####

# Step 6: Regeneration presence (Y/N)
regeneration_presence <- input_data %>%
  group_by(plot) %>%
  summarise(regeneration_presence = ifelse(any((size_class == "sapling" & living_dead == "L") | (size_class == "seedling" & count_seedling > 0)), "Regeneration present", "Regeneration absent"))

# Step 7: Seedlings per acre
seedlings_per_acre <- input_data %>%
  filter(size_class == "seedling", !is.na(count_seedling)) %>%
  group_by(plot) %>%
  summarise(seedlings_per_acre = sum(count_seedling, na.rm = TRUE) * 50) %>%
  right_join(distinct(select(input_data, plot)), by = "plot") %>%
  mutate(seedlings_per_acre = if_else(is.na(seedlings_per_acre), 0, seedlings_per_acre))

# Step 8: Dominant regeneration species
dominant_regeneration_species <- input_data %>%
  filter((size_class == "sapling" & living_dead == "L") | (size_class == "seedling" & count_seedling > 0)) %>%
  group_by(plot, species) %>%
  summarise(regeneration_count = sum(size_class == "sapling") + sum(ifelse(size_class == "seedling", count_seedling, 0))) %>%
  group_by(plot) %>%
  mutate(percent_frequency = regeneration_count / sum(regeneration_count) * 100) %>%
  mutate(species = case_when(
    species == "ABCO" ~ "White fir",
    species == "ABLA" ~ "Subalpine fir",
    species == "ACGL" ~ "Rocky Mountain maple",
    species == "JUSC" ~ "Rocky Mountain juniper",
    species == "PIED" ~ "Colorado pinyon",
    species == "PIEN" ~ "Engelmann spruce",
    species == "PIFL" ~ "Limber pine",
    species == "PIPO" ~ "Ponderosa pine",
    species == "POTR" ~ "Aspen",
    species == "PSME" ~ "Douglas fir",
    TRUE ~ as.character(species)
  )) %>%
  arrange(desc(percent_frequency)) %>%
  summarise(dominant_regeneration_species = {
    if (percent_frequency[1] <= 50) {
      paste(species[1], " (", round(percent_frequency[1], 2), "%), ", 
            species[2], " (", round(percent_frequency[2], 2), "%)", sep = "")
    } else {
      paste(species[1], " (", round(percent_frequency[1], 2), "%)", sep = "")
    }
  })

dominant_regeneration_species <- input_data %>%
  distinct(plot) %>%
  left_join(dominant_regeneration_species, by = "plot") %>%
  mutate(dominant_regeneration_species = ifelse(is.na(dominant_regeneration_species), "None", dominant_regeneration_species))

#### DAMAGE STATISTICS ####

# Step 9: Insect presence (Y/N)
insect_damage_presence <- input_data %>%
  filter(!is.na(insect_disease)) %>%
  group_by(plot) %>%
  summarise(insect_damage_presence = ifelse(any(insect_disease == 1), "Insect damage present", "Insect damage absent"))

# Step 10: Browse presence (Y/N)
browse_damage_presence <- input_data %>%
  filter(!is.na(browsing_damage)) %>%
  group_by(plot) %>%
  summarise(browse_damage_presence = ifelse(any(browsing_damage == 1), "Browse present", "Browse absent"))

# Merge all outputs into one dataframe
output_statistics_2022 <- Reduce(function(x, y) merge(x, y, by = "plot", all = TRUE), 
                            list(basal_area, average_dbh, average_height, dominant_tree_species, 
                                 regeneration_presence, seedlings_per_acre, dominant_regeneration_species,
                                 insect_damage_presence, browse_damage_presence))

# Write output to CSV
write.csv(output_statistics_2022, file = "/Users/harley/Documents/Github/Trinchera_summary/2022/2022_output_statistics.csv", row.names = FALSE)
