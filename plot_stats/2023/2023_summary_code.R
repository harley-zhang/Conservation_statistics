# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Read in raw/input CSV
input_data <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2023/cleanedMerged_Forestry23_030524_2.csv")

input_data <- input_data %>%
  rename(plot = new_plot_key)
#### TREATMENT STATISTICS ####

# Step 2: Treatment year
treatment_year <- input_data %>%
  group_by(plot) %>%
  summarise(treatment_year = ifelse(is.na(unique(treatment_year)) | unique(treatment_year) == 0, "Unknown", as.character(unique(treatment_year))))

# Step 3: Treatment type
treatment_type <- input_data %>%
  separate_rows(short_description_of_treament, sep = ",") %>%
  mutate(short_description_of_treament = trimws(short_description_of_treament)) %>%
  mutate(short_description_of_treament = case_when(
    short_description_of_treament == "THIN_FROM_BELOW" ~ "thin from below",
    TRUE ~ NA_character_
  )) %>%
  distinct(plot, short_description_of_treament) %>%
  group_by(plot) %>%
  summarise(treatment_type = if_else(all(short_description_of_treament %in% NA), "Unknown", paste(sort(na.omit(short_description_of_treament)), collapse = "; "))) %>%
  mutate(treatment_type = str_to_sentence(treatment_type, locale="en"))

#### TREE STATISTICS ####

# Step 4: Basal area per acre (in)
basal_area_per_acre_in <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(plot) %>%
  summarise(basal_area_per_acre_in = round(sum(0.005454 * (dbh_cm_tree/2.54)^2) * 5, 2))

# Step 5: Average DBH (in)
average_dbh_in <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(plot) %>%
  summarise(average_dbh_in = round(mean(dbh_cm_tree) / 2.54, 2))

# Step 6: Average height (ft)
average_height_ft <- input_data %>%
  filter(alive_or_dead == "living", !is.na(total_height_m_tree)) %>%
  group_by(plot) %>%
  summarise(average_height_ft = round(mean(total_height_m_tree) * 3.28084, 2))

# Step 7: Dominant tree species
dominant_tree_species <- input_data %>%
  filter(alive_or_dead == "living", size_class == "tree", !is.na(tree_species)) %>%
  group_by(plot) %>%
  summarise(dominant_tree_species = {
    species_counts <- table(tree_species)
    max_count <- max(species_counts)
    most_common_species <- names(species_counts)[species_counts == max_count]
    percent_frequency <- max_count / length(tree_species) * 100
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
      second_most_common_species <- names(sort(table(tree_species), decreasing = TRUE))[2]
      second_max_count <- table(tree_species)[second_most_common_species]
      second_percent_frequency <- second_max_count / length(tree_species) * 100
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

# Step 8: Regeneration presence (Y/N)
regeneration_presence <- input_data %>%
  group_by(plot) %>%
  summarise(regeneration_presence = ifelse(any((size_class == "sapling" & alive_or_dead == "living") | (size_class == "seedling" & number_of_seedlings > 0)), "Regeneration present", "Regeneration absent"))

# Step 9: Seedlings per acre
seedlings_per_acre <- input_data %>%
  filter(size_class == "seedling", !is.na(number_of_seedlings)) %>%
  group_by(plot) %>%
  summarise(seedlings_per_acre = sum(number_of_seedlings, na.rm = TRUE) * 50) %>%
  right_join(distinct(select(input_data, plot)), by = "plot") %>%
  mutate(seedlings_per_acre = if_else(is.na(seedlings_per_acre), 0, seedlings_per_acre))

# Step 10: Dominant regeneration species
dominant_regeneration_species <- input_data %>%
  filter((size_class == "sapling" & alive_or_dead == "living") | (size_class == "seedling" & number_of_seedlings > 0)) %>%
  group_by(plot, tree_species) %>%
  summarise(regeneration_count = sum(size_class == "sapling") + sum(ifelse(size_class == "seedling", number_of_seedlings, 0))) %>%
  group_by(plot) %>%
  mutate(percent_frequency = regeneration_count / sum(regeneration_count) * 100) %>%
  mutate(tree_species = case_when(
    tree_species == "ABCO" ~ "White fir",
    tree_species == "ABLA" ~ "Subalpine fir",
    tree_species == "ACGL" ~ "Rocky Mountain maple",
    tree_species == "JUSC" ~ "Rocky Mountain juniper",
    tree_species == "PIED" ~ "Colorado pinyon",
    tree_species == "PIEN" ~ "Engelmann spruce",
    tree_species == "PIFL" ~ "Limber pine",
    tree_species == "PIPO" ~ "Ponderosa pine",
    tree_species == "POTR" ~ "Aspen",
    tree_species == "PSME" ~ "Douglas fir",
    TRUE ~ as.character(tree_species)
  )) %>%
  arrange(desc(percent_frequency)) %>%
  summarise(dominant_regeneration_species = {
    if (percent_frequency[1] <= 50) {
      paste(tree_species[1], " (", round(percent_frequency[1], 2), "%), ", 
            tree_species[2], " (", round(percent_frequency[2], 2), "%)", sep = "")
    } else {
      paste(tree_species[1], " (", round(percent_frequency[1], 2), "%)", sep = "")
    }
  })

dominant_regeneration_species <- input_data %>%
  distinct(plot) %>%
  left_join(dominant_regeneration_species, by = "plot") %>%
  mutate(dominant_regeneration_species = ifelse(is.na(dominant_regeneration_species), "None", dominant_regeneration_species))

#### DAMAGE STATISTICS ####

# Step 11: Insect presence (Y/N)
insect_damage_presence <- input_data %>%
  filter(!is.na(insect_presence)) %>%
  group_by(plot) %>%
  summarise(insect_damage_presence = ifelse(any(insect_presence == 1), "Insect damage present", "Insect damage absent"))

# Step 12: Browse presence (Y/N)
browse_damage_presence <- input_data %>%
  filter(!is.na(browse_presence)) %>%
  group_by(plot) %>%
  summarise(browse_damage_presence = ifelse(any(browse_presence == 1), "Browse present", "Browse absent"))

# Step 13: List of damage types
list_damage <- input_data %>%
  mutate(
    what_if_any_disease_damage_present = tolower(what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = gsub("mechanicaldamamge", "mechanicaldamage", what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = gsub("woodpeckers", "woodpecker", what_if_any_disease_damage_present)
  ) %>%
  separate_rows(what_if_any_disease_damage_present, sep = ",") %>%
  mutate(
    what_if_any_disease_damage_present = trimws(what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = case_when(
      what_if_any_disease_damage_present == "barkbeetle" ~ "bark beetle",
      what_if_any_disease_damage_present == "browse" ~ "browse",
      what_if_any_disease_damage_present == "canker" ~ "canker",
      what_if_any_disease_damage_present == "douglasfiradelgid" ~ "Douglas fir adelgid",
      what_if_any_disease_damage_present == "fungus" ~ "fungus",
      what_if_any_disease_damage_present == "mistletoe" ~ "mistletoe",
      what_if_any_disease_damage_present == "galls" ~ "galls",
      what_if_any_disease_damage_present == "gash" ~ "gash",
      what_if_any_disease_damage_present == "mechanicaldamage" ~ "mechanical damage",
      what_if_any_disease_damage_present == "sapsucker" ~ "sapsucker",
      what_if_any_disease_damage_present == "sprucebudworm" ~ "spruce budworm",
      what_if_any_disease_damage_present == "winddamage" ~ "wind damage",
      what_if_any_disease_damage_present == "woodpecker" ~ "woodpecker",
      what_if_any_disease_damage_present == "rot" ~ "rot",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(plot, what_if_any_disease_damage_present) %>%
  group_by(plot) %>%
  summarise(
    list_damage = if_else(all(what_if_any_disease_damage_present %in% NA), "None", paste(sort(na.omit(what_if_any_disease_damage_present)), collapse = ", "))
  ) %>%
  mutate(
    list_damage = str_to_sentence(list_damage, locale="en"),
    list_damage = gsub("douglas", "Douglas", list_damage)
  )

# Step 14: Merge all outputs into one dataframe
output_statistics_2023 <- Reduce(function(x, y) merge(x, y, by = "plot", all = TRUE), 
                            list(treatment_year, treatment_type, basal_area_per_acre_in, average_dbh_in, average_height_ft, dominant_tree_species, 
                                 regeneration_presence, seedlings_per_acre, dominant_regeneration_species,
                                 insect_damage_presence, browse_damage_presence, list_damage))

# Step 15: Write output to CSV
write.csv(output_statistics_2023, file = "/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2023/2023_output_statistics.csv", row.names = FALSE)



#### SEPARATE ####

# Adult live tree count
adult_live_tree_count <- input_data %>%
  filter(alive_or_dead == "living", size_class == "tree") %>%
  group_by(plot) %>%
  summarise(adult_live_tree_count = n())

write.csv(adult_live_tree_count, file = "/Users/harley/Documents/adult_live_tree_count.csv", row.names = FALSE)