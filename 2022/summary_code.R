# Load required libraries
library(dplyr)
library(tidyr)


# Step 1: Read in cleanedUnderstory.csv and cleanedOverstory
understory_data <- read.csv("/Users/harley/Documents/Trinchera_summary/2022/cleanedUnderstory.csv")
overstory_data <- read.csv("/Users/harley/Documents/Trinchera_summary/2022/cleanedOverstory.csv")


# Step 2: Creating filtered_overstory_data with only living trees
filtered_overstory_data <- overstory_data %>% filter(living_dead == "L" | is.na(living_dead))
all_plots <- data.frame(plot = unique(overstory_data$plot))

# Step 3: Basal Area Per Acre (in)
output_summary <- all_plots %>%
  left_join(filtered_overstory_data %>%
              group_by(plot) %>%
              summarize(basal_area_per_acre_in = round(sum(0.005454 * (dbh/2.54)^2) * 5, 2)),
            by = "plot")


# Step 4: Dominant Tree Species
dominant_species <- filtered_overstory_data %>%
  group_by(plot, species) %>%
  summarise(count = n()) %>%
  top_n(1, count) %>%
  select(-count) %>%
  mutate(dominant_tree_species = case_when(
    species == "PSME" ~ "Douglas fir",
    species == "PIPO" ~ "Ponderosa pine",
    species == "POTR" ~ "Aspen",
    species == "JUSC" ~ "Rocky Mountain juniper",
    species == "ABCO" ~ "White fir",
    species == "PIEN" ~ "Engelmann spruce",
    species == "PIFL" ~ "Limber pine",
    species == "UNK" ~ "Unknown",
    species == "PIED" ~ "Colorado pinyon"
  )) %>%
  select(plot, dominant_tree_species)

output_summary <- left_join(output_summary, dominant_species, by = "plot")


# Step 5: Average DBH (in) and Height (ft)
average_dbh_and_height <- filtered_overstory_data %>%
  group_by(plot) %>%
  summarize(average_dbh_in = round(mean(dbh/2.54), 2),
            average_height_ft = round(mean(total_height_m) * 3.28084, 2)
  )

output_summary <- left_join(output_summary, average_dbh_and_height, by = "plot")


# Step 6: Regeneration Presence
regeneration_presence <- understory_data %>%
  group_by(plot) %>%
  summarize(regeneration_presence = if_else(all(species == "none"), "Regeneration absent", "Regeneration present"))

output_summary <- left_join(output_summary, regeneration_presence, by = "plot")


# Step 7: Insect or Disease Damage Presence
insect_or_disease_overstory <- overstory_data %>%
  group_by(plot) %>%
  summarize(insect_or_disease_overstory = if_else(any(insect_disease == "Y"), "Insect/disease damage present", "Insect/disease damage absent"))

insect_or_disease_understory <- understory_data %>%
  group_by(plot) %>%
  summarize(insect_or_disease_understory = if_else(any(insect_disease_presence == 1), "Insect/disease damage present", "Insect/disease damage absent"))

output_summary <- left_join(output_summary, insect_or_disease_overstory, by = "plot")
output_summary <- left_join(output_summary, insect_or_disease_understory, by = "plot")

output_summary$insect_or_disease_presence <- if_else(
  !is.na(output_summary$insect_or_disease_overstory) & output_summary$insect_or_disease_overstory == "Insect/disease damage present",
  "Insect/disease damage present",
  if_else(
    !is.na(output_summary$insect_or_disease_understory) & output_summary$insect_or_disease_understory == "Insect/disease damage present",
    "Insect/disease damage present",
    "Insect/disease damage absent"
  )
)

output_summary <- select(output_summary, -c(insect_or_disease_overstory, insect_or_disease_understory))


# Step 8: Browse Damage Presence
browse_overstory <- overstory_data %>%
  group_by(plot) %>%
  summarize(browse_overstory = if_else(any(browsing_damage == "Y"), "Browse damage present", "Browse damage absent"))

browse_understory <- understory_data %>%
  group_by(plot) %>%
  summarize(browse_understory = if_else(any(browsing_damage == 1), "Browse damage present", "Browse damage absent"))

output_summary <- left_join(output_summary, browse_overstory, by = "plot")
output_summary <- left_join(output_summary, browse_understory, by = "plot")

output_summary$browse_presence <- if_else(
  !is.na(output_summary$browse_overstory) & output_summary$browse_overstory == "Browse damage present",
  "Browse damage present",
  if_else(
    !is.na(output_summary$browse_understory) & output_summary$browse_understory == "Browse damage present",
    "Browse damage present",
    "Browse damage absent"
  )
)

output_summary <- select(output_summary, -c(browse_overstory, browse_understory))


# Export the final summary to CSV
write.csv(output_summary, "/Users/harley/Documents/Trinchera_summary/2022/output_summary.csv", row.names = FALSE)
