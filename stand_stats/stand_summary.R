# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Read in raw/input CSVs
summary_2022 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2022/2022_summary.csv")
summary_2023 <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/plot_stats/2023/2023_summary.csv")
stand_plots <- read.csv("/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_plots.csv")

# Step 2: Convert data frames all to <character>
summary_2022 <- summary_2022 %>%
  mutate(treatment_year = as.character(treatment_year),
         basal_area_per_acre_in = as.numeric(basal_area_per_acre_in),
         average_dbh_in = as.numeric(average_dbh_in),
         average_height_ft = as.numeric(average_height_ft)) %>%
  rename(dominant_tree = dominant_tree_species,
         dominant_regeneration = dominant_regeneration_species)

summary_2023 <- summary_2023 %>%
  mutate(treatment_year = as.character(treatment_year),
         basal_area_per_acre_in = as.numeric(basal_area_per_acre_in),
         average_dbh_in = as.numeric(average_dbh_in),
         average_height_ft = as.numeric(average_height_ft)) %>%
  rename(dominant_tree = dominant_tree_species,
         dominant_regeneration = dominant_regeneration_species)


# Step 3: Merge data frames
summary_merged <- bind_rows(summary_2022, summary_2023)

# Step 4: Assign stands to each plot in the merged data frame
summary_merged <- right_join(stand_plots, summary_merged, by = "plot")

# Step 5: When there is a second species in `dominant_tree_species`, get rid of the second one unless same %
remove_second_dom <- function(species) {
  species_list <- strsplit(species, ",")[[1]]
  if (length(species_list) == 1) {
    return(species)
  }
  if (length(species_list) == 2) {
    percent1 <- as.numeric(gsub("[^0-9.]", "", species_list[1]))
    percent2 <- as.numeric(gsub("[^0-9.]", "", species_list[2]))
    if (percent1 == percent2) {
      return(species)
    } else {
      if (percent1 > percent2) {
        return(species_list[1])
      } else {
        return(species_list[2])
      }
    }
  }
}

summary_merged <- summary_merged %>%
  mutate(
    dominant_tree = sapply(dominant_tree, remove_second_dom),
    dominant_regeneration = sapply(dominant_regeneration, remove_second_dom)
  )


#### TREE STATISTICS ####

# Step 5: Average basal area per acre (in)
average_basal_area_per_acre_in <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(average_basal_area_per_acre_in = round(mean(basal_area_per_acre_in), 2))

# Step 6: Average DBH (in)
average_dbh_in <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(average_dbh_in = round(mean(average_dbh_in), 2))

# Step 7: Average height (ft)
average_height_ft <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(average_height_ft = round(mean(average_height_ft), 2))

# Step 8: Dominant tree species
dominant_tree_species <- summary_merged %>%
  filter(!is.na(stand)) %>%
  mutate(dominant_tree = str_extract_all(dominant_tree, "(Aspen|Douglas fir|Colorado Pinyon|Engelmann spruce|Limber pine|Ponderosa pine|Rocky Mountain juniper|Rocky Mountain maple|Subalpine fir|White fir|No live adult trees present)")) %>%
  group_by(stand) %>%
  mutate(total = sum(n())) %>%
  ungroup() %>%
  unnest(dominant_tree) %>%
  group_by(stand, dominant_tree) %>%
  summarise(occurrences = n(),
            total = unique(total)) %>%
  group_by(stand) %>%
  mutate(percent_frequency = occurrences / total) %>%
  mutate(dominant_tree_species = paste0(dominant_tree, " ", occurrences, " plots, ", round(percent_frequency*100, 2), "% of plots")) %>%
  mutate(rank = ifelse(dominant_tree == "No live adult trees present", occurrences - max(occurrences), occurrences)) %>%
  group_by(stand) %>%
  slice(which.max(rank)) %>%
  select(stand, dominant_tree_species)


#### REGENERATION STATISTICS ####

# Step 8: Regeneration presence (Y/N)
regeneration_presence <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(regeneration_presence = ifelse(any(regeneration_presence == "Regeneration present"), "Regeneration present", "Regeneration absent"))

# Step 9: Average seedlings per acre
average_seedlings_per_acre <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(average_seedlings_per_acre = round(mean(seedlings_per_acre), 2))

# Step 10: Dominant regeneration species
dominant_regeneration_species <- summary_merged %>%
  filter(!is.na(stand)) %>%
  mutate(dominant_regeneration = str_extract_all(dominant_regeneration, "(Aspen|Douglas fir|Colorado Pinyon|Engelmann spruce|Limber pine|Ponderosa pine|Rocky Mountain juniper|Rocky Mountain maple|Subalpine fir|White fir|No live adult trees present)")) %>%
  group_by(stand) %>%
  mutate(total = sum(n())) %>%
  ungroup() %>%
  unnest(dominant_regeneration) %>%
  group_by(stand, dominant_regeneration) %>%
  summarise(occurrences = n(),
            total = unique(total)) %>%
  group_by(stand) %>%
  mutate(percent_frequency = occurrences / total) %>%
  mutate(dominant_regeneration_species = paste0(dominant_regeneration, " ", occurrences, " plots, ", round(percent_frequency*100, 2), "% of plots")) %>%
  mutate(rank = ifelse(dominant_regeneration == "No live adult trees present", occurrences - max(occurrences), occurrences)) %>%
  group_by(stand) %>%
  slice(which.max(rank)) %>%
  select(stand, dominant_regeneration_species)


#### DAMAGE STATISTICS ####

# Step 11: Insect presence (Y/N)
insect_damage_presence <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(insect_damage_presence = ifelse(any(insect_damage_presence == "Insect damage present"), "Insect damage present", "Insect damage absent"))

# Step 12: Browse presence (Y/N)
browse_damage_presence <- summary_merged %>%
  filter(!is.na(stand)) %>%
  group_by(stand) %>%
  summarise(browse_damage_presence = ifelse(any(browse_damage_presence == "Browse present"), "Browse present", "Browse absent"))

# Step 13: List of damage types
list_damage <- summary_merged %>%
  filter(!is.na(stand)) %>%
  mutate(
    list_damage = tolower(list_damage),
    list_damage = gsub("\\s+", "", list_damage),
  ) %>%
  separate_rows(list_damage, sep = ",") %>%
  mutate(
    list_damage = case_when(
      list_damage == "barkbeetle" ~ "bark beetle",
      list_damage == "browse" ~ "browse",
      list_damage == "canker" ~ "canker",
      list_damage == "douglas fir adelgid" ~ "douglas fir adelgid",
      list_damage == "fungus" ~ "fungus",
      list_damage == "mistletoe" ~ "mistletoe",
      list_damage == "galls" ~ "galls",
      list_damage == "gash" ~ "gash",
      list_damage == "mechanicaldamage" ~ "mechanical damage",
      list_damage == "sapsucker" ~ "sapsucker",
      list_damage == "sprucebudworm" ~ "spruce budworm",
      list_damage == "wind damage" ~ "wind damage",
      list_damage == "woodpecker" ~ "woodpecker",
      list_damage == "rot" ~ "rot",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(stand, list_damage) %>%
  group_by(stand) %>%
  summarise(
    list_damage = if_else(all(list_damage %in% NA), "None", paste(sort(na.omit(list_damage)), collapse = ", "))
  ) %>%
  mutate(
    list_damage = str_to_sentence(list_damage, locale="en"),
    list_damage = gsub("douglas", "Douglas", list_damage)
  )


#### MERGE TO CREATE SUMMARY ####

# Step 14: Merge all outputs into one dataframe
stand_summary <- Reduce(function(x, y) merge(x, y, by = "stand", all = TRUE), 
                        list(average_basal_area_per_acre_in, average_dbh_in, average_height_ft, dominant_tree_species,
                             regeneration_presence, average_seedlings_per_acre, dominant_regeneration_species,
                             insect_damage_presence, browse_damage_presence, list_damage))

# Step 15: Write output to CSV
write.csv(stand_summary, file = "/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_summary.csv", row.names = FALSE)
