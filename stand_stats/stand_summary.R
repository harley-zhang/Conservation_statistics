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

#### TREE STATISTICS ####

# Step 5: Average basal area per acre (in)
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

# Step 8: Dominant tree species


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
  mutate(seedlings_per_acre = as.integer(seedlings_per_acre)) %>%
  summarise(average_seedlings_per_acre = round(mean(seedlings_per_acre), 2))

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



# Step XX: Merge all outputs into one dataframe
stand_summary <- Reduce(function(x, y) merge(x, y, by = "stand", all = TRUE), 
                        list(average_basal_area_per_acre_in, average_dbh_in, average_height_ft, 
                             regeneration_presence, average_seedlings_per_acre,
                             insect_damage_presence, browse_damage_presence, list_damage))

# Step XX: Write output to CSV
write.csv(stand_summary, file = "/Users/harley/Documents/Github/Trinchera_summary/stand_stats/stand_summary.csv", row.names = FALSE)
