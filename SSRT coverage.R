# Test case for SSRT coverage
# Export SFW mapping dictionary to CSV, if available preferably CSV UTF-8
# this supports virtually all characters (including non-Latin alphabets) and uses standard line endings (\n)
# read.csv and read_csv functions designed specifically for UTF-8



# This script reads in the SFW Mapping Dictionary CSV file, calculates the total volume for specific groups,
# and then visualizes the results as a donut chart of SSRT coverage of GLOBAL PRODUCTION.
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(purrr)

## FIXIT - create script for adding SSRT.YN column to SFW Mapping Dictionary file and identifying Y's 
# Read the CSV file into a data frame.
df <- read.csv("Data/2025 SFW Mapping Dictionary - SSRT coverage.csv", header = TRUE)

# Replace blank rows (empty strings) and NA values in SSRT.YN with "N".
df <- df %>%
  mutate(SSRT.YN = if_else(is.na(SSRT.YN) | SSRT.YN == "", "N", SSRT.YN))

# Create categories for SSRT coverage comparisons; do this by breaking up ISSCAAP groups which are too broad
# df %>% filter(ISSCAAP.group == "Squids, cuttlefishes, octopuses") %>% select(Scientific.name) %>% distinct()
# df %>% filter(ISSCAAP.group == "Tunas, bonitos, billfishes") %>% select(Scientific.name) %>% distinct()
# Use AI to classify scientific names as squids vs cuttlefishes vs octopuses and return as a character string list in quotation marks
squid_species <- c("Alloteuthis media", "Alloteuthis spp", "Alloteuthis subulata", "Berryteuthis magister", "Dosidicus gigas", "Doryteuthis gahi", "Doryteuthis opalescens", "Doryteuthis pealeii", "Doryteuthis pleii", "Histioteuthis spp", "Illex argentinus", "Illex coindetii", "Illex illecebrosus", "Illex spp", "Loliginidae", "Loliginidae, Ommastrephidae", "Loligo forbesii", "Loligo reynaudii", "Loligo spp", "Loligo vulgaris", "Lolliguncula diomedeae", "Martialia hyadesi", "Nototodarus sloanii", "Ommastrephes bartramii", "Ommastrephes spp", "Ommastrephidae", "Onykia spp", "Sepioteuthis lessoniana", "Sepioteuthis sepioidea", "Sthenoteuthis pteropus", "Todarodes filippovae", "Todarodes pacificus", "Todarodes sagittatus", "Todarodes spp", "Todaropsis eblanae", "Uroteuthis (Photololigo) duvaucelii")
cuttlefish_species <- c("Rossia macrosoma", "Sepia bertheloti", "Sepia elegans", "Sepia officinalis", "Sepia orbignyana", "Sepia pharaonis", "Sepia spp", "Sepiola rondeletii", "Sepiidae, Sepiolidae") # includes BOBTAIL SQUID
octopus_species <- c("Callistoctopus macropus", "Eledone cirrhosa", "Eledone moschata", "Eledone spp", "Enteroctopus magnificus", "Octopodidae", "Octopus maya", "Octopus salutii", "Octopus spp", "Octopus vulgaris", "Pinnoctopus cordiformis", "Scaeurgus unicirrhus")
# Some entries labeled as cephalopoda, which is the scientific grouping for all of the above
# Both squid and cuttlefish mentioned in SSRT reports, so combining both, but removing ocotpus
# For now, keeping bobtail squid as part of cuttlefish group because they are often sold as "cuttlefish" due to their similar cooking properties and shape

# For tuna comparisons, for now, only look at tropical tuna species (bigeye, skipjack, and yellowfin), which are the focus of the SSRT profiles
tuna_species <- c("Allothunnus fallai", "Auxis rochei", "Auxis thazard", "Auxis thazard, A. rochei", "Euthynnus affinis", "Euthynnus alletteratus", "Euthynnus lineatus", "Katsuwonus pelamis", "Thunnini", "Thunnus alalunga", "Thunnus atlanticus", "Thunnus albacares", "Thunnus maccoyii", "Thunnus obesus", "Thunnus orientalis", "Thunnus spp", "Thunnus thynnus", "Thunnus tonggol")
# From Gemini AI: from a commodity (commercial trade and fishing) perspective, "tunas" includes not only the true tunas (Thunnus genus) but also the highly abundant Skipjack tuna (Katsuwonus pelamis) and the group of related species in the tribe Thunnini.
# The remaining species found in the much broader ISSCAAP group "Tunas, bonitos, billfishes" are: bonitos, mackerel, wahoo, bonitos, other mackerel-like species; all not typically traded as "tuna"

# For shrimp, can continue to use ISSCAAP group "Shrimps, prawns"

df <- df %>%
  mutate(SSRT.group = case_when(
    # Match and label based on the different species list
    Scientific.name %in% squid_species ~ "Squid and cuttlefish",
    Scientific.name %in% cuttlefish_species ~ "Squid and cuttlefish",
    Scientific.name %in% octopus_species ~ "Octopus",
    # Match against the single word "Cephalopoda"
     Scientific.name == "Cephalopoda" ~ "Cephalopod",
    Scientific.name %in% tuna_species ~ "Tuna",
    ISSCAAP.group == "Shrimps, prawns" ~ "Shrimp and prawn",
    # Default value for anything that doesn't match
    TRUE ~ NA_character_
  )
  )

df %>%
  group_by(SSRT.group, SSRT.YN) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')


# Define the three groups of interest.
groups_of_interest <- c("Shrimp and prawn", "Tuna", "Squid and cuttlefish")

# Calculate the total overall volume to use for percentage calculation.
total_overall_volume <- sum(df$Volume..mt., na.rm = TRUE)

# Summarize the volume for the three groups, separating by SSRT.YN status.
summary_data_y_n <- df %>%
  filter(SSRT.group %in% groups_of_interest) %>%
  group_by(SSRT.group, SSRT.YN) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')

# Create a combined label for the chart legend.
summary_data_y_n$label <- paste(summary_data_y_n$SSRT.group, " (SSRT:", summary_data_y_n$SSRT.YN, ")")

# Summarize the volume for all other groups.
other_groups_volume <- df %>%
  filter(!SSRT.group %in% groups_of_interest) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE)) %>%
  mutate(
    SSRT.group = "Other",
    SSRT.YN = "Other",
    label = "All Other Groups"
  )

# Combine the two datasets.
combined_data <- bind_rows(summary_data_y_n, other_groups_volume) %>%
  mutate(percentage = (volume / total_overall_volume) * 100)

# Manually set the order of the categories in the legend.
combined_data$label <- factor(combined_data$label, levels = c(unique(summary_data_y_n$label), "All Other Groups"))

# Define colors: "Paired" palette for the main groups and a separate color for "Other".
n_paired_colors <- length(unique(summary_data_y_n$label))
paired_colors <- brewer.pal(max(3, n_paired_colors), "Paired")[1:n_paired_colors]
manual_colors <- c(paired_colors, "grey50")

# Create the first donut chart for all groups.
p1 <- ggplot(combined_data, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "Seafood Social Risk Tool Coverage",
    subtitle = "By Percentage of Total Global Production Volume",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = manual_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

# Save the first plot as a PNG file.
ggsave("all_groups_donut_chart.png", plot = p1, width = 8, height = 8)

# --- Create a donut chart for each individual group of interest ---
for (group in groups_of_interest) {
  # Filter the data for the current group.
  individual_group_data <- df %>%
    filter(SSRT.group == group) %>%
    group_by(SSRT.YN) %>%
    summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')
  
  # Calculate the total volume for the current group.
  total_group_volume <- sum(individual_group_data$volume, na.rm = TRUE)
  
  # Check to prevent division by zero.
  if (total_group_volume == 0) {
    message(sprintf("No volume found for the group: %s. Skipping chart.", group))
    next
  }
  
  # Calculate the percentage of Y vs N within the group.
  individual_group_data <- individual_group_data %>%
    mutate(percentage = (volume / total_group_volume) * 100)
  
  # Create the donut chart for the individual group.
  p_group <- ggplot(individual_group_data, aes(x = 2, y = percentage, fill = SSRT.YN)) +
    geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
    coord_polar(theta = "y") +
    xlim(c(0.5, 2.5)) +
    labs(
      title = paste("Seafood Social Risk Tool Coverage for", group),
      subtitle = "By Percentage of Global Production Volume",
      fill = "SSRT.YN"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # Add text labels for percentages.
    geom_text(aes(label = sprintf("%.1f%%", percentage)),
              position = position_stack(vjust = 0.5, reverse = TRUE)) +
    # Use the "Paired" color palette
    scale_fill_brewer(palette = "Paired")
  
  # Save the individual group plot as a PNG file.
  # We use paste0 to create a dynamic filename based on the group name.
  ggsave(paste0(gsub(" ", "_", group), "_donut_chart.png"), plot = p_group, width = 8, height = 8)
}


###############################################################################################################################################
## Make donut charts of SSRT coverage of US SEAFOOD MARKET
### One route is to download raw consumption data from here: https://knb.ecoinformatics.org/view/doi:10.5063/F1CZ35N7
### Another route is to use the already processed data file used in the SFW dashboard (e.g., US_Market_Data_2025-09-10.xlsx)

# Code for raw data
# df <- read.csv("Data/ARTIS Consumption Data/consumption_midpoint_HS17_2020.csv", header = TRUE) %>%
#   filter(consumer_iso3c == "USA")
# 
# df %>% summarize(sum(consumption_live_t, na.rm = TRUE))
# # 7,117,417
# 
# Create a list of all farmed species within an SSRT country (e.g., India) and pass this list to AI to identify all the shrimp species
# df %>% 
#   filter(source_country_iso3c == "IND" & method == "aquaculture") %>%
#   select(sciname, sciname_hs_modified) %>%
#   filter(sciname == sciname_hs_modified) %>%
#   distinct(sciname)

#Code for processed data
raw_df <- read.csv("Data/US_Market_Data_2025-09-10.csv", header = TRUE) 

# Create lists of 1 - all countries with shrimp SSRT profiles and 2 - all shrimp species farmed in those countries
shrimp_countries_list <- c("BGD", "IND", "CHN", "IDN", "ECU", "MEX")
shrimp_species_list <- raw_df %>%
  filter(Country.code %in% shrimp_countries_list) %>%
  filter(Commodity == "Shrimp & prawn") %>%
  select(Commodity, Scientific.name) %>%
  distinct() %>%
  pull(Scientific.name)

# Create lists of 1 - all countries with tuna SSRT profiles
# 2 - List of the tropical tuna species (bigeye, skipjack, and yellowfin) covered by SSRT profiles
# 3 - List of all other tuna species
# List of the tropical tuna species (bigeye, skipjack, and yellowfin) covered by SSRT profiles
tuna_countries_list <- c("COL", "ECU", "FRA", "GHA", "IDN", "ITA", "MDV", "MUS", "MEX", "PAN",
                         "PHL", "PRT", "SEN", "KOR", "ESP", "LKA", "TWN", "THA", "VEN", "VNM")
SSRT_tuna_species_list <- c("Katsuwonus pelamis", "Thunnus albacares", "Thunnus obesus")
target_commodities <- c("Tuna (albacore or longfin)", "Tuna (Skipjack or stripe-belly bonito)", "Tuna nes")
all_tuna_species <- raw_df %>%
  filter(Commodity %in% target_commodities) %>%
  pull(Scientific.name) %>%
  unique() %>%
  sort()

# Create lists of 1 - all countries with squid SSRT profiles and 2 - all shrimp species farmed in those countries
squid_countries_list <- c("CHN", "KOR")
squid_species_list <- raw_df %>%
  filter(Country.code %in% squid_countries_list) %>%
  filter(Commodity == "Cuttle fish, squid") %>%
  select(Commodity, Scientific.name) %>%
  distinct() %>%
  pull(Scientific.name)

# Data cleaning
df <- raw_df %>%
  mutate(
    # Check 1: Is the species one of the target tuna species?
    # Check 2: Is the country one of the target country codes?
    is_shrimp_match = (Scientific.name %in% shrimp_species_list & Country.code %in% shrimp_countries_list), 
    is_tuna_match = (Scientific.name %in% SSRT_tuna_species_list & Country.code %in% tuna_countries_list),
    is_squid_match = (Scientific.name %in% squid_species_list & Country.code %in% squid_countries_list),
    SSRT.YN = if_else(
      is_shrimp_match | is_tuna_match | is_squid_match, # TRUE if EITHER the shrimp OR the tuna condition is met
      "Y", 
      "N"
    )
  ) %>%
  # Remove the intermediate columns used for clarity
  select(-is_shrimp_match, -is_tuna_match) %>%
  # Create a copy of the Commodity column called SSRT Commodity that identifies tropical tunas
  mutate(SSRT.Commodity = case_when(
    # Match and label based on the different species list
    Scientific.name %in% all_tuna_species ~ "Tuna",
    Commodity == "Cuttle fish, squid" ~ "Squid and cuttlefish", # changing name to match labels from Global Production charts above
    Commodity == "Shrimp & prawn" ~ "Shrimp and prawn", # changing name to match labels from Global Production charts above
    # Default value for anything that doesn't match
    TRUE ~ NA_character_))




# Data checks:
# View all SSRT.YN == Y
# df %>% filter(SSRT.YN == "Y") %>% select(Scientific.name, Country.code) %>% distinct()
# View all tropical tuna, SSRT.YN == Y versus N
# df %>% filter(SSRT.Commodity == "Tropical tuna" & SSRT.YN == "Y") %>% select(Scientific.name, Country.code, SSRT.YN) %>% distinct() %>% arrange(Country.code)
# df %>% filter(SSRT.Commodity == "Tropical tuna" & SSRT.YN == "N") %>% select(Scientific.name, Country.code, SSRT.YN) %>% distinct() %>% arrange(Country.code)


# Calculate the total overall volume to use for percentage calculation.
total_overall_volume <- sum(df$Assessment.Volume, na.rm = TRUE)

# Summarize the volume for the three groups, separating by SSRT.YN status.
groups_of_interest <- c("Shrimp and prawn", "Squid and cuttlefish", "Tuna")

summary_data_y_n <- df %>%
  filter(SSRT.Commodity %in% groups_of_interest) %>%
  group_by(SSRT.Commodity, SSRT.YN) %>%
  summarise(volume = sum(Assessment.Volume, na.rm = TRUE), .groups = 'drop')

#### FIX IT - create a second version of summary_data_y_n that compares all tuna species with vs without an SSRT (not just tropical tuna species)



# Create a combined label for the chart legend.
summary_data_y_n$label <- paste(summary_data_y_n$SSRT.Commodity, " (SSRT:", summary_data_y_n$SSRT.YN, ")")

# Summarize the volume for all other groups.
other_groups_volume <- df %>%
  filter(!SSRT.Commodity %in% groups_of_interest) %>%
  summarise(volume = sum(Assessment.Volume, na.rm = TRUE)) %>%
  mutate(
    SSRT.Commodity = "Other",
    SSRT.YN = "Other",
    label = "All Other Groups"
  )

# Combine the two datasets.
combined_data <- bind_rows(summary_data_y_n, other_groups_volume) %>%
  mutate(percentage = (volume / total_overall_volume) * 100)

# Manually set the order of the categories in the legend.
combined_data$label <- factor(combined_data$label, levels = c(unique(summary_data_y_n$label), "All Other Groups"))

# Define colors: "Paired" palette for the main groups and a separate color for "Other".
n_paired_colors <- length(unique(summary_data_y_n$label))
paired_colors <- brewer.pal(max(3, n_paired_colors), "Paired")[1:n_paired_colors]
manual_colors <- c(paired_colors, "grey50")

# Create the first donut chart for all groups.
p1 <- ggplot(combined_data, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "Volume of Selected Seafood Commodities with SSRT profiles",
    subtitle = "Percentage of Total Volume in the US Market",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = manual_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

# Save the first plot as a PNG file.
ggsave("all_groups_donut_chart-USMarket.png", plot = p1, width = 8, height = 8)

# --- Create a donut chart for each individual group of interest ---
for (group in groups_of_interest) {
  # Filter the data for the current group.
  individual_group_data <- df %>%
    filter(SSRT.Commodity == group) %>%
    group_by(SSRT.YN) %>%
    summarise(volume = sum(Assessment.Volume, na.rm = TRUE), .groups = 'drop')
  
  # Calculate the total volume for the current group.
  total_group_volume <- sum(individual_group_data$volume, na.rm = TRUE)
  
  # Check to prevent division by zero.
  if (total_group_volume == 0) {
    message(sprintf("No volume found for the group: %s. Skipping chart.", group))
    next
  }
  
  # Calculate the percentage of Y vs N within the group.
  individual_group_data <- individual_group_data %>%
    mutate(percentage = (volume / total_group_volume) * 100)
  
  # Create the donut chart for the individual group.
  p_group <- ggplot(individual_group_data, aes(x = 2, y = percentage, fill = SSRT.YN)) +
    geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
    coord_polar(theta = "y") +
    xlim(c(0.5, 2.5)) +
    labs(
      title = paste("SSRT Coverage for", group),
      subtitle = "Percentage of Total Volume in the US Market",
      fill = "SSRT.YN"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # Add text labels for percentages.
    geom_text(aes(label = sprintf("%.1f%%", percentage)),
              position = position_stack(vjust = 0.5, reverse = TRUE)) +
    # Use the "Paired" color palette
    scale_fill_brewer(palette = "Paired")
  
  # Save the individual group plot as a PNG file.
  # We use paste0 to create a dynamic filename based on the group name.
  ggsave(paste0(gsub(" ", "_", group), "_donut_chart-USMarket.png"), plot = p_group, width = 8, height = 8)
}




### END NOTES:
# For tuna, was going to create a chart section for "Tuna NES" with footnote to show that some of this volume may be covered by SSRT but that we can't know for sure since species name isn't known
# Turns out however that only Thunnini could potentially fall under this category and volume for this was only 14.55
# For squid, both "squid" and "cuttlefish" are mentioned in the China SSRT profile so not filtering out cuttlefish species