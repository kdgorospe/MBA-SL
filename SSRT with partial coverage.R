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
df <- read.csv("Data/SSRT Coverage/2025 SFW Mapping Dictionary - SSRT coverage.csv", header = TRUE) %>%
  mutate(SSRT.YN = if_else(is.na(SSRT.YN) | SSRT.YN == "", "N", SSRT.YN)) %>%
  # Group the data by Country
  # FIX IT - need to clean up column "Country" or "Country.code" - e.g., Spain has both ESP and EUESP; TWN has both Taiwan (Province of China) and Taiwan Province of China (without parentheses)
  group_by(Country) %>%
  # Create new column SSRT.YNPartial
  mutate(
    # For a country group, if rows with both SSRT.YN == Y and N are found, then every row in SSRT.YNPartial for the country will be TRUE
    has_both_Y_and_N = any(SSRT.YN == "Y") & any(SSRT.YN == "N"), 
    # Apply the conditional logic for the new column
    SSRT.YNPartial = case_when(
      # If the country has both 'Y' and 'N', and the current row is 'N', label it "Partial"
      has_both_Y_and_N & SSRT.YN == "N" ~ "Partial",
      # For all other rows, copy the value from SSRT.YN
      TRUE ~ SSRT.YN
    )
  ) %>%
  # Remove the temporary grouping and helper column
  ungroup() %>%
  select(-has_both_Y_and_N)

# ISSCAAP groups are broader than SSRT risk profiles: Create column "SSRT.group" for each of the risk profile species groups and "Other"
# df %>% filter(ISSCAAP.group == "Squids, cuttlefishes, octopuses") %>% select(Scientific.name) %>% distinct()
# df %>% filter(ISSCAAP.group == "Tunas, bonitos, billfishes") %>% select(Scientific.name) %>% distinct()
# Use AI to classify scientific names as squids vs cuttlefishes vs octopuses and return as a character string list in quotation marks
squid_species <- c("Alloteuthis media", "Alloteuthis spp", "Alloteuthis subulata", "Berryteuthis magister", "Dosidicus gigas", "Doryteuthis gahi", "Doryteuthis opalescens", "Doryteuthis pealeii", "Doryteuthis pleii", "Histioteuthis spp", "Illex argentinus", "Illex coindetii", "Illex illecebrosus", "Illex spp", "Loliginidae", "Loliginidae, Ommastrephidae", "Loligo forbesii", "Loligo reynaudii", "Loligo spp", "Loligo vulgaris", "Lolliguncula diomedeae", "Martialia hyadesi", "Nototodarus sloanii", "Ommastrephes bartramii", "Ommastrephes spp", "Ommastrephidae", "Onykia spp", "Sepioteuthis lessoniana", "Sepioteuthis sepioidea", "Sthenoteuthis pteropus", "Todarodes filippovae", "Todarodes pacificus", "Todarodes sagittatus", "Todarodes spp", "Todaropsis eblanae", "Uroteuthis (Photololigo) duvaucelii")
cuttlefish_species <- c("Rossia macrosoma", "Sepia bertheloti", "Sepia elegans", "Sepia officinalis", "Sepia orbignyana", "Sepia pharaonis", "Sepia spp", "Sepiola rondeletii", "Sepiidae, Sepiolidae") # includes BOBTAIL SQUID family Sepiolidae
octopus_species <- c("Callistoctopus macropus", "Eledone cirrhosa", "Eledone moschata", "Eledone spp", "Enteroctopus magnificus", "Octopodidae", "Octopus maya", "Octopus salutii", "Octopus spp", "Octopus vulgaris", "Pinnoctopus cordiformis", "Scaeurgus unicirrhus")
# Some entries labeled as cephalopoda, which is the scientific grouping for all of the above
# Both squid and cuttlefish mentioned in SSRT reports, so combining both, but removing octopus
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

# df %>%
#   group_by(SSRT.group, SSRT.YN) %>%
#   summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')


# Define the three groups of interest.
groups_of_interest <- c("Shrimp and prawn", "Tuna", "Squid and cuttlefish")

# Calculate the total overall volume to use for percentage calculation.
total_overall_volume <- sum(df$Volume..mt., na.rm = TRUE)

# Summarize the volume for the three groups, separating by SSRT.YN status.
summary_data_y_n <- df %>%
  filter(SSRT.group %in% groups_of_interest) %>%
  group_by(SSRT.group, SSRT.YNPartial) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')

# Create a combined label for the chart legend.
summary_data_y_n$label <- paste(summary_data_y_n$SSRT.group, " (SSRT:", summary_data_y_n$SSRT.YNPartial, ")")

# Summarize the volume for all other groups.
other_groups_volume <- df %>%
  filter(!SSRT.group %in% groups_of_interest) %>%
  group_by(SSRT.YNPartial) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE)) %>%
  mutate(
    SSRT.group = "Other",
    label = "All Other Groups"
  )

# Combine the two datasets.
combined_data <- bind_rows(summary_data_y_n, other_groups_volume) %>%
  mutate(percentage = (volume / total_overall_volume) * 100)

# Manually set the order of the categories in the legend.
combined_data$label <- factor(combined_data$label, levels = c(unique(summary_data_y_n$label), "All Other Groups"))


##################################
# Define colors:
# Define the desired color palettes for each group
# Use sequential palettes (like 'Reds', 'Blues', 'Greens') to get a gradient.

# Get 3 shades for Shrimp (e.g., in a Red/Orange tone)
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(1, 3, 5)] 
# Get 3 shades for Squid (e.g., in a Blue/Teal tone)
squid_colors <- brewer.pal(n = 5, name = "Blues")[c(1, 3, 5)]
# Get 3 shades for Tuna (e.g., in a Purple/Magenta tone)
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(1, 3, 5)]
# Define the two shades of gray for 'Other'
other_colors <- c("grey50", "grey80") 
# Combine the colors into a single vector, maintaining the order of the data
# We must ensure the colors match the order of the 'volume label' or 'SSRT.YNPartial' column
manual_colors_vector <- c(
  # Shrimp and prawn (N, Partial, Y)
  shrimp_colors[1], shrimp_colors[2], shrimp_colors[3],
  # Squid and cuttlefish (N, Partial, Y)
  squid_colors[1], squid_colors[2], squid_colors[3],
  # Tuna (N, Partial, Y)
  tuna_colors[1], tuna_colors[2], tuna_colors[3],
  # Other (N/Partial are grouped as "All Other Groups" in your label)
  other_colors[1], other_colors[2]
)

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
  scale_fill_manual(values = manual_colors_vector) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

