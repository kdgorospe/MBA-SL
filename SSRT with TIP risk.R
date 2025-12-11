# This script reads in the SFW Mapping Dictionary CSV file, calculates the total volume for specific groups,
# and then visualizes the results as a donut chart of SSRT coverage of GLOBAL PRODUCTION.
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(purrr)

## FIXIT - create script for adding SSRT.YN column to SFW Mapping Dictionary file and identifying Y's 
# Read the CSV file into a data frame.
df <- read.csv("Data/SSRT Coverage/2025 SFW Mapping Dictionary - SSRT coverage.csv", header = TRUE) %>%
  mutate(SSRT.YN = if_else(is.na(SSRT.YN) | SSRT.YN == "", "N", SSRT.YN)) %>%
  # Clean country codes that are part of "EU" stats
  mutate(
    Country.code = case_when(
      Country.code == "EUESP" ~ "ESP",
      Country.code == "EUFRA" ~ "FRA",
      Country.code == "EUITA" ~ "ITA",
      Country.code == "EUMYT" ~ "MYT",
      Country.code == "EUPRT" ~ "PRT",
      Country.code == "EUREU" ~ "REU",
      # For all other values, keep the original value
      TRUE ~ Country.code
    )
  )

# To output list of countries
# country_risk_template <- df %>%
#   select(Country, Country.code) %>%
#   distinct() %>%
#   arrange(Country.code) %>%
#   slice(-(1:2)) 
# remove first two rows - blank row and ICCAT row
# write.csv(country_risk_template, "country_risk_template.csv", row.names = FALSE)
# Tried to output a table of countries, and use AI to fill in risk, but Gemini said it was too big of a task??

tip <- read.csv("Data/SSRT Coverage/TIP Tiers - 2024 report.csv", header = TRUE) 

# Join TIP risk with SFW Mapping Dictionary 
# Use a left_join to ensure all countries from the original 'df' are kept
df_risk <- df %>%
  left_join(tip, by = c("Country.code" = "Country_Code")) %>%
  mutate(Tier_Placement = replace_na(Tier_Placement, "No TIP Ranking")) %>%
  mutate(Tier_Placement = if_else(
    Tier_Placement == "Special Case", 
    "No TIP Ranking", 
    Tier_Placement)) # Keep the original value if the condition is FALSE

# Check that anything with missing data (wgi.rol) should have "No Risk Score" under Tier_Placement
df_risk %>%
  filter(is.na(Tier_Placement)) %>%
  select(Country, Country.code, Tier_Placement) %>%
  distinct()

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
df_risk <- df_risk %>%
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

df_risk %>%
  group_by(SSRT.group, SSRT.YN) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop')




########################################
# PLOTS
######################################## 
# Plot only risk scores (no SSRT coverage)

risk_dat <- df_risk %>%
  group_by(Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(percentage = (volume / sum(volume)) * 100)

# Define colors: "Paired" palette for the main groups and a separate color for "Other".
risk_colors <- brewer.pal(n=5, name = "Reds")

# Create donut chart based on risk groups
risk_donut <- ggplot(risk_dat, aes(x = 2, y = percentage, fill = Tier_Placement)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "Seafood Risk",
    subtitle = "Based on 2024 TIP Report",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = risk_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

# Save the first plot as a PNG file.
ggsave("TIP-global_seafood_risk_levels.png", plot = risk_donut, width = 8, height = 8)

##############################################################################
# Plot total seafood risk production scores with SSRT coverage
global_dat <- df_risk %>%
  group_by(Tier_Placement, SSRT.YN) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(percentage = (volume / sum(volume) * 100))

global_dat$label <- paste(global_dat$Tier_Placement, " (SSRT:", global_dat$SSRT.YN, ")")

# Manually set the order of the categories in the legend.
global_dat$label <- factor(global_dat$label, levels = c(unique(global_dat$label)))

# Define colors: "Paired" palette for the main groups and a separate color for "Other".
paired_colors <- brewer.pal(10, "Paired") # one for each pairing (Very Low Risk N and Very Low Risk Y, etc)
global_colors <- c("grey50", paired_colors) #grey for No TIP Ranking

# Create donut chart based on risk groups
global_donut <- ggplot(global_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "Seafood Risk",
    subtitle = "Based on 2024 TIP Report",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = global_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

# Save the first plot as a PNG file.
ggsave("TIP-global_seafood_risk_levels_with_SSRT_coverage.png", plot = global_donut, width = 8, height = 8)

##############################################################################
# VERY HIGH RISK

# Then for each risk category, create a new donut showing SSRT coverage
# Define the three groups of interest.
groups_of_interest <- c("Shrimp and prawn", "Tuna", "Squid and cuttlefish")

very_hi_dat <- df_risk %>%
  filter(Tier_Placement == "Tier 3") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

very_hi_dat$label <- paste(very_hi_dat$SSRT.group, " (SSRT:", very_hi_dat$SSRT.YN, ")")

# FIX IT - here and below, is there a way to set the order dynamically (not manually each time?)
# Manually set the order of the categories in the legend.
very_hi_dat$label <- factor(very_hi_dat$label, 
                            levels = c("Shrimp and prawn  (SSRT: N )", 
                                       "Shrimp and prawn  (SSRT: Y )",
                                       "Squid and cuttlefish  (SSRT: N )",
                                       "Squid and cuttlefish  (SSRT: Y )",
                                       "Tuna  (SSRT: N )",
                                       "Tuna  (SSRT: Y )",
                                       "Other  (SSRT: N )"))

# FIX IT - here and below, is there a way to set the colors just once, and not manually for each plot?
# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(2,4)] 
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[c(2,4)] 
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
very_hi_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

very_hi_donut <- ggplot(very_hi_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Very High Risk Seafood Production",
    subtitle = "Based on 2024 TIP Report (Tier 3 Countries)",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = very_hi_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_tier_3_seafood.png", plot = very_hi_donut, width = 8, height = 8)

##############################################################################
# HIGH RISK

hi_dat <- df_risk %>%
  filter(Tier_Placement == "Tier 2 Watch List") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

hi_dat$label <- paste(hi_dat$SSRT.group, " (SSRT:", hi_dat$SSRT.YN, ")")
# Manually set the order of the categories in the legend.
hi_dat$label <- factor(hi_dat$label, 
                       levels = c("Shrimp and prawn  (SSRT: N )", 
                                  "Squid and cuttlefish  (SSRT: N )",
                                  "Tuna  (SSRT: N )",
                                  "Tuna  (SSRT: Y )",
                                  "Other  (SSRT: N )"))

# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[3]
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[3]
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
hi_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

hi_donut <- ggplot(hi_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of High Risk Seafood Production",
    subtitle = "Based on 2024 TIP Report (Tier 2 Watch List Countries)",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = hi_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_tier_2_watchlist_seafood.png", plot = hi_donut, width = 8, height = 8)


##############################################################################
# MEDIUM RISK

med_dat <- df_risk %>%
  filter(Tier_Placement == "Tier 2") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

med_dat$label <- paste(med_dat$SSRT.group, " (SSRT:", med_dat$SSRT.YN, ")")
# Manually set the order of the categories in the legend.
med_dat$label <- factor(med_dat$label, 
                       levels = c("Shrimp and prawn  (SSRT: N )", 
                                  "Shrimp and prawn  (SSRT: Y )",
                                  "Squid and cuttlefish  (SSRT: N )",
                                  "Tuna  (SSRT: N )",
                                  "Tuna  (SSRT: Y )",
                                  "Other  (SSRT: N )"))

# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(2,4)] 
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[3] 
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
med_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

med_donut <- ggplot(med_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Medium Risk Seafood Production",
    subtitle = "Based on 2024 TIP Report (Tier 2 Countries)",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = med_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_tier_2_seafood.png", plot = med_donut, width = 8, height = 8)

##############################################################################
# LOW RISK

low_dat <- df_risk %>%
  filter(Tier_Placement == "Tier 1") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

low_dat$label <- paste(low_dat$SSRT.group, " (SSRT:", low_dat$SSRT.YN, ")")

# Manually set the order of the categories in the legend.
low_dat$label <- factor(low_dat$label, 
                        levels = c("Shrimp and prawn  (SSRT: N )", 
                                   "Squid and cuttlefish  (SSRT: N )",
                                   "Tuna  (SSRT: N )",
                                   "Tuna  (SSRT: Y )",
                                   "Other  (SSRT: N )"))

# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[2] 
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[2] 
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
low_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

low_donut <- ggplot(low_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Low Risk Seafood Production",
    subtitle = "Based on 2024 TIP Report",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = low_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_tier_1_seafood.png", plot = low_donut, width = 8, height = 8)


##############################################################################
# VERY LOW RISK

very_low_dat <- df_risk %>%
  filter(Tier_Placement == "Very Low Risk") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

very_low_dat$label <- paste(very_low_dat$SSRT.group, " (SSRT:", very_low_dat$SSRT.YN, ")")

# Manually set the order of the categories in the legend.
very_low_dat$label <- factor(very_low_dat$label, 
                        levels = c("Shrimp and prawn  (SSRT: N )", 
                                   "Squid and cuttlefish  (SSRT: N )",
                                   "Tuna  (SSRT: N )",
                                   "Tuna  (SSRT: Y )",
                                   "Other  (SSRT: N )"))

# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[2] 
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[2] 
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
very_low_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

very_low_donut <- ggplot(very_low_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Very Low Risk Seafood Production",
    subtitle = "Based on 2024 TIP Report",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = very_low_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_very_low_risk_seafood.png", plot = very_low_donut, width = 8, height = 8)

##############################################################################
# NO RISK SCORES

no_scores_dat <- df_risk %>%
  filter(Tier_Placement == "No Risk Score") %>%
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Check if the value in 'SSRT.group' is NOT in 'groups_of_interest'
    SSRT.group = if_else(
      !SSRT.group %in% groups_of_interest,
      # If the value is NOT in the list, replace it with NA
      NA_character_,
      # If the value IS in the list, keep the original value
      SSRT.group
    )
  ) %>%
  # Summarise again with the new labels
  group_by(SSRT.group, SSRT.YN, Tier_Placement) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

no_scores_dat$label <- paste(no_scores_dat$SSRT.group, " (SSRT:", no_scores_dat$SSRT.YN, ")")

# Manually set the order of the categories in the legend.
no_scores_dat$label <- factor(no_scores_dat$label, 
                             levels = c("Shrimp and prawn  (SSRT: N )", 
                                        "Squid and cuttlefish  (SSRT: N )",
                                        "Tuna  (SSRT: N )",
                                        "Other  (SSRT: N )"))

# Shrimp colors
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[2] 
# Squid colors
squid_colors <- brewer.pal(n = 5, name = "Blues")[2] 
# Tuna colors
tuna_colors <- brewer.pal(n = 5, name = "Greens")[2] 
# Other colors
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
no_scores_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

no_scores_donut <- ggplot(no_scores_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Seafood Production with No Risk Score",
    subtitle = "Based on 2024 TIP Report",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # Use the manual color palette.
  scale_fill_manual(values = no_scores_colors) +
  # Add text labels for percentages inside the chart.
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggsave("SSRT_coverage_of_missing_risk_score_seafood.png", plot = no_scores_donut, width = 8, height = 8)

