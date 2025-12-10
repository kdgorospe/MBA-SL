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
country_risk_template <- df %>%
  select(Country, Country.code) %>%
  distinct() %>%
  arrange(Country.code) %>%
  slice(-(1:2)) # remove blank row and ICCAT row
# write.csv(country_risk_template, "country_risk_template.csv", row.names = FALSE)
# Tried to output a table of countries, and use AI to fill in risk, but Gemini said it was too big of a task??

wgi <- read.csv("Data/SSRT Coverage/WGI data/wgidataset.csv", header = TRUE) 

wgi_clean <- wgi %>%
  filter(indicator == "rl") %>% # i.e., "Rule of Law" subindex
  filter(year == 2023) %>%
  select(code, estimate) %>%
  rename(Country.code = code) %>%
  rename(wgi.rol = estimate) %>% # WGI rule of law
  # as.numeric() converts to number, coercing any non-numeric text (like "N/A" or ".." ) to NA
  mutate(wgi.rol = as.numeric(wgi.rol)) %>%
  # Calculate quintiles and assign to bins
  mutate(
    wgi.risk = case_when(
      # High ROL scores (high governance quality) get lower risk labels
      wgi.rol <= quantile(wgi.rol, 0.2, na.rm = TRUE) ~ "Very High Risk", # Bottom 20%
      wgi.rol <= quantile(wgi.rol, 0.4, na.rm = TRUE) ~ "High Risk",
      wgi.rol <= quantile(wgi.rol, 0.6, na.rm = TRUE) ~ "Medium Risk",
      wgi.rol <= quantile(wgi.rol, 0.8, na.rm = TRUE) ~ "Low Risk",
      # Top 20% of scores (highest ROL)
      TRUE ~ "Very Low Risk" 
    )
  ) 

# Join WGI risk with SFW Mapping Dictionary 
# Use a left_join to ensure all countries from the original 'df' are kept
df_risk <- df %>%
  left_join(wgi_clean, by = "Country.code") %>%
  mutate(wgi.risk = replace_na(wgi.risk, "No Risk Score")) %>%
  # Convert the new column to a factor for proper sorting and visualization
  # The levels ensure the categories are ordered from Low Risk to High Risk
  mutate(
    wgi.risk = factor(
      wgi.risk,
      levels = c("No Risk Score", "Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Very High Risk")
    )
  )

# Check that anything with missing data (wgi.rol) should have "No Risk Score" under wgi.risk
df_risk %>%
  filter(is.na(wgi.rol)) %>%
  select(Country, Country.code, wgi.rol, wgi.risk) %>%
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

##### LEFT OFF HERE
# Calculate the total overall volume to use for percentage calculation.
total_overall_volume <- sum(df_risk$Volume..mt., na.rm = TRUE)

global_dat <- df_risk %>%
  group_by(wgi.risk) %>%
  summarise(volume = sum(Volume..mt., na.rm = TRUE), .groups = 'drop') %>%
  mutate(percentage = (volume / total_overall_volume) * 100)

risk_colors <- brewer.pal(n = 6, name = "Reds")

# Create the first donut chart based on risk groups
global_donut <- ggplot(global_dat, aes(x = 2, y = percentage, fill = wgi.risk)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "Seafood Risk",
    subtitle = "Based on World Governance Index Rule of Law Scores",
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
ggsave("global_seafood_risk_levels.png", plot = global_donut, width = 8, height = 8)

# Then for each risk category, create a new donut showing SSRT coverage
# Define the three groups of interest.
groups_of_interest <- c("Shrimp and prawn", "Tuna", "Squid and cuttlefish")

##############################################################################
# VERY HIGH RISK

very_hi_dat <- df_risk %>%
  filter(wgi.risk == "Very High Risk") %>%
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
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
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Replace all NA values in the 'SSRT.group' column with "Other"
    SSRT.group = replace_na(SSRT.group, "Other")
  ) %>%
  mutate(percentage = (volume / sum(volume) * 100))

very_hi_dat$label <- paste(very_hi_dat$SSRT.group, " (SSRT:", very_hi_dat$SSRT.YN, ")")
# Manually set the order of the categories in the legend.
very_hi_dat$label <- factor(very_hi_dat$label, 
                            levels = c("Shrimp and prawn  (SSRT: N )", 
                                       "Shrimp and prawn  (SSRT: Y )",
                                       "Squid and cuttlefish  (SSRT: N )",
                                       "Tuna  (SSRT: N )",
                                       "Tuna  (SSRT: Y )",
                                       "Other  (SSRT: N )"))

# Get 3 shades for Shrimp=
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(2,4)] 
# Get 3 shades for Squid=
squid_colors <- brewer.pal(n = 5, name = "Blues")[3]
# Get 3 shades for Tuna
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Define the two shades of gray for 'Other'
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
very_hi_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

very_hi_donut <- ggplot(very_hi_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Very High Risk Seafood Production",
    subtitle = "Based on World Governance Index Rule of Law",
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

ggsave("SSRT_coverage_of_very_high_risk_seafood.png", plot = very_hi_donut, width = 8, height = 8)

##############################################################################
# HIGH RISK

hi_dat <- df_risk %>%
  filter(wgi.risk == "High Risk") %>%
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
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
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
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
                                  "Shrimp and prawn  (SSRT: Y )",
                                  "Squid and cuttlefish  (SSRT: N )",
                                  "Tuna  (SSRT: N )",
                                  "Tuna  (SSRT: Y )",
                                  "Other  (SSRT: N )"))

# Get 3 shades for Shrimp=
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(2,4)] 
# Get 3 shades for Squid=
squid_colors <- brewer.pal(n = 5, name = "Blues")[3]
# Get 3 shades for Tuna
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Define the two shades of gray for 'Other'
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
hi_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

hi_donut <- ggplot(hi_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of High Risk Seafood Production",
    subtitle = "Based on World Governance Index Rule of Law",
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

ggsave("SSRT_coverage_of_high_risk_seafood.png", plot = hi_donut, width = 8, height = 8)


##############################################################################
# MEDIUM RISK

med_dat <- df_risk %>%
  filter(wgi.risk == "Medium Risk") %>%
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
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
  group_by(SSRT.group, SSRT.YN, wgi.risk) %>%
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
                                  "Squid and cuttlefish  (SSRT: Y )",
                                  "Tuna  (SSRT: N )",
                                  "Tuna  (SSRT: Y )",
                                  "Other  (SSRT: N )"))

# Get 3 shades for Shrimp=
shrimp_colors <- brewer.pal(n = 5, name = "Reds")[c(2,4)] 
# Get 3 shades for Squid=
squid_colors <- brewer.pal(n = 5, name = "Blues")[c(2,4)] 
# Get 3 shades for Tuna
tuna_colors <- brewer.pal(n = 5, name = "Greens")[c(2,4)] 
# Define the two shades of gray for 'Other'
other_colors <- c("grey50") 
# Combine the colors into a single vector, maintaining the order of the data
med_colors <- c(shrimp_colors, squid_colors, tuna_colors, other_colors)

med_donut <- ggplot(med_dat, aes(x = 2, y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  labs(
    title = "SSRT Coverage of Medium Risk Seafood Production",
    subtitle = "Based on World Governance Index Rule of Law",
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

ggsave("SSRT_coverage_of_medium_risk_seafood.png", plot = med_donut, width = 8, height = 8)
