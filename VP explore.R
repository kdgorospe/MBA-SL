# Explore VP data, plot # of farms assessed / verified green through time in Vietnam 

rm(list=ls())
library(tidyverse)
library(lubridate)

df_sites <- read.csv("Data/VP/DataExport_16-12-2025 - Kelvin test - sites - no filters.csv", header = TRUE)
df_details <- read.csv("Data/VP/DataExport_16-12-2025 - Kelvin test - assessment details - filter by created on date - 20240701 - 20250101.csv", header = TRUE)

# For "assessment header.CSV," Problem is that some of the values for the column "Address Line 1" sometimes contains a comma - e.g., "Trần Đề District,	Sóc Trăng" and is getting split into two columns
# df_header <- read.csv("Data/VP/DataExport_16-12-2025 - Kelvin test - assessment header - no filters.csv") 
# Error in read.table(file = file, header = header, sep = sep, quote = quote,  : duplicate 'row.names' are not allowed

# Read the entire file as a character vector
raw_text <- readLines("Data/VP/DataExport_16-12-2025 - Kelvin test - assessment header - no filters.csv")
# Replace ", " (comma-space) with " ; " or any other unique character
clean_text <- gsub(", ", "; ", raw_text)
# Read that cleaned text as a CSV
df_header <- read.csv(text = clean_text, sep = ",")


# Explore which columns are useful before filtering and selecting (need to verify this with Reuben, Cormac, et al.):
# Which columns have "Green" as a value
green_columns <- vnm_header %>%
  select(where(~ any(. == "Green", na.rm = TRUE))) %>%
  colnames()
summary_counts <- vnm_header %>%
  mutate(across(all_of(green_columns), as.factor)) %>%
  select(all_of(green_columns)) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "value") %>%
  count(column_name, value)
# Red/Green/Yellow: Assessment.Outcome or Snapshot.Assessment.Grouping.Outcome - unclear what the difference is
# For sample size: Group.Company.Sample.Size, Snapshot.Group.Company.Sample.Size, Group.SGS.Sample.Size, Snapshot.Group.SGS.Sample.Size - unclear what the difference is
# For total number in group: Group.Total.Sites 
# Project Name (Minh Phu: Pilot Groups, Demonstration, and others? should probably be filtered out)
# Group ID: Site.Reference,  Assessor.Type?

# Likely useless:
# Group.Name: all rows say "System.Collections.Generic.HashSet`1[SGS.Localization.Entities.DynamicTranslation]"
# Standard.Version: all 2.1 (shrimp standard)

vnm_header <- df_header %>%
  filter(Country == "Vietnam") %>%
  filter(Group.Phase != "Pilot") %>%
  filter(!Project.Name %in% c("Minh Phu: Pilot Groups", "Demonstration", "Testing")) %>%
  # Select important columns
  # select(CreatedOn, Assessment.End.Time, Project.Name, Site.Reference, Assessor.Type, Latitude, Longitude, Group.Total.Sites, Assessment.Outcome, Snapshot.Assessment.Grouping.Outcome) %>%
  select(CreatedOn, Assessment.End.Time, Project.Name, Site.Reference, Assessor.Type, Latitude, Longitude, Group.Total.Sites, Assessment.Outcome, Snapshot.Assessment.Grouping.Outcome, Snapshot.Assessment.Grouping.Status, Company.Group.Status, SGS.Group.Status) %>%
  filter(Snapshot.Assessment.Grouping.Status %in% c("Green", "Complete")) %>% # Unclear what is green vs complete / filtering here drops 1500+ rows
  filter(SGS.Group.Status == "Closed") %>%
  filter(Company.Group.Status == "Closed") %>%
  # Clean dates
  mutate(CreatedOn.Clean = mdy_hms(CreatedOn)) %>%
  mutate(Assessment.End.Time.Clean = mdy_hms(Assessment.End.Time))


# Try plotting Assessment Outcome x Group size through time?
### BENCHMARK: MBA Vietnam website says 4,174 farms verified green as of December 2025


# Unclear what Assessor.Type means: for now, split into two dataframes
# Note: two rows have same Site.Reference but they get split into separate dataframes based on Assessor.Type
# vnm_header %>%
#   group_by(Site.Reference) %>%
#   filter(n()>1) %>%
#   arrange(Site.Reference)

vnm_header_co <- vnm_header %>%
  filter(Assessor.Type == "Company")

vnm_header_sgs <- vnm_header %>%
  filter(Assessor.Type == "SGS")

plot_data <- vnm_header_co %>%
  # Filter for only the "Green" rows
  filter(Assessment.Outcome == "Green") %>%
  # Arrange by time to ensure the cumulative count flows correctly
  arrange(Assessment.End.Time.Clean) %>%
  # Create a cumulative count (row_number() works since each row = 1 event)
  mutate(cumulative_total_sites = cumsum(Group.Total.Sites)) 

# 2. Create the plot
ggplot(plot_data, aes(x = Assessment.End.Time.Clean, y = cumulative_total_sites)) +
  geom_step(color = "forestgreen", size = 1) + # geom_step shows the 'jumps' clearly
  # Add key project dates: Project Start (First Timestamp in data)
  geom_vline(xintercept = min(plot_data$Assessment.End.Time.Clean, na.rm = TRUE), 
             color = "red", linetype = "dotted") +
  annotate("text", x = min(plot_data$Assessment.End.Time.Clean, na.rm = TRUE), 
           y = max(plot_data$cumulative_total_sites) * 0.7, 
           label = "Project start with Minh Phu", color = "red", angle = 90, vjust = -0.5) +
  
  # 2. CASES Partnership (Jan 1, 2022)
  geom_vline(xintercept = as.POSIXct("2022-01-01"), color = "red", linetype = "dotted") +
  annotate("text", x = as.POSIXct("2022-01-01"), 
           y = max(plot_data$cumulative_total_sites) * 0.5, 
           label = "CASES partnership", color = "red", angle = 90, vjust = -0.5) +
  
  # 3. CAMIMEX Partnership (Jan 1, 2023)
  geom_vline(xintercept = as.POSIXct("2023-01-01"), color = "red", linetype = "dotted") +
  annotate("text", x = as.POSIXct("2023-01-01"), 
           y = max(plot_data$cumulative_total_sites) * 0.1, 
           label = "CAMIMEX partnership", color = "red", angle = 90, vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Cumulative Sum of Green-rated Sites Over Time",
    subtitle = paste("Total Green Sites:", sum(plot_data$Group.Total.Sites, na.rm = TRUE)),
    x = "Time of Assessment",
    y = "Total Count (Cumulative)"
  )

