# Create data visualizations of SSRT website data
rm(list=ls())

# --- 1. Load Libraries ---
# Tidyverse is a collection of packages that share common data principles.
# It includes dplyr (data manipulation), readr (file reading),
# stringr (string manipulation), purrr (functional programming), and ggplot2 (plotting).
library(tidyverse)
library(lubridate) # For easy date parsing (ymd)

# --- 2. Choose between Downloads vs Visits

# For Downloads data:
# data_dir <- "/Users/kgorospe/Code/MBA-SL/Data/SSRT Website/SFW downloads"
# output_dir <- "/Users/kgorospe/Code/MBA-SL/Outputs"
# output_file_path <- file.path(output_dir, "ssrt_individual_downloads_time_series.png")
# plot_title <- "SSRT Downloads Over Time"
# y_axis <- "Number of Downloads"
# file_col <- "File name"
# metric_col <- "Downloads"


# For Visits data:
data_dir <- "/Users/kgorospe/Code/MBA-SL/Data/SSRT Website/SFW visits"
output_dir <- "/Users/kgorospe/Code/MBA-SL/Outputs"
output_file_path <- file.path(output_dir, "ssrt_site_visits_time_series.png")
plot_title <- "SSRT Visits Over Time"
y_axis <- "Number of Visits"
file_col <- "Page path"
metric_col <- "Page views"

# --- 3. Helper Function to Process a Single File ---

#' Reads a CSV, filters for 'ssrt' files, extracts the reporting period's start date,
#' and retains individual file download counts for time series analysis.
#'
#' @param file_path The full path to the CSV file.
#' @return A tibble with three columns: start_date (Date), Metric_Value (numeric), and File_Identifier (character).
process_download_file <- function(file_path, file_col, metric_col) {
  # Get the filename (e.g., "SFW downloads - 20230101 to 20230331.csv")
  file_name <- basename(file_path)
  
  # 1. Extract the starting date (YYYYMMDD) from the filename.
  # We use a regex to capture the first sequence of 8 digits.
  date_string <- stringr::str_extract(file_name, "\\d{8}")
  
  # 2. Read the CSV file. read_csv is fast and tidy-friendly.
  # We suppress column type messages for clean output.
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # 3. Use the pipe (%>%) for a fluent data manipulation sequence.
  result <- data %>%
    # Filter for rows where the 'File name' column contains the string "ssrt"
    dplyr::filter(stringr::str_detect(!!sym(file_col), "ssrt")) %>%
    # Add the extracted date, converting the 8-digit string to a proper Date object
    dplyr::mutate(
      start_date = lubridate::ymd(date_string)
    ) %>%
    # Select the required columns: date, download count, and the file identifier
    dplyr::select(start_date, 
                  Metric_Value = !!sym(metric_col),
                  File_Identifier = !!sym(file_col))
  
  return(result)
}

# --- 4. Main Execution: Read All Files and Combine ---

# Get a list of all CSV files in the directory.
# fs::dir_ls is the tidyverse-idiomatic way to list files.
file_list <- fs::dir_ls(data_dir, regexp = "\\.csv$")

# Use purrr::map_df to apply the process_download_file function to every
# file in the list and combine the resulting data frames (tibbles) by row.
message("Processing ", length(file_list), " files and compiling individual file download data...")
ssrt_downloads_time_series <- purrr::map_df(file_list, 
                                            process_download_file, 
                                            file_col = file_col,
                                            metric_col = metric_col
                                            )

# Ensure the data is ordered by date before plotting
ssrt_downloads_time_series <- ssrt_downloads_time_series %>%
  dplyr::arrange(start_date)

# Function to find the longest common prefix (LCP) of a character vector
find_longest_common_prefix <- function(strings) {
  if (length(strings) <= 1) return("") # No common prefix if 0 or 1 item
  
  # Pick the first string to compare against
  first <- strings[1]
  prefix <- ""
  
  for (i in 1:nchar(first)) {
    char <- substr(first, i, i)
    temp_prefix <- paste0(prefix, char)
    
    # Check if this temporary prefix is at the start of ALL strings
    # stringr::fixed() is used to ensure fixed string matching, not regex.
    is_common <- all(stringr::str_starts(strings, stringr::fixed(temp_prefix)))
    
    if (is_common) {
      prefix <- temp_prefix
    } else {
      break
    }
  }
  return(prefix)
}

# NEW STEP: Simplify File Names for Legend
# 1. Get the unique file paths
unique_files <- unique(ssrt_downloads_time_series$File_Identifier)

# 2. Find the LCP
common_prefix <- find_longest_common_prefix(unique_files)
message("Common prefix found and will be removed from legend: '", common_prefix, "'")

# 3. Trim the data and create a new column with the shortened name
ssrt_downloads_time_series <- ssrt_downloads_time_series %>%
  dplyr::mutate(
    # Use stringr::str_remove to take off the common prefix
    short_file_name = stringr::str_remove(File_Identifier, stringr::fixed(common_prefix))
  )

# --- 5. Plotting the Time Series ---

# Create a time series plot using ggplot2, grouping and coloring by 'short_file_name'
download_plot <- ssrt_downloads_time_series %>%
  ggplot2::ggplot(ggplot2::aes(
    x = start_date, 
    y = Metric_Value, # Use the individual Downloads column
    group = short_file_name, # Group by the short name
    color = short_file_name # Color by the short name
  )) +
  # Add a line layer to show the trend over time
  ggplot2::geom_line(linewidth = 1.2) +
  # Add points to clearly mark the data for each period
  ggplot2::geom_point(size = 3, alpha = 0.8) +
  # Set descriptive labels and titles
  ggplot2::labs(
    title = plot_title,
    x = "Date",
    y = y_axis,
    color = "SSRT File" # Update legend title (already short now)
  ) +
  # Customize the date axis for readability
  ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  # Apply a clean, minimal theme
  ggplot2::theme_minimal() +
  # Force the legend to display in two columns for better space utilization
  ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    plot.subtitle = ggplot2::element_text(size = 12, color = "gray50"),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom", # Move legend for better visualization of many lines
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

# Save the plot to the specified path as a high-quality PNG
ggplot2::ggsave(
  filename = output_file_path,
  plot = download_plot,
  width = 12, # Saved width is increased slightly to better accommodate the two-column legend
  height = 8,
  units = "in"
)

# Print the final compiled data (optional, for verification)
print(ssrt_downloads_time_series)

