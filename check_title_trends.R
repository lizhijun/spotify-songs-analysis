# Check title length trends
library(ggplot2)
library(dplyr)
library(readr)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Extract year and calculate track name length
spotify_data <- spotify_data %>%
  mutate(
    release_year = as.numeric(substr(track_album_release_date, 1, 4)),
    track_name_length = nchar(track_name)
  )

# Filter for reasonable years
valid_years <- spotify_data %>%
  filter(release_year >= 1950 & release_year <= 2023)

# Calculate average track name length by year
track_name_by_year <- valid_years %>%
  group_by(release_year) %>%
  summarise(
    avg_name_length = mean(track_name_length),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 10)

# Display the data
cat("Track name length trends:\n")
print(track_name_by_year)

# Check overall trend
cat("\nOverall trend analysis:\n")
early_years <- track_name_by_year$avg_name_length[track_name_by_year$release_year <= 1970]
late_years <- track_name_by_year$avg_name_length[track_name_by_year$release_year >= 2010]

cat("Earliest years (1950s-1970) average:", mean(early_years, na.rm = TRUE), "\n")
cat("Latest years (2010s-2020s) average:", mean(late_years, na.rm = TRUE), "\n")

# Simple correlation test
correlation <- cor(track_name_by_year$release_year, track_name_by_year$avg_name_length, use = "complete.obs")
cat("Correlation between year and title length:", correlation, "\n")

if(!is.na(correlation)) {
  if(correlation > 0) {
    cat("Trend: INCREASING over time\n")
  } else {
    cat("Trend: DECREASING over time\n")
  }
} else {
  cat("Cannot determine trend due to missing data\n")
}

# Show year range in data
cat("\nYear range in data:", min(track_name_by_year$release_year), "to", max(track_name_by_year$release_year), "\n")