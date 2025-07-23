# Track Popularity Visualization Analysis
# Enhanced visualization with modern styling

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(viridis)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Display basic data information
cat("Dataset Overview:\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Total columns:", ncol(spotify_data), "\n")
cat("Popularity range:", min(spotify_data$track_popularity), "-", max(spotify_data$track_popularity), "\n")
cat("Playlist genres:", paste(unique(spotify_data$playlist_genre), collapse = ", "), "\n\n")

# Custom theme for elegant visualization
custom_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.3),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "grey20", margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 12, color = "grey30", face = "bold"),
    axis.text = element_text(size = 10, color = "grey50"),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

# 1. Overall track popularity histogram with enhanced styling
cat("Generating overall track popularity histogram...\n")
overall_histogram <- ggplot(spotify_data, aes(x = track_popularity)) +
  geom_histogram(bins = 35, fill = "#2E86AB", color = "white", alpha = 0.8, size = 0.3) +
  geom_density(aes(y = ..density.. * nrow(spotify_data) * (max(spotify_data$track_popularity) - min(spotify_data$track_popularity)) / 35), 
               color = "#A23B72", size = 1.2, alpha = 0.7) +
  labs(
    title = "Track Popularity Distribution",
    subtitle = "Distribution of track popularity scores across all songs",
    x = "Track Popularity Score",
    y = "Number of Tracks"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = seq(0, 100, 20)) +
  scale_y_continuous(labels = comma_format()) +
  custom_theme

# Save overall popularity histogram
ggsave("overall_track_popularity_histogram.png", overall_histogram, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 2. Calculate average popularity by playlist genre
cat("Calculating average popularity by playlist genre...\n")
avg_popularity_by_genre <- spotify_data %>%
  group_by(playlist_genre) %>%
  summarise(
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_popularity))

# Display average popularity statistics
print(avg_popularity_by_genre)

# Create color palette for genres
genre_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83", "#048A81")

# 3. Average popularity by playlist genre - elegant bar chart
cat("Generating average popularity by genre visualization...\n")
genre_histogram <- ggplot(avg_popularity_by_genre, 
                         aes(x = reorder(playlist_genre, avg_popularity), 
                             y = avg_popularity,
                             fill = playlist_genre)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0(round(avg_popularity, 1))), 
            hjust = -0.2, size = 4, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = genre_colors) +
  scale_y_continuous(limits = c(0, max(avg_popularity_by_genre$avg_popularity) * 1.15),
                     breaks = seq(0, 100, 10)) +
  labs(
    title = "Average Track Popularity by Genre",
    subtitle = "Comparison of mean popularity scores across different playlist genres",
    x = "Playlist Genre",
    y = "Average Popularity Score"
  ) +
  custom_theme +
  theme(
    axis.text.y = element_text(size = 11, color = "grey30", face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Save genre popularity chart
ggsave("avg_popularity_by_genre_histogram.png", genre_histogram, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Additional: Create a combined summary statistics table
summary_stats <- spotify_data %>%
  group_by(playlist_genre) %>%
  summarise(
    tracks = n(),
    avg_popularity = round(mean(track_popularity, na.rm = TRUE), 1),
    median_popularity = round(median(track_popularity, na.rm = TRUE), 1),
    min_popularity = min(track_popularity, na.rm = TRUE),
    max_popularity = max(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_popularity))

cat("\nDetailed Genre Statistics:\n")
print(summary_stats)

cat("\nAnalysis completed! Generated visualization files:\n")
cat("1. overall_track_popularity_histogram.png - Overall popularity distribution\n")
cat("2. avg_popularity_by_genre_histogram.png - Average popularity by genre\n")