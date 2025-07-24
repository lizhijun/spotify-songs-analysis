# Music Trends Analysis
# Analyzing trends in popular music using Spotify dataset

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(gridExtra)
library(scales)
library(viridis)
library(tidyr)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Display basic information
cat("Dataset Overview:\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Date range:", min(spotify_data$track_album_release_date), "to", max(spotify_data$track_album_release_date), "\n\n")

# Custom theme for visualizations
custom_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "grey20"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey40"),
    axis.title = element_text(size = 11, color = "grey30", face = "bold"),
    axis.text = element_text(size = 9, color = "grey50"),
    legend.title = element_text(size = 10, face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
  )

# 1. Track Name Length Analysis
cat("Analyzing track name length trends...\n")

# Extract year from release date and calculate track name length
spotify_data <- spotify_data %>%
  mutate(
    release_year = as.numeric(substr(track_album_release_date, 1, 4)),
    track_name_length = nchar(track_name)
  )

# Filter for reasonable years (some data might have errors)
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
  filter(count >= 10)  # Only include years with sufficient data

# Plot track name length trend
title_length_plot <- ggplot(track_name_by_year, aes(x = release_year, y = avg_name_length)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(color = "#A23B72", size = 2) +
  geom_smooth(method = "loess", color = "#F18F01", se = TRUE, linewidth = 1, alpha = 0.2) +
  labs(
    title = "Track Title Length Over Time",
    subtitle = "Average number of characters in song titles by year",
    x = "Release Year",
    y = "Average Title Length (characters)"
  ) +
  scale_x_continuous(breaks = seq(min(track_name_by_year$release_year), 
                                 max(track_name_by_year$release_year), 
                                 by = 5)) +
  custom_theme

ggsave("title_length_trends.png", title_length_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 2. Collaboration Analysis
cat("Analyzing collaboration effects on popularity...\n")

# Identify collaborations (tracks with "feat.", "with", "&", "," in artist name)
spotify_data <- spotify_data %>%
  mutate(
    is_collaboration = case_when(
      grepl("feat\\.|ft\\.|with|,|&", track_artist, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Compare popularity of collaborations vs. solo tracks
collab_vs_solo <- spotify_data %>%
  group_by(is_collaboration) %>%
  summarise(
    avg_popularity = mean(track_popularity),
    median_popularity = median(track_popularity),
    count = n(),
    .groups = 'drop'
  )

# Plot collaboration vs. popularity
collab_plot <- ggplot(collab_vs_solo, aes(x = factor(is_collaboration, 
                                                    labels = c("Solo", "Collaboration")), 
                                         y = avg_popularity, 
                                         fill = factor(is_collaboration))) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(avg_popularity, 1)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "Impact of Collaborations on Track Popularity",
    subtitle = "Average popularity score for solo tracks vs. collaborations",
    x = "Track Type",
    y = "Average Popularity Score"
  ) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72"), 
                    name = "Track Type") +
  ylim(0, max(collab_vs_solo$avg_popularity) * 1.2) +
  custom_theme +
  theme(legend.position = "none")

ggsave("collaboration_effect.png", collab_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Collaboration trends over time
collab_by_year <- spotify_data %>%
  filter(release_year >= 1950 & release_year <= 2023) %>%
  group_by(release_year) %>%
  summarise(
    collab_percentage = mean(is_collaboration) * 100,
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 10)  # Only include years with sufficient data

# Plot collaboration trends
collab_trend_plot <- ggplot(collab_by_year, aes(x = release_year, y = collab_percentage)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(color = "#A23B72", size = 2) +
  geom_smooth(method = "loess", color = "#F18F01", se = TRUE, linewidth = 1, alpha = 0.2) +
  labs(
    title = "Collaboration Trends Over Time",
    subtitle = "Percentage of tracks featuring collaborations by year",
    x = "Release Year",
    y = "Collaboration Percentage (%)"
  ) +
  scale_x_continuous(breaks = seq(min(collab_by_year$release_year), 
                                 max(collab_by_year$release_year), 
                                 by = 5)) +
  scale_y_continuous(limits = c(0, max(collab_by_year$collab_percentage) * 1.1)) +
  custom_theme

ggsave("collaboration_trends.png", collab_trend_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Collaboration effect by genre
collab_by_genre <- spotify_data %>%
  group_by(playlist_genre, is_collaboration) %>%
  summarise(
    avg_popularity = mean(track_popularity),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 10) %>%  # Only include genres with sufficient data
  pivot_wider(
    names_from = is_collaboration,
    values_from = avg_popularity,
    names_prefix = "popularity_"
  ) %>%
  mutate(
    popularity_diff = popularity_TRUE - popularity_FALSE,
    effect = ifelse(popularity_diff > 0, "Positive", "Negative")
  )

# Plot collaboration effect by genre
collab_genre_plot <- ggplot(collab_by_genre, aes(x = reorder(playlist_genre, popularity_diff), 
                                               y = popularity_diff,
                                               fill = effect)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = round(popularity_diff, 1)), 
            hjust = ifelse(collab_by_genre$popularity_diff > 0, -0.1, 1.1),
            size = 4, fontface = "bold", color = "grey30") +
  coord_flip() +
  scale_fill_manual(values = c("Negative" = "#A23B72", "Positive" = "#2E86AB")) +
  labs(
    title = "Impact of Collaborations by Genre",
    subtitle = "Difference in popularity between collaborations and solo tracks",
    x = "Genre",
    y = "Popularity Difference (Collaboration - Solo)"
  ) +
  custom_theme +
  theme(legend.position = "none")

ggsave("collaboration_effect_by_genre.png", collab_genre_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 3. Other Trends Analysis
cat("Analyzing other music trends...\n")

# Genre popularity over time
genre_by_year <- spotify_data %>%
  filter(release_year >= 1950 & release_year <= 2023) %>%
  group_by(release_year, playlist_genre) %>%
  summarise(
    avg_popularity = mean(track_popularity),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 5)  # Only include year-genre combinations with sufficient data

# Plot genre evolution
genre_evolution_plot <- ggplot(genre_by_year, aes(x = release_year, 
                                                y = avg_popularity, 
                                                color = playlist_genre)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 1.5) +
  scale_color_viridis_d() +
  labs(
    title = "Genre Popularity Evolution Over Time",
    subtitle = "Average popularity score by genre and year",
    x = "Release Year",
    y = "Average Popularity Score",
    color = "Genre"
  ) +
  scale_x_continuous(breaks = seq(min(genre_by_year$release_year), 
                                 max(genre_by_year$release_year), 
                                 by = 5)) +
  custom_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggsave("genre_evolution_over_time.png", genre_evolution_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Audio features trends over time
# Select key audio features
key_features <- c("danceability", "energy", "acousticness", "valence")

# Prepare data for audio features trends
audio_features_by_year <- spotify_data %>%
  filter(release_year >= 1950 & release_year <= 2023) %>%
  group_by(release_year) %>%
  summarise(
    across(all_of(key_features), mean),
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 10) %>%  # Only include years with sufficient data
  pivot_longer(
    cols = all_of(key_features),
    names_to = "feature",
    values_to = "value"
  )

# Plot audio features trends
audio_features_plot <- ggplot(audio_features_by_year, aes(x = release_year, 
                                                        y = value, 
                                                        color = feature)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    title = "Audio Features Trends Over Time",
    subtitle = "Evolution of key audio characteristics in music",
    x = "Release Year",
    y = "Average Value (0-1 scale)",
    color = "Audio Feature"
  ) +
  scale_x_continuous(breaks = seq(min(audio_features_by_year$release_year), 
                                 max(audio_features_by_year$release_year), 
                                 by = 5)) +
  custom_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggsave("audio_features_trends.png", audio_features_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Song duration trends
duration_by_year <- spotify_data %>%
  filter(release_year >= 1950 & release_year <= 2023) %>%
  group_by(release_year) %>%
  summarise(
    avg_duration_sec = mean(duration_ms) / 1000,  # Convert to seconds
    count = n(),
    .groups = 'drop'
  ) %>%
  filter(count >= 10)  # Only include years with sufficient data

# Plot duration trends
duration_plot <- ggplot(duration_by_year, aes(x = release_year, y = avg_duration_sec)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(color = "#A23B72", size = 2) +
  geom_smooth(method = "loess", color = "#F18F01", se = TRUE, linewidth = 1, alpha = 0.2) +
  labs(
    title = "Song Duration Trends Over Time",
    subtitle = "Average track length in seconds by year",
    x = "Release Year",
    y = "Average Duration (seconds)"
  ) +
  scale_x_continuous(breaks = seq(min(duration_by_year$release_year), 
                                 max(duration_by_year$release_year), 
                                 by = 5)) +
  custom_theme

ggsave("song_duration_trends.png", duration_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("\nAnalysis completed! Generated visualization files:\n")
cat("1. title_length_trends.png - Track title length over time\n")
cat("2. collaboration_effect.png - Impact of collaborations on popularity\n")
cat("3. collaboration_trends.png - Collaboration trends over time\n")
cat("4. collaboration_effect_by_genre.png - Impact of collaborations by genre\n")
cat("5. genre_evolution_over_time.png - Genre popularity evolution\n")
cat("6. audio_features_trends.png - Audio features trends over time\n")
cat("7. song_duration_trends.png - Song duration trends over time\n")