# Music Industry Trends Analysis
# Evolution of "trendy" in the music industry over time

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(scales)
library(viridis)
library(gridExtra)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Display basic information
cat("Music Trends Analysis:\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Date range:", min(spotify_data$track_album_release_date, na.rm = TRUE), "to", 
    max(spotify_data$track_album_release_date, na.rm = TRUE), "\n\n")

# Data preprocessing
# Convert release date to proper date format and extract year
spotify_data <- spotify_data %>%
  mutate(
    release_date = as.Date(track_album_release_date),
    release_year = year(release_date),
    # Calculate title length
    title_length = nchar(track_name),
    # Detect collaborations in track names
    has_collaboration = str_detect(track_name, regex("feat\\.|featuring|ft\\.|with|&|x ", ignore_case = TRUE)),
    # Clean artist names for collaboration detection
    has_collab_artist = str_detect(track_artist, regex("&|feat\\.|featuring|ft\\.|with|x ", ignore_case = TRUE))
  ) %>%
  # Filter for reasonable years (remove outliers)
  filter(release_year >= 1990 & release_year <= 2020) %>%
  # Remove missing values
  filter(!is.na(release_year), !is.na(track_popularity))

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

# 1. GENRE POPULARITY EVOLUTION OVER TIME
cat("=== GENRE POPULARITY EVOLUTION ===\n")

# Calculate average popularity by genre and year
genre_trends <- spotify_data %>%
  group_by(release_year, playlist_genre) %>%
  summarise(
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    track_count = n(),
    .groups = 'drop'
  ) %>%
  filter(track_count >= 10)  # Filter out years with too few tracks

# Display genre trends summary
genre_summary <- spotify_data %>%
  group_by(playlist_genre) %>%
  summarise(
    total_tracks = n(),
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    peak_year = release_year[which.max(track_popularity)],
    peak_popularity = max(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_popularity))

cat("Genre Summary (by average popularity):\n")
print(genre_summary)
cat("\n")

# Create genre evolution plot
genre_evolution_plot <- ggplot(genre_trends, aes(x = release_year, y = avg_popularity, color = playlist_genre)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_d(name = "Genre") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(
    title = "Evolution of Genre Popularity Over Time",
    subtitle = "Average track popularity by genre and release year",
    x = "Release Year",
    y = "Average Popularity Score",
    caption = "Only years with 10+ tracks per genre shown"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("genre_evolution_over_time.png", genre_evolution_plot, 
       width = 14, height = 10, dpi = 300, bg = "white")

# 2. SONG TITLE LENGTH TRENDS
cat("=== SONG TITLE LENGTH TRENDS ===\n")

# Calculate title length trends by year
title_length_trends <- spotify_data %>%
  group_by(release_year) %>%
  summarise(
    avg_title_length = mean(title_length, na.rm = TRUE),
    median_title_length = median(title_length, na.rm = TRUE),
    track_count = n(),
    .groups = 'drop'
  ) %>%
  filter(track_count >= 50)  # Filter years with sufficient data

# Statistical test for trend
title_trend_test <- cor.test(title_length_trends$release_year, title_length_trends$avg_title_length)

cat("Title Length Trends:\n")
cat("Correlation between year and average title length:", round(title_trend_test$estimate, 4), "\n")
cat("P-value:", format.pval(title_trend_test$p.value), "\n")
cat("Trend direction:", ifelse(title_trend_test$estimate > 0, "INCREASING", "DECREASING"), "\n\n")

# Display title length statistics by decade
title_by_decade <- spotify_data %>%
  mutate(decade = floor(release_year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(
    avg_length = round(mean(title_length, na.rm = TRUE), 2),
    median_length = median(title_length, na.rm = TRUE),
    min_length = min(title_length, na.rm = TRUE),
    max_length = max(title_length, na.rm = TRUE),
    track_count = n(),
    .groups = 'drop'
  )

cat("Title Length by Decade:\n")
print(title_by_decade)
cat("\n")

# Create title length trend plot
title_length_plot <- ggplot(title_length_trends, aes(x = release_year)) +
  geom_line(aes(y = avg_title_length), color = "#2E86AB", size = 1.5, alpha = 0.8) +
  geom_point(aes(y = avg_title_length), color = "#2E86AB", size = 3, alpha = 0.7) +
  geom_smooth(aes(y = avg_title_length), method = "lm", color = "#A23B72", 
              fill = "#A23B72", alpha = 0.2, size = 1) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(
    title = "Evolution of Song Title Length Over Time",
    subtitle = paste("Average title length trend (correlation =", 
                     round(title_trend_test$estimate, 3), ")"),
    x = "Release Year",
    y = "Average Title Length (characters)",
    caption = "Blue line: actual data, Purple line: trend line"
  ) +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("title_length_trends.png", title_length_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 3. COLLABORATION ANALYSIS
cat("=== COLLABORATION ANALYSIS ===\n")

# Analyze collaboration patterns
collab_analysis <- spotify_data %>%
  mutate(
    collaboration = case_when(
      has_collaboration | has_collab_artist ~ "Collaboration",
      TRUE ~ "Solo"
    )
  )

# Overall collaboration statistics
collab_stats <- collab_analysis %>%
  group_by(collaboration) %>%
  summarise(
    track_count = n(),
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    median_popularity = median(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(percentage = round(track_count / sum(track_count) * 100, 2))

cat("Overall Collaboration Statistics:\n")
print(collab_stats)
cat("\n")

# Statistical test for collaboration effect
collab_test <- t.test(track_popularity ~ collaboration, data = collab_analysis)
cat("T-test for collaboration effect on popularity:\n")
cat("Mean popularity - Collaboration:", round(collab_test$estimate[1], 2), "\n")
cat("Mean popularity - Solo:", round(collab_test$estimate[2], 2), "\n")
cat("Difference:", round(collab_test$estimate[1] - collab_test$estimate[2], 2), "\n")
cat("P-value:", format.pval(collab_test$p.value), "\n")
cat("Effect:", ifelse(collab_test$p.value < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), "\n\n")

# Collaboration trends over time
collab_trends <- collab_analysis %>%
  group_by(release_year, collaboration) %>%
  summarise(
    track_count = n(),
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(release_year) %>%
  mutate(
    total_tracks = sum(track_count),
    percentage = track_count / total_tracks * 100
  ) %>%
  filter(total_tracks >= 50)

# Create collaboration trends plot
collab_trends_plot <- ggplot(collab_trends, aes(x = release_year, y = percentage, fill = collaboration)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("Collaboration" = "#A23B72", "Solo" = "#2E86AB"),
                    name = "Track Type") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evolution of Collaborations in Music",
    subtitle = "Percentage of collaborative vs solo tracks over time",
    x = "Release Year",
    y = "Percentage of Tracks",
    caption = "Based on track names and artist credits"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("collaboration_trends.png", collab_trends_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 4. COLLABORATION POPULARITY BY GENRE
collab_by_genre <- collab_analysis %>%
  group_by(playlist_genre, collaboration) %>%
  summarise(
    track_count = n(),
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = collaboration, values_from = c(track_count, avg_popularity)) %>%
  mutate(
    collab_advantage = `avg_popularity_Collaboration` - `avg_popularity_Solo`,
    collab_percentage = `track_count_Collaboration` / (`track_count_Collaboration` + `track_count_Solo`) * 100
  )

cat("Collaboration Effect by Genre:\n")
print(collab_by_genre %>% 
      select(playlist_genre, collab_advantage, collab_percentage) %>%
      arrange(desc(collab_advantage)))
cat("\n")

# Create collaboration effect by genre plot
collab_genre_plot <- ggplot(collab_by_genre, aes(x = reorder(playlist_genre, collab_advantage), 
                                                 y = collab_advantage)) +
  geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(ifelse(collab_advantage > 0, "+", ""), round(collab_advantage, 1))),
            hjust = ifelse(collab_by_genre$collab_advantage > 0, -0.1, 1.1),
            size = 4, color = "grey30", fontface = "bold") +
  coord_flip() +
  labs(
    title = "Collaboration Effect on Popularity by Genre",
    subtitle = "Difference in average popularity: Collaboration - Solo",
    x = "Music Genre",
    y = "Popularity Advantage of Collaborations",
    caption = "Positive values indicate collaborations are more popular"
  ) +
  custom_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("collaboration_effect_by_genre.png", collab_genre_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 5. COMPREHENSIVE TRENDS DASHBOARD
# Create a summary of key trends
trends_summary <- data.frame(
  Metric = c("Genre with highest average popularity", 
             "Genre with most growth potential",
             "Title length trend",
             "Collaboration effect on popularity",
             "Most collaborative genre",
             "Decade with longest titles"),
  Finding = c(
    paste(genre_summary$playlist_genre[1], "(", round(genre_summary$avg_popularity[1], 1), ")"),
    "Analysis by year-over-year growth needed",
    ifelse(title_trend_test$estimate > 0, "Getting LONGER", "Getting SHORTER"),
    ifelse(collab_test$p.value < 0.05, 
           paste("SIGNIFICANT advantage:", round(collab_test$estimate[1] - collab_test$estimate[2], 1), "points"),
           "NO significant effect"),
    paste(collab_by_genre$playlist_genre[which.max(collab_by_genre$collab_percentage)], 
          "(", round(max(collab_by_genre$collab_percentage, na.rm = TRUE), 1), "%)"),
    paste(title_by_decade$decade[which.max(title_by_decade$avg_length)], "s",
          "(", title_by_decade$avg_length[which.max(title_by_decade$avg_length)], " chars)")
  )
)

cat("=== KEY TRENDS SUMMARY ===\n")
print(trends_summary, row.names = FALSE)
cat("\n")

# 6. ADDITIONAL INSIGHTS
cat("=== ADDITIONAL INSIGHTS ===\n")

# Most popular years overall
popular_years <- spotify_data %>%
  group_by(release_year) %>%
  summarise(
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    track_count = n(),
    .groups = 'drop'
  ) %>%
  filter(track_count >= 100) %>%
  arrange(desc(avg_popularity)) %>%
  head(5)

cat("Top 5 years by average track popularity:\n")
print(popular_years)
cat("\n")

# Genre diversity over time
genre_diversity <- spotify_data %>%
  group_by(release_year) %>%
  summarise(
    unique_genres = n_distinct(playlist_genre),
    total_tracks = n(),
    .groups = 'drop'
  ) %>%
  filter(total_tracks >= 50)

diversity_trend <- cor.test(genre_diversity$release_year, genre_diversity$unique_genres)
cat("Genre diversity trend over time:\n")
cat("Correlation:", round(diversity_trend$estimate, 4), "\n")
cat("P-value:", format.pval(diversity_trend$p.value), "\n\n")

cat("Generated files:\n")
cat("1. genre_evolution_over_time.png - Genre popularity trends\n")
cat("2. title_length_trends.png - Song title length evolution\n")
cat("3. collaboration_trends.png - Collaboration percentage over time\n")
cat("4. collaboration_effect_by_genre.png - Collaboration impact by genre\n")

# 7. FINAL RECOMMENDATIONS FOR RECORD LABELS
cat("\n=== RECOMMENDATIONS FOR RECORD LABELS ===\n")
cat("Based on the trends analysis:\n\n")

cat("1. GENRE STRATEGY:\n")
cat("   - Focus on", genre_summary$playlist_genre[1], "genre (highest avg popularity:", 
    round(genre_summary$avg_popularity[1], 1), ")\n")
cat("   - Consider diversifying portfolio as genre preferences evolve\n\n")

cat("2. TITLE STRATEGY:\n")
if(title_trend_test$estimate > 0) {
  cat("   - Titles are getting LONGER over time\n")
  cat("   - Consider more descriptive, longer titles for modern appeal\n")
} else {
  cat("   - Titles are getting SHORTER over time\n")
  cat("   - Consider concise, catchy titles for modern appeal\n")
}
cat("\n")

cat("3. COLLABORATION STRATEGY:\n")
if(collab_test$p.value < 0.05) {
  if(collab_test$estimate[1] > collab_test$estimate[2]) {
    cat("   - Collaborations significantly BOOST popularity by", 
        round(collab_test$estimate[1] - collab_test$estimate[2], 1), "points\n")
    cat("   - Actively pursue collaborative projects\n")
  } else {
    cat("   - Solo tracks perform better than collaborations\n")
    cat("   - Focus on developing individual artist brands\n")
  }
} else {
  cat("   - No significant difference between solo and collaborative tracks\n")
  cat("   - Base collaboration decisions on artistic merit rather than popularity impact\n")
}

# Best genre for collaborations
best_collab_genre <- collab_by_genre$playlist_genre[which.max(collab_by_genre$collab_advantage)]
cat("   - Best genre for collaborations:", best_collab_genre, "\n")
cat("   - Collaboration advantage:", round(max(collab_by_genre$collab_advantage, na.rm = TRUE), 1), "points\n")

cat("\nAnalysis complete! Check generated visualizations for detailed insights.\n")