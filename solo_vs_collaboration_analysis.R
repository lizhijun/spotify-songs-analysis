# Solo vs Collaboration Analysis
# Calculate and compare average popularity scores

library(ggplot2)
library(dplyr)
library(readr)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

cat("=== Solo vs Collaboration Popularity Analysis ===\n\n")

# Identify collaborations based on common indicators in artist names
spotify_data <- spotify_data %>%
  mutate(
    is_collaboration = case_when(
      # Look for common collaboration indicators
      grepl("feat\\.|ft\\.|featuring|with|,|&|\\+|vs\\.|versus", track_artist, ignore.case = TRUE) ~ TRUE,
      # Also check track names for collaboration indicators
      grepl("feat\\.|ft\\.|featuring|with|vs\\.|versus", track_name, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    ),
    track_type = ifelse(is_collaboration, "Collaboration", "Solo")
  )

# Calculate basic statistics
total_tracks <- nrow(spotify_data)
solo_tracks <- sum(!spotify_data$is_collaboration)
collab_tracks <- sum(spotify_data$is_collaboration)

cat("Dataset Overview:\n")
cat("Total tracks:", total_tracks, "\n")
cat("Solo tracks:", solo_tracks, "(", round(solo_tracks/total_tracks*100, 1), "%)\n")
cat("Collaboration tracks:", collab_tracks, "(", round(collab_tracks/total_tracks*100, 1), "%)\n\n")

# Calculate average popularity scores
popularity_comparison <- spotify_data %>%
  group_by(track_type) %>%
  summarise(
    count = n(),
    avg_popularity = mean(track_popularity, na.rm = TRUE),
    median_popularity = median(track_popularity, na.rm = TRUE),
    min_popularity = min(track_popularity, na.rm = TRUE),
    max_popularity = max(track_popularity, na.rm = TRUE),
    std_dev = sd(track_popularity, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Popularity Score Comparison:\n")
print(popularity_comparison)
cat("\n")

# Determine which is higher
solo_avg <- popularity_comparison$avg_popularity[popularity_comparison$track_type == "Solo"]
collab_avg <- popularity_comparison$avg_popularity[popularity_comparison$track_type == "Collaboration"]

difference <- collab_avg - solo_avg

cat("=== RESULTS ===\n")
cat("Solo tracks average popularity:", round(solo_avg, 2), "\n")
cat("Collaboration tracks average popularity:", round(collab_avg, 2), "\n")
cat("Difference (Collaboration - Solo):", round(difference, 2), "\n")

if (difference > 0) {
  cat("CONCLUSION: Collaboration tracks have HIGHER average popularity\n")
  cat("Collaborations are", round(difference, 2), "points more popular on average\n")
} else {
  cat("CONCLUSION: Solo tracks have HIGHER average popularity\n")
  cat("Solo tracks are", round(abs(difference), 2), "points more popular on average\n")
}

# Statistical significance test
t_test_result <- t.test(
  spotify_data$track_popularity[spotify_data$is_collaboration], 
  spotify_data$track_popularity[!spotify_data$is_collaboration]
)

cat("\nStatistical Significance Test (t-test):\n")
cat("p-value:", format.pval(t_test_result$p.value), "\n")
if (t_test_result$p.value < 0.05) {
  cat("The difference is STATISTICALLY SIGNIFICANT (p < 0.05)\n")
} else {
  cat("The difference is NOT statistically significant (p >= 0.05)\n")
}

# Create visualization
custom_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "grey20"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey40"),
    axis.title = element_text(size = 11, color = "grey30", face = "bold"),
    axis.text = element_text(size = 10, color = "grey50"),
    legend.title = element_text(size = 10, face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
  )

# Bar chart comparison
comparison_plot <- ggplot(popularity_comparison, aes(x = track_type, y = avg_popularity, fill = track_type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(avg_popularity, 1), "\n(n=", format(count, big.mark = ","), ")")), 
            vjust = -0.5, size = 4, fontface = "bold", color = "grey30") +
  scale_fill_manual(values = c("Solo" = "#2E86AB", "Collaboration" = "#A23B72")) +
  labs(
    title = "Average Popularity: Solo vs Collaboration Tracks",
    subtitle = paste0("Difference: ", round(difference, 2), " points ", 
                     ifelse(difference > 0, "(Collaborations higher)", "(Solo higher)")),
    x = "Track Type",
    y = "Average Popularity Score",
    caption = paste0("Total tracks analyzed: ", format(total_tracks, big.mark = ","))
  ) +
  ylim(0, max(popularity_comparison$avg_popularity) * 1.2) +
  custom_theme +
  theme(legend.position = "none")

ggsave("solo_vs_collaboration_comparison.png", comparison_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Box plot for distribution comparison
distribution_plot <- ggplot(spotify_data, aes(x = track_type, y = track_popularity, fill = track_type)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = c("Solo" = "#2E86AB", "Collaboration" = "#A23B72")) +
  labs(
    title = "Popularity Score Distribution: Solo vs Collaboration",
    subtitle = "Box plots showing the distribution of popularity scores",
    x = "Track Type",
    y = "Popularity Score",
    caption = "Box shows median and quartiles; whiskers show range"
  ) +
  custom_theme +
  theme(legend.position = "none")

ggsave("solo_vs_collaboration_distribution.png", distribution_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Show some examples
cat("\n=== EXAMPLES ===\n")
cat("Top 5 most popular collaboration tracks:\n")
top_collabs <- spotify_data %>%
  filter(is_collaboration) %>%
  arrange(desc(track_popularity)) %>%
  select(track_name, track_artist, track_popularity) %>%
  head(5)
print(top_collabs)

cat("\nTop 5 most popular solo tracks:\n")
top_solos <- spotify_data %>%
  filter(!is_collaboration) %>%
  arrange(desc(track_popularity)) %>%
  select(track_name, track_artist, track_popularity) %>%
  head(5)
print(top_solos)

cat("\nGenerated visualization files:\n")
cat("1. solo_vs_collaboration_comparison.png - Average popularity comparison\n")
cat("2. solo_vs_collaboration_distribution.png - Distribution comparison\n")