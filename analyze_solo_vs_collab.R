# Analyze Solo vs Collaboration Average Scores
library(dplyr)
library(readr)

# Read the CSV file
spotify_data <- read_csv("spotify_songs.csv")

# Identify collaborations based on common patterns in track_artist and track_name
spotify_data <- spotify_data %>%
  mutate(
    is_collaboration = case_when(
      # Check for collaboration indicators in artist name
      grepl("feat\\.|ft\\.|featuring|with|,|&|\\+|vs\\.|versus|x ", track_artist, ignore.case = TRUE) ~ TRUE,
      # Check for collaboration indicators in track name
      grepl("feat\\.|ft\\.|featuring|with|vs\\.|versus", track_name, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    ),
    track_type = ifelse(is_collaboration, "Collaboration", "Solo")
  )

# Calculate basic statistics
cat("=== SOLO vs COLLABORATION ANALYSIS ===\n\n")

total_tracks <- nrow(spotify_data)
solo_count <- sum(!spotify_data$is_collaboration)
collab_count <- sum(spotify_data$is_collaboration)

cat("Dataset Overview:\n")
cat("Total tracks:", format(total_tracks, big.mark = ","), "\n")
cat("Solo tracks:", format(solo_count, big.mark = ","), "(", round(solo_count/total_tracks*100, 1), "%)\n")
cat("Collaboration tracks:", format(collab_count, big.mark = ","), "(", round(collab_count/total_tracks*100, 1), "%)\n\n")

# Calculate average popularity scores
results <- spotify_data %>%
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

cat("POPULARITY SCORE COMPARISON:\n")
cat("Track Type        | Count     | Avg Score | Median | Min | Max | Std Dev\n")
cat("------------------|-----------|-----------|--------|-----|-----|--------\n")
for(i in 1:nrow(results)) {
  cat(sprintf("%-17s | %9s | %9.2f | %6.1f | %3d | %3d | %7.2f\n",
              results$track_type[i],
              format(results$count[i], big.mark = ","),
              results$avg_popularity[i],
              results$median_popularity[i],
              results$min_popularity[i],
              results$max_popularity[i],
              results$std_dev[i]))
}

# Get specific values for comparison
solo_avg <- results$avg_popularity[results$track_type == "Solo"]
collab_avg <- results$avg_popularity[results$track_type == "Collaboration"]
difference <- collab_avg - solo_avg

cat("\n=== FINAL RESULTS ===\n")
cat("Solo tracks average score:", sprintf("%.2f", solo_avg), "\n")
cat("Collaboration tracks average score:", sprintf("%.2f", collab_avg), "\n")
cat("Difference (Collaboration - Solo):", sprintf("%.2f", difference), "\n\n")

if (difference > 0) {
  cat("CONCLUSION: COLLABORATION tracks have HIGHER average popularity!\n")
  cat("Collaborations are", sprintf("%.2f", difference), "points more popular on average.\n")
  percentage_higher <- (difference / solo_avg) * 100
  cat("This represents a", sprintf("%.1f%%", percentage_higher), "increase over solo tracks.\n")
} else {
  cat("CONCLUSION: SOLO tracks have HIGHER average popularity!\n")
  cat("Solo tracks are", sprintf("%.2f", abs(difference)), "points more popular on average.\n")
  percentage_higher <- (abs(difference) / collab_avg) * 100
  cat("This represents a", sprintf("%.1f%%", percentage_higher), "increase over collaboration tracks.\n")
}

# Show some examples
cat("\n=== EXAMPLES ===\n")
cat("Top 5 most popular collaboration tracks:\n")
top_collabs <- spotify_data %>%
  filter(is_collaboration) %>%
  arrange(desc(track_popularity)) %>%
  select(track_name, track_artist, track_popularity) %>%
  head(5)

for(i in 1:nrow(top_collabs)) {
  cat(sprintf("%d. %s - %s (Score: %d)\n", 
              i, top_collabs$track_name[i], top_collabs$track_artist[i], top_collabs$track_popularity[i]))
}

cat("\nTop 5 most popular solo tracks:\n")
top_solos <- spotify_data %>%
  filter(!is_collaboration) %>%
  arrange(desc(track_popularity)) %>%
  select(track_name, track_artist, track_popularity) %>%
  head(5)

for(i in 1:nrow(top_solos)) {
  cat(sprintf("%d. %s - %s (Score: %d)\n", 
              i, top_solos$track_name[i], top_solos$track_artist[i], top_solos$track_popularity[i]))
}

# Statistical significance test
cat("\n=== STATISTICAL SIGNIFICANCE ===\n")
t_test_result <- t.test(
  spotify_data$track_popularity[spotify_data$is_collaboration], 
  spotify_data$track_popularity[!spotify_data$is_collaboration]
)

cat("T-test p-value:", format.pval(t_test_result$p.value), "\n")
if (t_test_result$p.value < 0.05) {
  cat("The difference is STATISTICALLY SIGNIFICANT (p < 0.05)\n")
} else {
  cat("The difference is NOT statistically significant (p >= 0.05)\n")
}

cat("\nAnalysis completed!\n")