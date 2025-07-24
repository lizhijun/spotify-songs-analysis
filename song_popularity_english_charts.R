# Song Popularity Prediction - Key Features Analysis
# Multiple Linear Regression Analysis with English Charts

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(broom)
library(gridExtra)
library(scales)
library(viridis)
library(RColorBrewer)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Data preprocessing
cat("=== Data Overview ===\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Popularity range:", min(spotify_data$track_popularity), "-", max(spotify_data$track_popularity), "\n")
cat("Average popularity:", round(mean(spotify_data$track_popularity), 2), "\n")
cat("Genres:", paste(unique(spotify_data$playlist_genre), collapse = ", "), "\n\n")

# Select features for modeling
features <- c("danceability", "energy", "loudness", "speechiness", 
              "acousticness", "instrumentalness", "liveness", "valence", 
              "tempo", "duration_ms", "playlist_genre")

# Create modeling dataset
model_data <- spotify_data %>%
  select(track_popularity, all_of(features)) %>%
  na.omit()

# Convert genre to factor
model_data$playlist_genre <- as.factor(model_data$playlist_genre)

# Custom theme
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

# 1. Build multiple linear regression model
cat("=== Multiple Linear Regression Analysis ===\n")
model <- lm(track_popularity ~ ., data = model_data)
model_summary <- summary(model)

# Display model performance
cat("Model Performance:\n")
cat("R-squared:", round(model_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
cat("F-statistic p-value:", format.pval(pf(model_summary$fstatistic[1], 
                                          model_summary$fstatistic[2], 
                                          model_summary$fstatistic[3], 
                                          lower.tail = FALSE)), "\n\n")

# 2. Extract and analyze coefficients
# Audio feature coefficients
audio_coefficients <- tidy(model) %>%
  filter(!grepl("^playlist_genre", term) & term != "(Intercept)") %>%
  arrange(desc(abs(estimate))) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    effect_type = ifelse(estimate > 0, "Positive", "Negative")
  )

# Genre coefficients
genre_coefficients <- tidy(model) %>%
  filter(grepl("^playlist_genre", term)) %>%
  arrange(desc(estimate)) %>%
  mutate(
    genre = gsub("playlist_genre", "", term),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    effect_type = ifelse(estimate > 0, "Positive", "Negative")
  )

# 3. Identify the most important features
# Combine audio features and genres, find the most important positive features
all_coefficients <- bind_rows(
  audio_coefficients %>% 
    mutate(feature_type = "Audio Feature", clean_name = term),
  genre_coefficients %>% 
    mutate(feature_type = "Music Genre", clean_name = genre)
) %>%
  filter(p.value < 0.05) %>%  # Only consider significant features
  arrange(desc(estimate))  # Sort by positive impact

# Find the most important positive features
top_positive_features <- all_coefficients %>%
  filter(estimate > 0) %>%
  head(3)

cat("=== Top 3 Most Important Features for Song Popularity ===\n")
for(i in 1:nrow(top_positive_features)) {
  cat(sprintf("%d. %s (%s)\n", 
              i,
              top_positive_features$clean_name[i], 
              top_positive_features$feature_type[i]))
  cat(sprintf("   Coefficient: %.3f %s\n", 
              top_positive_features$estimate[i], 
              top_positive_features$significance[i]))
  cat(sprintf("   p-value: %.5f\n\n", top_positive_features$p.value[i]))
}

# 4. Create feature importance visualization
# Prepare combined feature data
combined_features <- all_coefficients %>%
  filter(estimate > 0) %>%
  head(10) %>%  # Take top 10 most important positive features
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Combined importance plot
combined_plot <- ggplot(combined_features, aes(x = reorder(clean_name, estimate), 
                                              y = estimate, 
                                              fill = feature_type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = -0.1, size = 4, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Audio Feature" = "#2E86AB", "Music Genre" = "#A23B72")) +
  labs(
    title = "Key Features Enhancing Song Popularity",
    subtitle = "Feature Importance Based on Multiple Linear Regression Analysis",
    x = "Feature",
    y = "Regression Coefficient (Positive Impact on Popularity)",
    fill = "Feature Type",
    caption = "*** p<0.001, ** p<0.01, * p<0.05\nHigher values indicate stronger positive impact on popularity"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.caption = element_text(size = 9, color = "grey60")
  )

ggsave("key_features_for_popularity_english.png", combined_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 5. Scatter plots showing relationship between top features and popularity
# Get the most important audio features
top_audio_features <- audio_coefficients %>%
  filter(estimate > 0) %>%
  head(3) %>%
  pull(term)

# Create scatter plots for each important feature
scatter_plots <- list()

for(i in 1:length(top_audio_features)) {
  feature <- top_audio_features[i]
  
  # Calculate correlation
  correlation <- cor(model_data[[feature]], model_data$track_popularity)
  
  p <- ggplot(model_data, aes(x = .data[[feature]], y = track_popularity)) +
    geom_point(alpha = 0.3, color = "#2E86AB", size = 0.8) +
    geom_smooth(method = "lm", color = "#A23B72", linewidth = 1.2, se = TRUE, alpha = 0.2) +
    labs(
      title = paste("Popularity vs", tools::toTitleCase(feature)),
      subtitle = paste("Correlation:", round(correlation, 3)),
      x = tools::toTitleCase(feature),
      y = "Track Popularity"
    ) +
    custom_theme +
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10))
  
  scatter_plots[[i]] <- p
}

# Combine scatter plots
combined_scatter <- do.call(grid.arrange, c(scatter_plots, ncol = 2))
ggsave("key_features_scatter_plots_english.png", combined_scatter, 
       width = 12, height = 10, dpi = 300, bg = "white")

# 6. Genre impact on popularity
# Genre impact plot
genre_plot <- ggplot(genre_coefficients, aes(x = reorder(genre, estimate), 
                                            y = estimate, 
                                            fill = effect_type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = ifelse(genre_coefficients$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#A23B72")) +
  labs(
    title = "Music Genre Impact on Song Popularity",
    subtitle = "Regression Coefficients Showing Genre Effects Relative to Baseline (EDM)",
    x = "Music Genre",
    y = "Regression Coefficient",
    fill = "Effect Type",
    caption = "*** p<0.001, ** p<0.01, * p<0.05"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

ggsave("genre_impact_on_popularity_english.png", genre_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 7. Multicollinearity check
# Calculate correlation matrix between audio features
numeric_features <- model_data %>% 
  select(-playlist_genre, -track_popularity) %>%
  names()

correlation_matrix <- cor(model_data[, numeric_features])

# Create correlation heatmap
png("features_correlation_heatmap_english.png", width = 10, height = 8, units = "in", res = 300)
corrplot(correlation_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#2E86AB", "white", "#A23B72"))(200),
         title = "Audio Features Correlation Matrix",
         mar = c(0,0,2,0))
dev.off()

# 8. Final conclusion
cat("\n=== Final Conclusion: Top 3 Most Important Features for Song Popularity ===\n")
cat("Based on multiple linear regression analysis, as a record label agent looking for potential young talent,\n")
cat("you should focus on the following three most important features:\n\n")

for(i in 1:min(3, nrow(top_positive_features))) {
  feature_name <- top_positive_features$clean_name[i]
  feature_type <- top_positive_features$feature_type[i]
  coefficient <- top_positive_features$estimate[i]
  p_value <- top_positive_features$p.value[i]
  
  feature_description <- case_when(
    feature_name == "danceability" ~ "Danceability - The rhythm and ease of dancing to the song",
    feature_name == "pop" ~ "Pop Genre - Mainstream popular music style",
    feature_name == "rock" ~ "Rock Genre - Rock music style", 
    feature_name == "latin" ~ "Latin Genre - Latin music style",
    feature_name == "acousticness" ~ "Acousticness - Proportion of acoustic instruments in the song",
    feature_name == "loudness" ~ "Loudness - Overall volume level of the song",
    feature_name == "valence" ~ "Valence - Positivity of emotions conveyed by the song",
    TRUE ~ feature_name
  )
  
  cat(sprintf("%d. %s (%s)\n", i, feature_description, feature_type))
  cat(sprintf("   Impact coefficient: +%.2f (p < %.5f)\n", coefficient, p_value))
  cat(sprintf("   Practical meaning: For each unit increase in this feature, popularity increases by %.2f points on average\n\n", coefficient))
}

cat("Generated visualization files:\n")
cat("1. key_features_for_popularity_english.png - Key features ranking\n")
cat("2. key_features_scatter_plots_english.png - Key features scatter plots\n")
cat("3. genre_impact_on_popularity_english.png - Genre impact analysis\n")
cat("4. features_correlation_heatmap_english.png - Features correlation heatmap\n")