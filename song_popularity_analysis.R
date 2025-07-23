# Song Popularity Prediction Analysis
# Multiple Linear Regression Analysis

# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(broom)
library(gridExtra)
library(scales)
library(viridis)

# Read data
cat("Loading data...\n")
spotify_data <- read_csv("spotify_songs.csv")

# Data overview
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

model_data$playlist_genre <- as.factor(model_data$playlist_genre)

cat("Modeling dataset size:", nrow(model_data), "rows\n\n")

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

# 2. Extract and analyze regression coefficients
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

cat("Audio Feature Coefficients (sorted by impact):\n")
print(audio_coefficients)
cat("\n")

cat("Genre Coefficients (sorted by impact):\n")
print(genre_coefficients)
cat("\n")

# 3. Find the most important three features
# Combine audio features and genres, find most important positive factors
all_coefficients <- bind_rows(
  audio_coefficients %>% select(term, estimate, p.value, significance, effect_type),
  genre_coefficients %>% select(term, estimate, p.value, significance, effect_type)
) %>%
  filter(p.value < 0.05) %>%  # Only consider significant features
  arrange(desc(estimate))

top_3_features <- head(all_coefficients, 3)

cat("=== Top 3 Most Important Features (Increase Song Popularity) ===\n")
for(i in 1:nrow(top_3_features)) {
  feature_name <- top_3_features$term[i]
  if(grepl("playlist_genre", feature_name)) {
    feature_name <- paste("Genre:", gsub("playlist_genre", "", feature_name))
  } else {
    feature_name <- paste("Audio Feature:", feature_name)
  }
  
  cat(sprintf("%d. %s\n", i, feature_name))
  cat(sprintf("   Coefficient: %.3f %s\n", top_3_features$estimate[i], top_3_features$significance[i]))
  cat(sprintf("   P-value: %.5f\n", top_3_features$p.value[i]))
  cat(sprintf("   Effect: %s\n\n", top_3_features$effect_type[i]))
}# 4. 
Create feature importance visualizations
cat("Generating feature importance visualizations...\n")

# Prepare audio feature importance data
audio_importance <- audio_coefficients %>%
  mutate(
    abs_estimate = abs(estimate),
    color_group = ifelse(estimate > 0, "Positive", "Negative"),
    feature_label = paste0(term, " (", round(estimate, 3), significance, ")")
  )

# Audio feature importance plot
audio_plot <- ggplot(audio_importance, aes(x = reorder(term, abs_estimate), 
                                          y = estimate, 
                                          fill = color_group)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 3), significance)), 
            hjust = ifelse(audio_importance$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#A23B72")) +
  labs(
    title = "Audio Features Impact on Song Popularity",
    subtitle = "Regression coefficients showing influence of audio features",
    x = "Audio Features",
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

ggsave("audio_features_importance.png", audio_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

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
    subtitle = "Genre effects relative to baseline genre (EDM)",
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

ggsave("genre_impact.png", genre_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 5. Combined feature importance plot (top 10 most important features)
top_10_features <- head(all_coefficients, 10) %>%
  mutate(
    feature_display = ifelse(grepl("playlist_genre", term), 
                           paste("Genre:", gsub("playlist_genre", "", term)),
                           paste("Audio:", term)),
    abs_estimate = abs(estimate)
  )

combined_plot <- ggplot(top_10_features, aes(x = reorder(feature_display, estimate), 
                                           y = estimate, 
                                           fill = effect_type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = ifelse(top_10_features$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#A23B72")) +
  labs(
    title = "Key Factors Influencing Song Popularity",
    subtitle = "Top 10 Most Important Features (Audio Features + Genres)",
    x = "Features",
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

ggsave("top_features_combined.png", combined_plot, 
       width = 14, height = 10, dpi = 300, bg = "white")

# 6. Correlation analysis
cat("Conducting correlation analysis...\n")

# Calculate correlations between numeric features and popularity
numeric_features <- model_data %>%
  select(-playlist_genre, -track_popularity) %>%
  names()

correlations <- cor(model_data[, c("track_popularity", numeric_features)])
popularity_correlations <- correlations[1, -1]

# Sort by correlation strength
sorted_correlations <- sort(abs(popularity_correlations), decreasing = TRUE)

cat("Correlations with Track Popularity (sorted by strength):\n")
for(i in 1:length(sorted_correlations)) {
  feature <- names(sorted_correlations)[i]
  correlation <- popularity_correlations[feature]
  cat(sprintf("%-15s: %6.3f\n", feature, correlation))
}
cat("\n")

# Create correlation heatmap
png("correlation_heatmap.png", width = 10, height = 8, units = "in", res = 300)
corrplot(correlations, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#A23B72", "white", "#2E86AB"))(200),
         title = "Audio Features Correlation with Popularity",
         mar = c(0,0,2,0))
dev.off()

# 7. Scatter plots for top 3 features
cat("Generating scatter plots...\n")

# Get top 3 numeric features
top_3_numeric <- names(head(sorted_correlations[names(sorted_correlations) %in% numeric_features], 3))

scatter_plots <- list()
for(i in 1:length(top_3_numeric)) {
  feature <- top_3_numeric[i]
  correlation <- round(popularity_correlations[feature], 3)
  
  p <- ggplot(model_data, aes(x = .data[[feature]], y = track_popularity)) +
    geom_point(alpha = 0.3, color = "#2E86AB", size = 0.8) +
    geom_smooth(method = "lm", color = "#A23B72", linewidth = 1.2, se = TRUE, alpha = 0.2) +
    labs(
      title = paste("Popularity vs", tools::toTitleCase(gsub("_", " ", feature))),
      subtitle = paste("Correlation:", correlation),
      x = tools::toTitleCase(gsub("_", " ", feature)),
      y = "Track Popularity"
    ) +
    custom_theme +
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10))
  
  scatter_plots[[i]] <- p
}

# Combine scatter plots
combined_scatter <- do.call(grid.arrange, c(scatter_plots, ncol = 3))
ggsave("top_features_scatter.png", combined_scatter, 
       width = 15, height = 5, dpi = 300, bg = "white")#
 8. Model diagnostics and multicollinearity check
cat("=== Model Diagnostics ===\n")

# Model goodness of fit assessment
cat("Model Goodness of Fit Assessment:\n")
cat(sprintf("R-squared: %.4f (explains %.1f%% of variance)\n", 
            model_summary$r.squared, model_summary$r.squared * 100))
cat(sprintf("Adjusted R-squared: %.4f\n", model_summary$adj.r.squared))
cat(sprintf("F-statistic: %.2f (p-value: %.5f)\n", 
            model_summary$fstatistic[1], 
            pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
               model_summary$fstatistic[3], lower.tail = FALSE)))

# Multicollinearity check
cat("\n=== Multicollinearity Check ===\n")

# Calculate correlation matrix between features
numeric_data <- model_data %>% select(-playlist_genre, -track_popularity)
feature_correlation <- cor(numeric_data)

# Find high correlations (|r| > 0.7)
high_correlations <- which(abs(feature_correlation) > 0.7 & abs(feature_correlation) < 1, arr.ind = TRUE)

if(length(high_correlations) > 0) {
  high_correlations_df <- as.data.frame(high_correlations)
  high_correlations_df$row_name <- rownames(feature_correlation)[high_correlations_df$row]
  high_correlations_df$col_name <- colnames(feature_correlation)[high_correlations_df$col]
  high_correlations_df$correlation <- feature_correlation[cbind(high_correlations_df$row, high_correlations_df$col)]
  
  # Show each pair only once
  high_correlations_df <- high_correlations_df[high_correlations_df$row < high_correlations_df$col, 
                                              c("row_name", "col_name", "correlation")]
  
  if(nrow(high_correlations_df) > 0) {
    cat("High correlation feature pairs found (|r| > 0.7):\n")
    print(high_correlations_df)
    cat("Multicollinearity issues detected!\n\n")
  } else {
    cat("No significant multicollinearity issues detected (no correlations > 0.7).\n\n")
  }
} else {
  cat("No significant multicollinearity issues detected (no correlations > 0.7).\n\n")
}

# Create feature correlation heatmap
png("multicollinearity_heatmap.png", width = 10, height = 8, units = "in", res = 300)
corrplot(feature_correlation, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#A23B72", "white", "#2E86AB"))(200),
         title = "Feature Correlation Matrix (Multicollinearity Check)",
         mar = c(0,0,2,0))
dev.off()

# 9. Genre popularity boxplot
genre_boxplot <- ggplot(model_data, aes(x = reorder(playlist_genre, track_popularity, FUN = median), 
                                       y = track_popularity, 
                                       fill = playlist_genre)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_viridis_d() +
  labs(
    title = "Song Popularity Distribution by Genre",
    subtitle = "Boxplot showing popularity distribution across music genres",
    x = "Music Genre",
    y = "Track Popularity"
  ) +
  custom_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

ggsave("genre_popularity_boxplot.png", genre_boxplot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 10. Final conclusions and recommendations
cat("=== Final Conclusions and Recommendations ===\n")

cat("As a record label agent looking for potential young talent, focus on these three most important features:\n\n")

# Reconfirm top 3 most important positive features
positive_features <- all_coefficients %>%
  filter(estimate > 0, p.value < 0.05) %>%
  arrange(desc(estimate)) %>%
  head(3)

for(i in 1:nrow(positive_features)) {
  feature_name <- positive_features$term[i]
  if(grepl("playlist_genre", feature_name)) {
    display_name <- paste("Music Genre:", gsub("playlist_genre", "", feature_name))
    interpretation <- switch(gsub("playlist_genre", "", feature_name),
                           "pop" = "Pop music has the broadest commercial appeal",
                           "rock" = "Rock music has a stable audience base",
                           "latin" = "Latin music shows strong growth potential",
                           "r&b" = "R&B music performs well in the market",
                           "rap" = "Rap music has strong commercial value",
                           "edm" = "EDM is popular among young audiences")
  } else {
    display_name <- paste("Audio Feature:", feature_name)
    interpretation <- switch(feature_name,
                           "danceability" = "High danceability songs spread easily on social media and at parties",
                           "valence" = "Positive emotion songs are more popular with general audiences",
                           "acousticness" = "Moderate acoustic elements increase song appeal",
                           "loudness" = "Appropriate volume levels help commercial success",
                           "energy" = "Moderate energy levels balance different audience preferences")
  }
  
  cat(sprintf("%d. %s\n", i, display_name))
  cat(sprintf("   Impact coefficient: +%.3f %s\n", positive_features$estimate[i], positive_features$significance[i]))
  cat(sprintf("   Business advice: %s\n\n", interpretation))
}

cat("Generated chart files:\n")
cat("1. audio_features_importance.png - Audio feature importance\n")
cat("2. genre_impact.png - Genre impact analysis\n")
cat("3. top_features_combined.png - Combined feature importance\n")
cat("4. correlation_heatmap.png - Correlation heatmap\n")
cat("5. top_features_scatter.png - Key feature scatter plots\n")
cat("6. multicollinearity_heatmap.png - Multicollinearity check\n")
cat("7. genre_popularity_boxplot.png - Genre popularity distribution\n")

cat("\nAnalysis completed!\n")