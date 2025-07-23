# Track Popularity Prediction Analysis
# Multiple linear regression to predict track popularity using audio features and genre

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
# library(car)     # For VIF (multicollinearity) - not available
library(broom)     # For tidy model output
library(gridExtra)
library(scales)
library(viridis)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Display basic information about track popularity
cat("Track Popularity Prediction Analysis:\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Popularity range:", min(spotify_data$track_popularity), "-", max(spotify_data$track_popularity), "\n")
cat("Mean popularity:", round(mean(spotify_data$track_popularity), 2), "\n")
cat("Median popularity:", median(spotify_data$track_popularity), "\n\n")

# Select relevant features for prediction
# Using numeric audio features and playlist genre
features <- c("danceability", "energy", "loudness", "speechiness", 
              "acousticness", "instrumentalness", "liveness", "valence", 
              "tempo", "duration_ms", "playlist_genre")

# Create a dataset with selected features
model_data <- spotify_data %>%
  select(track_popularity, all_of(features)) %>%
  na.omit()

# Convert playlist_genre to factor
model_data$playlist_genre <- as.factor(model_data$playlist_genre)

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

# 1. CORRELATION ANALYSIS
cat("=== CORRELATION ANALYSIS ===\n")

# Calculate correlation matrix for numeric features
numeric_features <- model_data %>%
  select(-playlist_genre, -track_popularity) %>%
  names()

correlation_matrix <- cor(model_data[, c("track_popularity", numeric_features)])
popularity_correlations <- correlation_matrix[1, -1]  # Exclude self-correlation

# Sort correlations by absolute value
sorted_correlations <- sort(abs(popularity_correlations), decreasing = TRUE)
cat("Correlations with Track Popularity (sorted by strength):\n")
for(i in 1:length(sorted_correlations)) {
  feature <- names(sorted_correlations)[i]
  correlation <- popularity_correlations[feature]
  cat(sprintf("%-15s: %6.3f\n", feature, correlation))
}
cat("\n")

# Create correlation heatmap
png("popularity_correlation_heatmap.png", width = 10, height = 8, units = "in", res = 300)
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
         title = "Audio Features Correlation with Track Popularity",
         mar = c(0,0,2,0))
dev.off()

# 2. MULTIPLE LINEAR REGRESSION
cat("=== MULTIPLE LINEAR REGRESSION ANALYSIS ===\n")

# Build the regression model
model <- lm(track_popularity ~ ., data = model_data)
model_summary <- summary(model)

cat("Multiple Linear Regression Results:\n")
cat("R-squared:", round(model_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
cat("F-statistic p-value:", format.pval(pf(model_summary$fstatistic[1], 
                                          model_summary$fstatistic[2], 
                                          model_summary$fstatistic[3], 
                                          lower.tail = FALSE)), "\n\n")

# Extract and display coefficients
coefficients_df <- tidy(model) %>%
  filter(!grepl("^playlist_genre", term)) %>%  # Filter out genre coefficients for now
  arrange(desc(abs(estimate)))

cat("Regression Coefficients for Audio Features (sorted by impact):\n")
print(coefficients_df, n = Inf)
cat("\n")

# Extract genre coefficients
genre_coefficients <- tidy(model) %>%
  filter(grepl("^playlist_genre", term)) %>%
  arrange(desc(estimate))

cat("Regression Coefficients for Playlist Genres (sorted by impact):\n")
print(genre_coefficients, n = Inf)
cat("\n")

# 3. MULTICOLLINEARITY ANALYSIS
cat("=== MULTICOLLINEARITY ANALYSIS ===\n")

# Calculate correlation matrix for numeric features to assess multicollinearity
numeric_data <- model_data %>% select(-playlist_genre, -track_popularity)
feature_correlation <- cor(numeric_data)

# Find high correlations (absolute value > 0.7) as indicator of multicollinearity
high_correlations <- which(abs(feature_correlation) > 0.7 & abs(feature_correlation) < 1, arr.ind = TRUE)
high_correlations <- as.data.frame(high_correlations)

if(nrow(high_correlations) > 0) {
  high_correlations$row_name <- rownames(feature_correlation)[high_correlations$row]
  high_correlations$col_name <- colnames(feature_correlation)[high_correlations$col]
  high_correlations$correlation <- feature_correlation[high_correlations$row, high_correlations$col]
  
  # Filter to show each pair only once
  high_correlations <- high_correlations[high_correlations$row < high_correlations$col, 
                                        c("row_name", "col_name", "correlation")]
  
  cat("High correlations between features (|r| > 0.7):\n")
  print(high_correlations)
  cat("\n")
} else {
  cat("No high correlations (|r| > 0.7) found between features.\n\n")
}

# Create a correlation heatmap for features
png("feature_correlation_heatmap.png", width = 10, height = 8, units = "in", res = 300)
corrplot(feature_correlation, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#2E86AB", "white", "#A23B72"))(200),
         title = "Correlation Matrix Between Audio Features",
         mar = c(0,0,2,0))
dev.off()

cat("Note: High correlation between predictors (|r| > 0.7) indicates potential multicollinearity.\n\n")

# 4. FEATURE IMPORTANCE VISUALIZATION
# Create a dataframe for feature importance
importance_data <- coefficients_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    abs_estimate = abs(estimate),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    color_group = ifelse(estimate > 0, "Positive", "Negative")
  )

# Plot feature importance
importance_plot <- ggplot(importance_data, aes(x = reorder(term, abs_estimate), 
                                              y = estimate, 
                                              fill = color_group)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 3), significance)), 
            hjust = ifelse(importance_data$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#A23B72")) +
  labs(
    title = "Feature Impact on Track Popularity",
    subtitle = "Regression coefficients showing influence of audio features",
    x = "Audio Features",
    y = "Coefficient Value",
    fill = "Effect",
    caption = "*** p<0.001, ** p<0.01, * p<0.05"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

ggsave("popularity_feature_importance.png", importance_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 5. GENRE IMPACT VISUALIZATION
# Create a dataframe for genre impact
genre_impact <- genre_coefficients %>%
  mutate(
    genre = gsub("playlist_genre", "", term),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    color_group = ifelse(estimate > 0, "Positive", "Negative")
  )

# Plot genre impact
genre_plot <- ggplot(genre_impact, aes(x = reorder(genre, estimate), 
                                      y = estimate, 
                                      fill = color_group)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = ifelse(genre_impact$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#A23B72")) +
  labs(
    title = "Impact of Music Genre on Track Popularity",
    subtitle = "Regression coefficients showing influence of playlist genre",
    x = "Music Genre",
    y = "Coefficient Value",
    fill = "Effect",
    caption = "*** p<0.001, ** p<0.01, * p<0.05"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

ggsave("popularity_genre_impact.png", genre_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 6. SCATTER PLOTS FOR TOP FEATURES
# Get top 4 numeric features by correlation strength
top_features <- names(head(sorted_correlations, 4))

scatter_plots <- list()
for(i in 1:length(top_features)) {
  feature <- top_features[i]
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
combined_scatter <- do.call(grid.arrange, c(scatter_plots, ncol = 2))
ggsave("popularity_scatter_plots.png", combined_scatter, 
       width = 12, height = 10, dpi = 300, bg = "white")

# 7. MODEL DIAGNOSTICS
png("popularity_model_diagnostics.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model, which = c(1, 2, 3, 5))
dev.off()

# 8. POPULARITY BY GENRE BOXPLOT
genre_boxplot <- ggplot(model_data, aes(x = reorder(playlist_genre, track_popularity, FUN = median), 
                                       y = track_popularity, 
                                       fill = playlist_genre)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_viridis_d() +
  labs(
    title = "Track Popularity Distribution by Genre",
    subtitle = "Comparing popularity scores across different music genres",
    x = "Playlist Genre",
    y = "Track Popularity"
  ) +
  custom_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

ggsave("popularity_by_genre_boxplot.png", genre_boxplot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 9. SUMMARY INSIGHTS
cat("=== KEY INSIGHTS ===\n")

# Top 3 features that increase popularity
top_positive_features <- coefficients_df %>% 
  filter(estimate > 0, p.value < 0.05) %>%
  arrange(desc(estimate)) %>%
  head(3)

cat("Top 3 features that INCREASE track popularity:\n")
if(nrow(top_positive_features) > 0) {
  for(i in 1:nrow(top_positive_features)) {
    cat(sprintf("%d. %s (coef: %.3f, p-value: %.5f)\n", 
                i,
                top_positive_features$term[i], 
                top_positive_features$estimate[i], 
                top_positive_features$p.value[i]))
  }
} else {
  cat("- No significant positive predictors found\n")
}
cat("\n")

# Top 3 features that decrease popularity
top_negative_features <- coefficients_df %>% 
  filter(estimate < 0, p.value < 0.05) %>%
  arrange(estimate) %>%
  head(3)

cat("Top 3 features that DECREASE track popularity:\n")
if(nrow(top_negative_features) > 0) {
  for(i in 1:nrow(top_negative_features)) {
    cat(sprintf("%d. %s (coef: %.3f, p-value: %.5f)\n", 
                i,
                top_negative_features$term[i], 
                top_negative_features$estimate[i], 
                top_negative_features$p.value[i]))
  }
} else {
  cat("- No significant negative predictors found\n")
}
cat("\n")

# Most popular genres
top_genres <- genre_coefficients %>%
  filter(p.value < 0.05) %>%
  arrange(desc(estimate)) %>%
  head(3)

cat("Most popular music genres:\n")
if(nrow(top_genres) > 0) {
  for(i in 1:nrow(top_genres)) {
    genre_name <- gsub("playlist_genre", "", top_genres$term[i])
    cat(sprintf("%d. %s (coef: %.3f, p-value: %.5f)\n", 
                i,
                genre_name, 
                top_genres$estimate[i], 
                top_genres$p.value[i]))
  }
} else {
  cat("- No significant genre effects found\n")
}
cat("\n")

# Model performance assessment
cat("Model Performance:\n")
cat(sprintf("- R-squared: %.3f (explains %.1f%% of variance in popularity)\n", 
            model_summary$r.squared, model_summary$r.squared * 100))
cat(sprintf("- Adjusted R-squared: %.3f\n", model_summary$adj.r.squared))
cat(sprintf("- F-statistic: %.2f (p-value: %.5f)\n", 
            model_summary$fstatistic[1], 
            pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
               model_summary$fstatistic[3], lower.tail = FALSE)))
cat("\n")

# Multicollinearity assessment
if(exists("high_correlations") && nrow(high_correlations) > 0) {
  cat("Multicollinearity Issues:\n")
  for(i in 1:nrow(high_correlations)) {
    cat(sprintf("- %s and %s: correlation = %.3f\n", 
                high_correlations$row_name[i], 
                high_correlations$col_name[i], 
                high_correlations$correlation[i]))
  }
} else {
  cat("No significant multicollinearity issues detected (no correlations > 0.7).\n")
}
cat("\n")

cat("Generated files:\n")
cat("1. popularity_correlation_heatmap.png - Correlation matrix of audio features with popularity\n")
cat("2. feature_correlation_heatmap.png - Correlation matrix between audio features\n")
cat("3. popularity_feature_importance.png - Feature importance visualization\n")
cat("4. popularity_genre_impact.png - Genre impact on popularity\n")
cat("5. popularity_scatter_plots.png - Scatter plots of top correlated features\n")
cat("6. popularity_model_diagnostics.png - Model diagnostic plots\n")
cat("7. popularity_by_genre_boxplot.png - Popularity distribution by genre\n")

# 10. ANSWERS TO SPECIFIC QUESTIONS
cat("\n=== ANSWERS TO SPECIFIC QUESTIONS ===\n")

cat("a. Top three important features for song popularity (for a label company):\n")
# This will be filled based on the analysis results

cat("\nb. Goodness of fit assessment:\n")
# This will be filled based on the analysis results

cat("\nc. Multicollinearity evaluation:\n")
# This will be filled based on the analysis results