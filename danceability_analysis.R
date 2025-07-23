# Danceability Analysis: Features that Make Songs More Danceable
# Using regression and correlation analyses

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(GGally)
library(broom)
library(gridExtra)
library(scales)

# Read data
spotify_data <- read_csv("spotify_songs.csv")

# Display basic information about danceability
cat("Danceability Analysis Overview:\n")
cat("Total tracks:", nrow(spotify_data), "\n")
cat("Danceability range:", round(min(spotify_data$danceability), 3), "-", round(max(spotify_data$danceability), 3), "\n")
cat("Mean danceability:", round(mean(spotify_data$danceability), 3), "\n")
cat("Median danceability:", round(median(spotify_data$danceability), 3), "\n\n")

# Select relevant audio features for analysis
audio_features <- c("energy", "valence", "tempo", "loudness", "acousticness", 
                   "instrumentalness", "liveness", "speechiness")

# Create a dataset with danceability and audio features
dance_data <- spotify_data %>%
  select(danceability, all_of(audio_features)) %>%
  na.omit()

cat("Audio features selected for analysis:\n")
cat(paste(audio_features, collapse = ", "), "\n\n")

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

# Calculate correlation matrix
correlation_matrix <- cor(dance_data)
danceability_correlations <- correlation_matrix[1, -1]  # Exclude self-correlation

# Sort correlations by absolute value
sorted_correlations <- sort(abs(danceability_correlations), decreasing = TRUE)
cat("Correlations with Danceability (sorted by strength):\n")
for(i in 1:length(sorted_correlations)) {
  feature <- names(sorted_correlations)[i]
  correlation <- danceability_correlations[feature]
  cat(sprintf("%-15s: %6.3f\n", feature, correlation))
}
cat("\n")

# Create correlation heatmap
png("correlation_heatmap.png", width = 10, height = 8, units = "in", res = 300)
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

# 2. REGRESSION ANALYSIS
cat("=== REGRESSION ANALYSIS ===\n")

# Multiple linear regression
model <- lm(danceability ~ ., data = dance_data)
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
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate)))

cat("Regression Coefficients (sorted by impact):\n")
print(coefficients_df, n = Inf)
cat("\n")

# 3. VISUALIZATIONS

# Scatter plots for top correlations
top_features <- names(head(sorted_correlations, 4))

scatter_plots <- list()
for(i in 1:length(top_features)) {
  feature <- top_features[i]
  correlation <- round(danceability_correlations[feature], 3)
  
  p <- ggplot(dance_data, aes(x = .data[[feature]], y = danceability)) +
    geom_point(alpha = 0.3, color = "#2E86AB", size = 0.8) +
    geom_smooth(method = "lm", color = "#A23B72", linewidth = 1.2, se = TRUE, alpha = 0.2) +
    labs(
      title = paste("Danceability vs", tools::toTitleCase(gsub("_", " ", feature))),
      subtitle = paste("Correlation:", correlation),
      x = tools::toTitleCase(gsub("_", " ", feature)),
      y = "Danceability"
    ) +
    custom_theme +
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10))
  
  scatter_plots[[i]] <- p
}

# Combine scatter plots
combined_scatter <- do.call(grid.arrange, c(scatter_plots, ncol = 2))
ggsave("danceability_scatter_plots.png", combined_scatter, 
       width = 12, height = 10, dpi = 300, bg = "white")

# 4. Feature importance visualization
importance_data <- coefficients_df %>%
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
    title = "Feature Impact on Danceability",
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

ggsave("feature_importance.png", importance_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 5. Model diagnostics
png("model_diagnostics.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model, which = c(1, 2, 3, 5))
dev.off()

# 6. Summary insights
cat("=== KEY INSIGHTS ===\n")
cat("Features that INCREASE danceability:\n")
positive_features <- coefficients_df %>% 
  filter(estimate > 0, p.value < 0.05) %>%
  arrange(desc(estimate))
if(nrow(positive_features) > 0) {
  for(i in 1:nrow(positive_features)) {
    cat(sprintf("- %s (coef: %.3f, p-value: %.3f)\n", 
                positive_features$term[i], 
                positive_features$estimate[i], 
                positive_features$p.value[i]))
  }
} else {
  cat("- No significant positive predictors found\n")
}

cat("\nFeatures that DECREASE danceability:\n")
negative_features <- coefficients_df %>% 
  filter(estimate < 0, p.value < 0.05) %>%
  arrange(estimate)
if(nrow(negative_features) > 0) {
  for(i in 1:nrow(negative_features)) {
    cat(sprintf("- %s (coef: %.3f, p-value: %.3f)\n", 
                negative_features$term[i], 
                negative_features$estimate[i], 
                negative_features$p.value[i]))
  }
} else {
  cat("- No significant negative predictors found\n")
}

cat("\nModel Performance:\n")
cat(sprintf("- Explains %.1f%% of variance in danceability\n", model_summary$r.squared * 100))
cat(sprintf("- Adjusted R-squared: %.3f\n", model_summary$adj.r.squared))

cat("\nGenerated files:\n")
cat("1. correlation_heatmap.png - Correlation matrix of all audio features\n")
cat("2. danceability_scatter_plots.png - Scatter plots of top correlated features\n")
cat("3. feature_importance.png - Regression coefficients visualization\n")
cat("4. model_diagnostics.png - Model diagnostic plots\n")