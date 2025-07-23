# 歌曲受欢迎程度预测 - 多元线性回归分析
# Multiple Linear Regression Analysis for Song Popularity Prediction

# 加载必要的库
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(broom)
library(gridExtra)
library(scales)
library(viridis)
library(RColorBrewer)

# 读取数据
spotify_data <- read_csv("spotify_songs.csv")

# 数据预处理
cat("=== 数据概览 ===\n")
cat("总歌曲数:", nrow(spotify_data), "\n")
cat("受欢迎程度范围:", min(spotify_data$track_popularity), "-", max(spotify_data$track_popularity), "\n")
cat("平均受欢迎程度:", round(mean(spotify_data$track_popularity), 2), "\n")
cat("流派类型:", paste(unique(spotify_data$playlist_genre), collapse = ", "), "\n\n")

# 选择用于建模的特征
features <- c("danceability", "energy", "loudness", "speechiness", 
              "acousticness", "instrumentalness", "liveness", "valence", 
              "tempo", "duration_ms", "playlist_genre")

# 创建建模数据集
model_data <- spotify_data %>%
  select(track_popularity, all_of(features)) %>%
  na.omit()

# 转换流派为因子
model_data$playlist_genre <- as.factor(model_data$playlist_genre)

# 自定义主题
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

# 1. 构建多元线性回归模型
cat("=== 多元线性回归分析 ===\n")
model <- lm(track_popularity ~ ., data = model_data)
model_summary <- summary(model)

# 显示模型性能
cat("模型性能指标:\n")
cat("R-squared:", round(model_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
cat("F统计量 p值:", format.pval(pf(model_summary$fstatistic[1], 
                                          model_summary$fstatistic[2], 
                                          model_summary$fstatistic[3], 
                                          lower.tail = FALSE)), "\n\n")

# 2. 提取和分析系数
coefficients_df <- tidy(model) %>%
  filter(!grepl("^playlist_genre", term)) %>%  # 先分析音频特征
  arrange(desc(abs(estimate)))

# 显示音频特征系数
cat("音频特征回归系数 (按影响力排序):\n")
print(coefficients_df, n = Inf)
cat("\n")

# 提取流派系数
genre_coefficients <- tidy(model) %>%
  filter(grepl("^playlist_genre", term)) %>%
  arrange(desc(estimate))

cat("流派回归系数 (按影响力排序):\n")
print(genre_coefficients, n = Inf)
cat("\n")

# 3. 识别最重要的三个特征
# 结合音频特征和流派，找出对受欢迎程度影响最大的正面特征
all_coefficients <- tidy(model) %>%
  filter(term != "(Intercept)", p.value < 0.05) %>%  # 只考虑显著特征
  mutate(
    feature_type = ifelse(grepl("^playlist_genre", term), "Genre", "Audio"),
    clean_name = ifelse(feature_type == "Genre", 
                       gsub("playlist_genre", "", term), 
                       term),
    abs_estimate = abs(estimate)
  ) %>%
  arrange(desc(estimate))  # 按正面影响排序

# 找出最重要的正面特征（对受欢迎程度有积极影响）
top_positive_features <- all_coefficients %>%
  filter(estimate > 0) %>%
  head(3)

cat("=== 最重要的三个提升受欢迎程度的特征 ===\n")
for(i in 1:nrow(top_positive_features)) {
  cat(sprintf("%d. %s (系数: %.3f, p值: %.5f)\n", 
              i,
              top_positive_features$clean_name[i], 
              top_positive_features$estimate[i], 
              top_positive_features$p.value[i]))
}
cat("\n")# 4. 创
建特征重要性可视化
# 准备音频特征数据
audio_importance <- coefficients_df %>%
  filter(term != "(Intercept)", p.value < 0.05) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    color_group = ifelse(estimate > 0, "正面影响", "负面影响"),
    feature_label = case_when(
      term == "danceability" ~ "可舞性",
      term == "energy" ~ "能量",
      term == "loudness" ~ "响度",
      term == "speechiness" ~ "语音性",
      term == "acousticness" ~ "原声性",
      term == "instrumentalness" ~ "器乐性",
      term == "liveness" ~ "现场感",
      term == "valence" ~ "情感效价",
      term == "tempo" ~ "节拍",
      term == "duration_ms" ~ "时长",
      TRUE ~ term
    )
  )

# 音频特征重要性图
audio_plot <- ggplot(audio_importance, aes(x = reorder(feature_label, estimate), 
                                          y = estimate, 
                                          fill = color_group)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = ifelse(audio_importance$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("正面影响" = "#2E86AB", "负面影响" = "#A23B72")) +
  labs(
    title = "音频特征对歌曲受欢迎程度的影响",
    subtitle = "回归系数显示各音频特征的影响力 (仅显示显著特征)",
    x = "音频特征",
    y = "回归系数",
    fill = "影响类型",
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

# 5. 流派影响可视化
genre_impact <- genre_coefficients %>%
  filter(p.value < 0.05) %>%
  mutate(
    genre = case_when(
      grepl("edm", term) ~ "电子舞曲",
      grepl("latin", term) ~ "拉丁音乐", 
      grepl("pop", term) ~ "流行音乐",
      grepl("r&b", term) ~ "节奏布鲁斯",
      grepl("rap", term) ~ "说唱音乐",
      grepl("rock", term) ~ "摇滚音乐",
      TRUE ~ gsub("playlist_genre", "", term)
    ),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    color_group = ifelse(estimate > 0, "正面影响", "负面影响")
  )

# 流派影响图
genre_plot <- ggplot(genre_impact, aes(x = reorder(genre, estimate), 
                                      y = estimate, 
                                      fill = color_group)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = ifelse(genre_impact$estimate > 0, -0.1, 1.1),
            size = 3.5, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("正面影响" = "#2E86AB", "负面影响" = "#A23B72")) +
  labs(
    title = "音乐流派对歌曲受欢迎程度的影响",
    subtitle = "回归系数显示不同流派相对于基准流派的影响",
    x = "音乐流派",
    y = "回归系数",
    fill = "影响类型",
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

# 6. 综合特征重要性图（包含音频特征和流派）
# 合并所有显著的正面特征
combined_features <- all_coefficients %>%
  filter(estimate > 0) %>%
  head(6) %>%  # 取前6个最重要的正面特征
  mutate(
    display_name = case_when(
      clean_name == "danceability" ~ "可舞性",
      clean_name == "acousticness" ~ "原声性",
      clean_name == "loudness" ~ "响度",
      clean_name == "valence" ~ "情感效价",
      clean_name == "pop" ~ "流行音乐",
      clean_name == "rock" ~ "摇滚音乐",
      clean_name == "latin" ~ "拉丁音乐",
      clean_name == "r&b" ~ "节奏布鲁斯",
      TRUE ~ clean_name
    ),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# 综合重要性图
combined_plot <- ggplot(combined_features, aes(x = reorder(display_name, estimate), 
                                              y = estimate, 
                                              fill = feature_type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(estimate, 2), significance)), 
            hjust = -0.1, size = 4, color = "grey30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Audio" = "#2E86AB", "Genre" = "#A23B72"),
                    labels = c("Audio" = "音频特征", "Genre" = "音乐流派")) +
  labs(
    title = "提升歌曲受欢迎程度的最重要特征",
    subtitle = "基于多元线性回归分析的特征重要性排序",
    x = "特征",
    y = "回归系数 (对受欢迎程度的正面影响)",
    fill = "特征类型",
    caption = "*** p<0.001, ** p<0.01, * p<0.05\n数值越大表示对受欢迎程度的正面影响越强"
  ) +
  custom_theme +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.caption = element_text(size = 9, color = "grey60")
  )

ggsave("top_features_for_popularity.png", combined_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")# 7. 散点
图展示最重要特征与受欢迎程度的关系
# 获取最重要的3个音频特征
top_audio_features <- audio_importance %>%
  filter(estimate > 0) %>%
  head(3) %>%
  pull(term)

# 为每个重要特征创建散点图
scatter_plots <- list()
feature_names_cn <- c("danceability" = "可舞性", 
                     "acousticness" = "原声性", 
                     "loudness" = "响度",
                     "valence" = "情感效价")

for(i in 1:length(top_audio_features)) {
  feature <- top_audio_features[i]
  feature_cn <- feature_names_cn[feature]
  
  # 计算相关系数
  correlation <- cor(model_data[[feature]], model_data$track_popularity)
  
  p <- ggplot(model_data, aes(x = .data[[feature]], y = track_popularity)) +
    geom_point(alpha = 0.3, color = "#2E86AB", size = 0.8) +
    geom_smooth(method = "lm", color = "#A23B72", linewidth = 1.2, se = TRUE, alpha = 0.2) +
    labs(
      title = paste("受欢迎程度 vs", feature_cn),
      subtitle = paste("相关系数:", round(correlation, 3)),
      x = feature_cn,
      y = "歌曲受欢迎程度"
    ) +
    custom_theme +
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10))
  
  scatter_plots[[i]] <- p
}

# 合并散点图
combined_scatter <- do.call(grid.arrange, c(scatter_plots, ncol = 2))
ggsave("top_features_scatter_plots.png", combined_scatter, 
       width = 12, height = 10, dpi = 300, bg = "white")

# 8. 模型拟合效果评估
cat("=== 模型拟合效果评估 ===\n")
cat(sprintf("R-squared: %.4f (解释了%.1f%%的受欢迎程度变异)\n", 
            model_summary$r.squared, model_summary$r.squared * 100))
cat(sprintf("调整后R-squared: %.4f\n", model_summary$adj.r.squared))
cat(sprintf("F统计量: %.2f (p值: %.5f)\n", 
            model_summary$fstatistic[1], 
            pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
               model_summary$fstatistic[3], lower.tail = FALSE)))

# 拟合效果评价
if(model_summary$r.squared < 0.3) {
  cat("\n模型拟合效果评价: 较差\n")
  cat("原因分析:\n")
  cat("1. 音频特征和流派只能解释", round(model_summary$r.squared * 100, 1), "%的受欢迎程度变异\n")
  cat("2. 可能缺失的重要因素包括:\n")
  cat("   - 歌词内容和情感表达\n")
  cat("   - 艺术家知名度和粉丝基础\n")
  cat("   - 营销推广和媒体曝光\n")
  cat("   - 发行时机和文化背景\n")
  cat("   - 音乐视频和视觉元素\n")
  cat("   - 社交媒体传播和病毒效应\n")
} else {
  cat("\n模型拟合效果评价: 良好\n")
  cat("艺术可以部分量化的证据:\n")
  cat("1. 某些音频特征确实与受欢迎程度显著相关\n")
  cat("2. 模型能够识别出影响流行度的关键因素\n")
}

# 9. 多重共线性检验
cat("\n=== 多重共线性检验 ===\n")
# 计算音频特征间的相关系数矩阵
numeric_features <- model_data %>% 
  select(-playlist_genre, -track_popularity) %>%
  names()

correlation_matrix <- cor(model_data[, numeric_features])

# 寻找高相关性 (|r| > 0.7)
high_correlations <- which(abs(correlation_matrix) > 0.7 & abs(correlation_matrix) < 1, arr.ind = TRUE)

if(length(high_correlations) > 0) {
  high_corr_df <- data.frame(
    feature1 = rownames(correlation_matrix)[high_correlations[,1]],
    feature2 = colnames(correlation_matrix)[high_correlations[,2]],
    correlation = correlation_matrix[high_correlations]
  )
  # 去除重复对
  high_corr_df <- high_corr_df[high_corr_df$feature1 < high_corr_df$feature2, ]
  
  if(nrow(high_corr_df) > 0) {
    cat("发现高相关性特征对 (|r| > 0.7):\n")
    print(high_corr_df)
    cat("存在多重共线性问题\n")
  } else {
    cat("未发现显著的多重共线性问题 (无相关系数 > 0.7)\n")
  }
} else {
  cat("未发现显著的多重共线性问题 (无相关系数 > 0.7)\n")
}

# 创建相关性热力图
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
         title = "音频特征相关性矩阵",
         mar = c(0,0,2,0))
dev.off()

# 10. 最终结论
cat("\n=== 最终结论：最可能让歌曲受欢迎的三个最重要特征 ===\n")
cat("基于多元线性回归分析，作为唱片公司经纪人寻找有潜力的年轻人才时，\n")
cat("应该重点关注以下三个最重要的特征：\n\n")

for(i in 1:min(3, nrow(top_positive_features))) {
  feature_name <- top_positive_features$clean_name[i]
  coefficient <- top_positive_features$estimate[i]
  p_value <- top_positive_features$p.value[i]
  
  feature_description <- case_when(
    feature_name == "danceability" ~ "可舞性 - 歌曲的节奏感和易于跳舞的程度",
    feature_name == "pop" ~ "流行音乐流派 - 主流流行音乐风格",
    feature_name == "rock" ~ "摇滚音乐流派 - 摇滚音乐风格", 
    feature_name == "latin" ~ "拉丁音乐流派 - 拉丁音乐风格",
    feature_name == "acousticness" ~ "原声性 - 歌曲中原声乐器的比例",
    feature_name == "loudness" ~ "响度 - 歌曲的整体音量水平",
    feature_name == "valence" ~ "情感效价 - 歌曲传达的积极情感程度",
    TRUE ~ feature_name
  )
  
  cat(sprintf("%d. %s\n", i, feature_description))
  cat(sprintf("   影响系数: +%.2f (p < %.3f)\n", coefficient, p_value))
  cat(sprintf("   实际意义: 该特征每增加一个单位，受欢迎程度平均增加%.2f分\n\n", coefficient))
}

cat("生成的可视化文件:\n")
cat("1. audio_features_importance.png - 音频特征重要性\n")
cat("2. genre_impact.png - 流派影响分析\n") 
cat("3. top_features_for_popularity.png - 最重要特征综合排序\n")
cat("4. top_features_scatter_plots.png - 重要特征散点图\n")
cat("5. correlation_heatmap.png - 特征相关性热力图\n")