# Verify Dataset Statistics
library(dplyr)
library(readr)

# Read the CSV file
spotify_data <- read_csv("spotify_songs.csv")

cat("=== DATASET STATISTICS VERIFICATION ===\n\n")

# 1. Total number of songs
total_songs <- nrow(spotify_data)
cat("1. Total songs:", format(total_songs, big.mark = ","), "\n")

# 2. Number of distinct playlists
# Check both playlist_name and playlist_id for distinct playlists
distinct_playlist_names <- spotify_data %>% 
  distinct(playlist_name) %>% 
  nrow()

distinct_playlist_ids <- spotify_data %>% 
  distinct(playlist_id) %>% 
  nrow()

cat("2. Distinct playlists:\n")
cat("   - By playlist_name:", format(distinct_playlist_names, big.mark = ","), "\n")
cat("   - By playlist_id:", format(distinct_playlist_ids, big.mark = ","), "\n")

# 3. Number of distinct artists
distinct_artists <- spotify_data %>% 
  distinct(track_artist) %>% 
  nrow()

cat("3. Distinct artists:", format(distinct_artists, big.mark = ","), "\n")

# Additional useful statistics
cat("\n=== ADDITIONAL STATISTICS ===\n")

# Distinct albums
distinct_albums <- spotify_data %>% 
  distinct(track_album_name) %>% 
  nrow()
cat("Distinct albums:", format(distinct_albums, big.mark = ","), "\n")

# Distinct tracks (by track_name)
distinct_track_names <- spotify_data %>% 
  distinct(track_name) %>% 
  nrow()
cat("Distinct track names:", format(distinct_track_names, big.mark = ","), "\n")

# Distinct track IDs
distinct_track_ids <- spotify_data %>% 
  distinct(track_id) %>% 
  nrow()
cat("Distinct track IDs:", format(distinct_track_ids, big.mark = ","), "\n")

# Genres
distinct_genres <- spotify_data %>% 
  distinct(playlist_genre) %>% 
  nrow()
cat("Distinct genres:", format(distinct_genres, big.mark = ","), "\n")

# Subgenres
distinct_subgenres <- spotify_data %>% 
  distinct(playlist_subgenre) %>% 
  nrow()
cat("Distinct subgenres:", format(distinct_subgenres, big.mark = ","), "\n")

# Year range
spotify_data <- spotify_data %>%
  mutate(release_year = as.numeric(substr(track_album_release_date, 1, 4)))

year_range <- spotify_data %>%
  summarise(
    min_year = min(release_year, na.rm = TRUE),
    max_year = max(release_year, na.rm = TRUE)
  )

cat("Year range:", year_range$min_year, "to", year_range$max_year, "\n")

cat("\n=== VERIFICATION RESULTS ===\n")
cat("Your stated statistics:\n")
cat("- 32,833 songs: ", ifelse(total_songs == 32833, "✓ CORRECT", "✗ INCORRECT"), "\n")
cat("- 449 distinct playlists: ", ifelse(distinct_playlist_names == 449 || distinct_playlist_ids == 449, "✓ CORRECT", "✗ INCORRECT"), "\n")
cat("- 9,693 distinct artists: ", ifelse(distinct_artists == 9693, "✓ CORRECT", "✗ INCORRECT"), "\n")

cat("\nActual numbers from the dataset:\n")
cat("- Songs:", format(total_songs, big.mark = ","), "\n")
cat("- Playlists (by name):", format(distinct_playlist_names, big.mark = ","), "\n")
cat("- Playlists (by ID):", format(distinct_playlist_ids, big.mark = ","), "\n")
cat("- Artists:", format(distinct_artists, big.mark = ","), "\n")

# Show some examples of the data structure
cat("\n=== SAMPLE DATA ===\n")
cat("First few playlist names:\n")
sample_playlists <- spotify_data %>% 
  distinct(playlist_name) %>% 
  head(10) %>% 
  pull(playlist_name)
for(i in 1:length(sample_playlists)) {
  cat(i, ".", sample_playlists[i], "\n")
}

cat("\nFirst few artist names:\n")
sample_artists <- spotify_data %>% 
  distinct(track_artist) %>% 
  head(10) %>% 
  pull(track_artist)
for(i in 1:length(sample_artists)) {
  cat(i, ".", sample_artists[i], "\n")
}

cat("\nVerification completed!\n")