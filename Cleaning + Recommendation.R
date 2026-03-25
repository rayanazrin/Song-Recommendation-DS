# 1. Load necessary libraries
library(tidyverse)
library(fastDummies)

# 2. Load the dataset
spotify_data <- read.csv("C:/Users/rayan/Downloads/dsClass/DSassignment/SpotifyFeatures.csv", stringsAsFactors = FALSE)

# 3. Handle Missing Values & Duplicates
spotify_cleaned <- spotify_data %>%
  drop_na(popularity, acousticness, danceability, energy, instrumentalness, 
          liveness, loudness, speechiness, tempo, valence, mode, genre) %>%
  distinct(track_name, artist_name, .keep_all = TRUE)

# 4. Feature Engineering
# Convert 'mode' to binary
# Removed 'key' from the selection/processing entirely
spotify_engineered <- spotify_cleaned %>%
  mutate(mode = ifelse(mode == "Major", 1, 0)) %>%
  # Create dummy columns ONLY for time_signature
  dummy_cols(select_columns = c("time_signature"), 
             remove_selected_columns = TRUE)

# 5. Remove Irrelevant Columns
# We explicitly remove 'key'
spotify_prepared <- spotify_engineered %>%
  select(-key)

# 6. Normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

numeric_cols <- c("popularity", "acousticness", "danceability", "duration_ms", 
                  "energy", "instrumentalness", "liveness", "loudness", 
                  "speechiness", "tempo", "valence")

spotify_final <- spotify_prepared %>%
  mutate(across(all_of(numeric_cols), normalize))

# 7. Preview the cleaned data
head(spotify_final)

# 8. Save the cleaned file
write.csv(spotify_final, "Cleaned_Spotify_Features.csv", row.names = FALSE)




# RECOMMENDATION PAKAI COSINE
library(lsa)

# 1. Select Features
features <- spotify_final[, c("popularity", "acousticness", "danceability",
                              "duration_ms", "energy", "instrumentalness",
                              "liveness", "loudness", "speechiness",
                              "tempo", "valence")]

feature_matrix <- as.matrix(features)

# 2. Recommendation function
recommend_cosine <- function(song_name, artist, data, feature_matrix, top_n = 5,
                             same_genre = FALSE, same_artist = FALSE) {
  song_index <- which(data$track_name == song_name & data$artist_name == artist)
  if (length(song_index) == 0) {
    stop("Song not found")
  }
  
  song_index <- song_index[1]
  target_song <- feature_matrix[song_index, ]
  
  # cosine similarity 
  sim_scores <- apply(feature_matrix, 1, function(x) {
    sum(target_song * x) / (sqrt(sum(target_song^2)) * sqrt(sum(x^2)))
  })
  
 
  
  # ==========================
  # filters
  # ==========================
  
  if (same_genre) {
    target_genre <- data$genre[song_index]
    sim_scores[data$genre != target_genre] <- -1
  }
  
  if (same_artist) {
    target_artist <- data$artist_name[song_index]
    sim_scores[data$artist_name != target_artist] <- -1
  }
  

  
  
  
  # remove itself
  sim_scores[song_index] <- -1
  
  # get top recommendations
  top_indices <- order(sim_scores, decreasing = TRUE)[1:top_n]
  
  result <- data[top_indices, c("track_name", "artist_name", "genre")]
  result$similarity <- round(sim_scores[top_indices], 4)
  
  result <- result %>%
    arrange(desc(similarity))
  
  return(result)
}

# 3. Test
recommend_cosine("One Last Time", "Ariana Grande", spotify_final, feature_matrix,
                 top_n = 20, same_genre = FALSE, same_artist = FALSE)













