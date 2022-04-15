library(spotifyr)
library(tidyverse)

# Primeiro acesso ---------------------------------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = 'd0720cf2f83c4e6ba6bcae8d7ca94c35')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '9c097b4e8a7f4032934c8bdf1a105c13')

access_token <- get_spotify_access_token()

# Bancos de Dados ---------------------------------------------------------------------

teste <- get_artist_audio_features("The Strokes") %>% 
  select(track_id, track_name, artist_id, artist_name, 
         album_name, danceability, energy, loudness, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo)
