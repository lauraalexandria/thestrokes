library(spotifyr)
library(tidyverse)

# Primeiro acesso ---------------------------------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = 'd0720cf2f83c4e6ba6bcae8d7ca94c35')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '9c097b4e8a7f4032934c8bdf1a105c13')

access_token <- get_spotify_access_token()

# Bancos de Dados ---------------------------------------------------------------------

dados_spotify <- get_artist_audio_features(
  "The Strokes",
  include_groups = c("album", "single") # Tanto as músicas de álbuns quanto de eps;
  ) %>% 
  select(track_name, album_name, danceability, energy, loudness, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo) %>% 
  distinct(track_name, .keep_all = T) %>% 
  mutate(
    track_name = str_remove_all(track_name, '"'), # Remover aspas dos títulos para evitar problemas
    track_name = str_to_title(track_name),        # Padronizar os títulos;
    track_name = str_replace(track_name, "&", "And")
    ) 
