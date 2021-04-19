library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(furrr)

# Número de Músicas: ----

read_html("https://www.vagalume.com.br/the-strokes/") %>% 
  html_nodes(".nameMusic") %>% html_text() %>% 
  unique() %>% length()

# Obtendo todas as letras de músicas ----

get_lyric <- function(song_link){
  
  # Reading the html page
  lyric <- read_html(paste0("https://www.vagalume.com.br", song_link)) %>% html_nodes("#lyrics")
  
  # Creating sep to replace linebreaks with ". "
  dummy <- xml_node(read_xml("<doc><span>. </span></doc>"), "span")
  
  # Replacing line-breaks
  xml_add_sibling(xml_nodes(lyric, "br"), dummy)
  
  res <- lyric %>% html_text()
  
  return(res)
}

get_lyric("/the-strokes/you-only-live-once.html")

get_lyrics_links <- function(artist_link){
  
  # Reading the html page on the artist
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  
  # Obtaining all the musics' links -
  music_name_node <- page %>% html_nodes(".nameMusic")
  music_names <- music_name_node %>% html_text()
  music_links <- music_name_node %>% html_attr("href")
  
  # Building final tibble
  res <- tibble(ALink = rep(artist_link, length(music_names)),
                SName = music_names,
                SLink = music_links)
  
  return(res)
}


# Returning all the links to the musics
plan(multisession)
musicas <- get_lyrics_links("/the-strokes/")

# Tentei 

albums <- function (name, message = TRUE) {
  name <- stringr::str_to_lower(name)
  cont <- paste0("https://www.vagalume.com.br/", name, "/index.js") %>% 
    jsonlite::fromJSON()
  if (!is.null(cont)) {
    albums <- data.frame(cont$artist$album$item, stringsAsFactors = FALSE)[,-3] 
    names(albums)[2] <- "title"
  }
  else {
    albums <- NULL
    if (message) 
      print("Artist not found.")
  }
  return(albums)
}

# Fonte: https://aneisse.com/post/2019-02-10-music-data-scraping/2019-02-10-music-data-scraping/
