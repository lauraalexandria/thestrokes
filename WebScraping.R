library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(furrr)

# Número de Músicas: ----

read_html("https://www.vagalume.com.br/the-strokes/") %>% 
  html_elements(".nameMusic") %>% html_text() %>% 
  unique() %>% length()

# Obtendo todas as letras de músicas ----

get_lyric <- function(song_link){
  
  # Reading the html page
  lyric <- read_html(paste0("https://www.vagalume.com.br", song_link)) %>% html_elements("#lyrics")
  
  # Creating sep to replace linebreaks with ". "
  dummy <- html_element(read_xml("<doc><span>. </span></doc>"), "span")
  
  # Replacing line-breaks
  xml_add_sibling(html_elements(lyric, "br"), dummy)
  
  res <- lyric %>% html_text()
  
  return(res)
}

get_lyric("/the-strokes/you-only-live-once.html")

get_lyrics_links <- function(artist_link){
  
  # Reading the html page on the artist
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  
  # Obtaining all the musics' links -
  music_name_node <- page %>% html_elements(".nameMusic")
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
songs <- get_lyrics_links("/the-strokes/") %>% slice(26:n()) # repete o top 25;

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

albuns <- data.frame(SName = c("Is This It", "The Modern Age", "Alone, Together", 
                               "Barely Legal", "Someday", "Last Nite", "Hard To Explain",
                               "When It Started", "Trying Your Luck", "Take It Or Leave It",
                               "What Ever Happened?", "Reptilia", "Automatic Stop", 
                               "12:51", "You Talk Way Too Much", "Between Love And Hate",
                               "Meet Me In The Bathroom", "Under Control", "The Way It Is",
                               "The End Has No End", "I Can't Win", "You Only Live Once",
                               "Juicebox", "Heart In A Cage", "Razorblade", 
                               "On The Other Side", "Vision Of Division", "Ask Me Anything",
                               "Electricityscape", "Killing Lies", "Fear Of Sleep", 
                               "15 Minutes", "Ize Of The World", "Evening Sun", "Red Light",
                               "Machu Picchu", "Under Cover Of Darkness", "Two Kinds Of Happiness",
                               "You're So Right", "Taken For a Fool", "Games", "Call Me Back", 
                               "Gratisfaction", "Metabolism", "Life Is Simple In The Moonlight", 
                               "Tap Out", "All The Time", "One Way Trigger", "Welcome To Japan",
                               "80's Comedown Machine", "50/50", "Slow Animals", 
                               "Partners In Crime", "Chances", "Happy Ending", 
                               "Call It Fate, Call It Karma", "Drag Queen", "Oblivious", 
                               "Threat of Joy", "The Adults Are Talking", "Selfless", 
                               "Brooklyn Bridge To Chorus", "Bad Decisions", "Eternal Summer",
                               "At The Door", "Why Are Sunday's So Depressing", 
                               "Not The Same Anymore", "Ode To The Mets"),
                     Album = c(rep("Is This It", 10),
                               rep("Room On Fire", 11),
                               rep("First Impressions of Earth", 14),
                               rep("Angles", 10),
                               rep("Comedown Machine", 11),
                               rep("Future Present Past [EP]", 3),
                               rep("The New Abnormal", 9)))

# Fonte: https://aneisse.com/post/20190210-music-data-scraping/20190210-music-data-scraping/

