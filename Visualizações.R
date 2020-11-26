library(tidyverse)
library(tidytext)
library(wordcloud2)
library(here)

# Pendências
## 1- Dar um jeito de poder escolher a nuvem  a partir da música, mas aí
## vai  ser flex ou shiny??
## Continuar lendo: https://www.tidytextmining.com/tidytext.html
## 

data("stop_words") # Fixando as stopwords

here(source("WebScraping.R"))
letras <- apply(musicas[,3], 1, get_lyric)
letras <- tibble("letras" = tolower(letras[-(1:25)])) # Raspando com todas as letras minusculas;

letras <- letras %>% unnest_tokens(word, letras) %>% # Cada Palavra terá sua própria linha; 
  anti_join(stop_words) %>% # Removendo as stopwords
  count(word) # Contando a frequência das palavras restantes;

wordcloud2(letras)
