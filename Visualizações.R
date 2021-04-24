library(tidyverse)
library(tidytext)
library(wordcloud2)
library(textdata) # contem os dados para a análise de sentimento
library(here)
library(echarts4r)

## Fontes: https://www.tidytextmining.com/tidytext.html
## 

## IDEIAS
# 1 - Tentar deixar a nuvem com formato específico;
# 2 - Fazer Análise Fatorial com as palavras;
# 3 - Análise de Sentimentos;
# 4 - Lembrar de dividir por álbuns!!

## OBSERVAÇÕES
# 1 - Revisar as músicas do top3 de sadness
# 2 - considero yeah, hey e gonna como stopwords?

data("stop_words") # Fixando as stopwords

# Carregando os Dados ------------------------------------------------------------------

here(source("WebScraping.R"))
letras <- apply(musicas[,3], 1, get_lyric)
musicas <- musicas %>% 
  mutate(letras = tolower(letras)) # Adicionando com todas as letras minusculas;

# Nuvem de Palavras --------------------------------------------------------------------

contagem <- musicas %>% select(c(2,4)) %>% 
  unnest_tokens(word, letras) %>% # Cada Palavra terá sua própria linha; 
  anti_join(stop_words) %>% # Removendo as stopwords
  count(word) # Contando a frequência das palavras restantes;

wordcloud2(contagem)
ggplot(contagem, aes(x = n)) + geom_histogram()
wordcloud2(contagem %>% filter(n > 10))
wordcloud2(contagem %>% filter(n > 5))

# Distribuição do número de  Palavras por música -------------------------------------------

num_por_musica <- musicas %>% select(c(2,4)) %>% 
  unnest_tokens(word, letras) %>%
  anti_join(stop_words) %>%
  group_by(SName) %>% 
  count()

ggplot(num_por_musica, aes(n)) + geom_histogram(color = "white") + theme_minimal()

num_por_musica %>% ungroup() %>% 
  e_charts() %>%
  e_histogram(n) %>% 
  e_tooltip(trigger = "axis") %>% 
  e_theme("sakura")

menor <- num_por_musica %>% arrange(n) %>% top_n(1)
maior <- num_por_musica %>% arrange(desc(n)) %>% top_n(1)

# Proporção de cada palavra por Música -----------------------------------------------------

count(contagem) # 1489 palavras presentes

prop_por_musica <- musicas %>% select(c(2,4)) %>% 
  unnest_tokens(word, letras) %>%
  anti_join(stop_words) %>%
  group_by(SName, word) %>% 
  count() %>% 
  ungroup() %>% group_by(SName) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-3) %>% 
  pivot_wider(1:3, names_from = word, values_from = prop, values_fill = 0)

top_10 <- contagem %>% arrange(desc(n)) %>% slice(1:10) %>% pull(word)

prop_por_musica %>% 
  select(1, top_10) %>% 
  mutate(var1 = sum(time:gonna),
         var2 = sum(life:friends)) %>% 
  ggplot(aes(x = var1, y = var2)) +
  geom_point()

# Duas palavras mais frequentes: gonna e live

ggplot(prop_por_musica, aes(x = gonna, y = live)) +
  geom_point()

# Análise de Sentimentos -------------------------------------------------------------------

## Mas não pode esquecer que algumas palavras tem mais de uma classificação e são repetidas

sentiments <- get_sentiments("nrc")

musicas_e_sent <- musicas %>% select(c(2,4)) %>% 
  unnest_tokens(word, letras) %>%
  anti_join(stop_words) %>% inner_join(sentiments) %>% 
  mutate(sentiment = str_to_sentence(sentiment))

## Sentimentos mais comuns entre todas as músicas
musicas_e_sent %>% 
  group_by(sentiment) %>% 
  count() %>% ungroup() %>% 
  e_charts(sentiment) %>% 
  e_bar(n, name = "Frequency", legend = F) %>% 
  e_flip_coords() %>% 
  e_tooltip() %>% 
  e_labels(position = "right")

## Sentimentos para uma música escolhida
sent_por_mus <- musicas_e_sent %>% 
  group_by(SName, sentiment) %>% count() %>% 
  ungroup() %>% group_by(SName) %>% 
  mutate(prop = n/sum(n)) %>% ungroup()

input_musica <- "Barely Legal"

sent_por_mus %>% 
  filter(SName == input_musica) %>% 
  arrange(prop) %>% 
  e_charts(sentiment) %>% 
  e_polar() %>% 
  e_angle_axis() %>% 
  e_radius_axis(sentiment) %>% 
  e_bar(prop, coord_system = "polar")
  
sent_por_mus %>% 
  filter(SName == input_musica) %>%
  e_charts(sentiment) %>% 
  e_pie(prop, legend = F)  
  
sent_por_mus %>% 
  filter(SName == input_musica) %>%
  e_charts(sentiment) %>% 
  e_pie(prop, legend = F, radius = c("45%", "70%"))  %>%
  e_text_g(right = 185, top = 175,
    style = list(text = input_musica, fontSize = 20, fontFamily = 'monospace'))

# Na verdade mesmo queria fazer uma treemap, né..

## Músicas mais tristes, alegres ou tanto faz

input_sentimento <- "Anger"
input_sentimento <- "Positive"
input_sentimento <- "Sadness"

top_3 <- sent_por_mus %>% 
  filter(sentiment == input_sentimento) %>% 
  slice_max(prop, n = 3)
  
e_charts() %>% # O outro modelo é mais bonitinho
  e_gauge(as.numeric((top_3[1,4]*100) %>% round(2)), 
          paste0(top_3[1,1], "\n", input_sentimento))

e_charts() %>% 
  e_gauge(as.numeric((top_3[2,4]*100) %>% round(2)), 
          paste0(top_3[2,1], "\n", input_sentimento)) 

e_charts() %>% 
  e_gauge(as.numeric((top_3[3,4]*100) %>% round(2)), 
          paste0(top_3[3,1], "\n", input_sentimento)) 

## Relacionando score de negatividade e frequência

afinn <- get_sentiments("afinn")

sent_num_por_mus <- musicas %>% select(c(2,4)) %>% 
  unnest_tokens(word, letras) %>%
  anti_join(stop_words) %>%
  group_by(word) %>% 
  count() %>% ungroup() %>% 
  inner_join(afinn) %>% 
  filter(word != "yeah")

sent_num_por_mus %>% 
  e_charts() %>% 
  e_histogram(n) 

sent_num_por_mus %>% 
  e_charts(value) %>% 
  e_scatter(n, name = "Frequency") %>% 
  e_tooltip()



