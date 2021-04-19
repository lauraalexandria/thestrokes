library(tidyverse)
library(tidytext)
library(wordcloud2)
library(here)

## Fontes: https://www.tidytextmining.com/tidytext.html
## 

## IDEIAS
# 1 - Tentar deixar a nuvem com formato específico;
# 2 - Fazer Análise Fatorial com as palavras;
# 3 - Análise de Sentimentos;
# 4 - Lembrar de dividir por álbuns!!

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

# Proporção de Palavras por Música -----------------------------------------------------

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



