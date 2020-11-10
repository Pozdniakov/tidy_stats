library(tidyverse)
tibble(text  = read_lines("https://www.gutenberg.org/files/100/100-0.txt")) %>% 
  filter(text != "") %>% 
  write_csv("data/shakespeare_all.csv")

# Manually annotate

sh <-  read_csv("data/shakespeare_all.csv")

sh %>%
  na.omit() %>% 
  mutate(number = str_count(text, "\\d") == str_length(text),
         title = ifelse(number, str_c(title, text, sep = " "), title),
         number = rep(title[which(number)], c(which(number)[-1], length(number)+1) - which(number)),
         title = ifelse(str_detect(title, "THE SONNETS"), number, title)) %>% 
  filter(str_count(text, "\\d") != str_length(text)) %>% 
  mutate(number = str_detect(title, "THE PASSIONATE PILGRIM") & str_length(text) < 6,
         title = ifelse(number, str_c(title, text, sep = " "), title),
         number = rep(c(NA, title[which(number)]), c(which(number)[1]-1, c(which(number)[-1], length(number)+1) - which(number))),
         title = ifelse(str_detect(title, "THE PASSIONATE PILGRIM"), number, title),
         text = str_remove_all(text, "\\d")) %>% 
  select(title, text) %>% 
  write_csv("data/shakespeare_all.csv")

# unique words ------------------------------------------------------------
library(tidyverse)
library(tidytext)
shakespeare <- read_csv("data/shakespeare_all.csv")
shakespeare %>% 
  mutate(text = str_remove_all(text, "_")) %>% 
  unnest_tokens(words, text, to_lower = FALSE) %>% 
  mutate(remove = ifelse(str_to_upper(words) == words & str_length(words) > 1, FALSE, TRUE)) %>% 
  filter(remove) %>% 
  select(-remove) %>% 
  mutate(words = str_to_lower(words)) %>% 
  count(title, words) %>% 
  pivot_wider(names_from = title, values_from = n) %>% 
  mutate(n_text = length(unique(shakespeare$title)) - rowSums(is.na(.))) %>% 
  pivot_longer(names_to = "title", values_to = "n", `A LOVER’S COMPLAINT`:`VENUS AND ADONIS`) %>% 
  select(title, words, n, n_text) ->
  analysis

analysis %>% 
  group_by(title) %>% 
  mutate(n_words = sum(n, na.rm = TRUE)) %>% 
  filter(n == 1, n_text == 1) %>% 
  count(title, n_words) %>% 
  rename(n_unique_words = n) %>% 
  filter(n_words < 160) %>% 
  ggplot(aes(n_words, n_unique_words, label = title))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = poisson))+
  theme_bw()+
  labs(title = "Shakespeare corpus",
       x = "number of words (log)",
       y = "number of words that are found only in this text (log)",
       caption = "based on text from gutenberg.org")
