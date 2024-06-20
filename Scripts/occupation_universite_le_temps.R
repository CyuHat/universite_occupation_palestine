# Libraries ----
pacman::p_load(tidyverse, rvest, hayalbaz)

# Option ----
theme_set(theme_bw())

table

# Data ---- 
df <- read_rds("MyData/df_articles.rds")

# Quel journal a écrit le plus d'article sur le sujet
count(df, sources, sort = TRUE) %>% 
  mutate(prop = n/sum(n))

# Titre des journaux
df %>% 
  filter(sources == "letemps") %>% 
  select(titres)

# Le nombre d'article dans le temps
df %>% 
  filter(sources %in% c("letemps", "rts", "tdg")) %>% 
  mutate(date = as_date(datetimes)) %>% 
  count(date, sources) %>% 
  ggplot(aes(date, n, fill = sources)) +
  geom_col() +
  scale_x_date(breaks = "days",
               date_labels = "%b %d") +
  labs(title = "Les Articles sortis dans le temps",
       subtitle = "Sujet: occupation unniversitaire (23 avril - 20 mai 2024)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Type de titre que nous avons pour chaque article
df %>% 
  filter(sources %in% c("letemps", "rts", "tdg")) %>% 
  View()
