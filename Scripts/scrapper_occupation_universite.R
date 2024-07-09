# Libraries ----
pacman::p_load(rvest, hayalbaz, tidyverse)

# Options ----
theme_set(theme_bw())

# Data ----
top3 <-
  read_rds("MyData/df_articles.rds") %>% 
  filter(sources %in% c("letemps", "rts", "tdg"))

# Scraper ----
top3 %>% 
  mutate(datetimes = as_date(datetimes)) %>% 
  count(datetimes) %>% 
  ggplot(aes(datetimes, n)) +
  geom_col() +
  scale_x_date(breaks = "days",
               date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90))

# Scraper for le Temps ----
scrap_lt <- function(link_lt, driver){
  
  driver$goto(link_lt)
  
  Sys.sleep(5)
  
  article_content_lt <- 
    driver$get_elements("main article.post-default") %>% 
    html_elements("h1, h3, h4, p") %>% 
    html_text2() %>% 
    .[1:length(.)-1] %>% 
    str_c(collapse="\n")
  
}

# Scraper for RTS ----
scrap_tdg <- function(link_tdg, driver){
  
  driver$goto(link_tdg)
  
  Sys.sleep(5)
  
  article_content_tdg <- 
    driver$get_elements("main article.ArticleContainer_root__a54nW") %>% 
    html_elements("h1, h2.ContentHead_title___XVQC, h3, h4, p.ContentHead_lead____SsS, p.ArticleParagraph_root__lhFZo") %>% 
    html_text2() %>% 
    str_c(collapse = "\n")
  
}

# Scraper for la Tribune de Genève ----
no_child <- function(x){
  html_children(x) %>% length() == 0
}

scrap_rts <- function(link_rts, driver){

  driver$goto(link_rts)
  
  Sys.sleep(5)
  
  article_content_rts <- 
    driver$get_elements("main article") %>% 
    html_elements('h1, h2, h3, p, figcaption, div.article-lead') %>% 
    keep(no_child) %>% 
    .[1:length(.)-1] %>% 
    html_text2() %>% 
    str_c(collapse = "\n")
  
}

# scraping ----
driver <- puppet$new()

driver$view()

text_top3 <- 
  top3 %>% 
  mutate(text = 
           case_when(
             sources == "letemps" ~ map_chr(urls, scrap_lt, driver),
             sources == "tdg" ~ map_chr(urls, scrap_tdg, driver),
             sources == "rts" ~ map_chr(urls, scrap_rts, driver),
             .default = NA
           ))

write_rds(text_top3, file = "MyData/text_top3.rds")