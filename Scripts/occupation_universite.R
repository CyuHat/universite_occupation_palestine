# Libraries ----
pacman::p_load(tidyverse, rvest, hayalbaz, htmltools, xml2)

# Data ----
link <- "https://news.google.com/search?q=universit%C3%A9%20AND%20occupation%20before%3A2024-05-22&hl=fr&gl=CH&ceid=CH%3Afr"
brw <- puppet$new(link)
  
brw$view()
  
articles <- brw$get_elements("article")

brw$close()

articles

# Journal
(
sources <- 
  articles %>% 
  html_element("div.IL9Cne div.oovtQ img") %>% 
  html_attr("src") %>%
  str_extract("\\/\\/(www\\.|wap\\.)?\\w{1,15}\\.ch") %>% 
  str_remove_all("\\/|www|\\.(ch)?")
)

# Titre
(
  titres <-
  articles %>% 
  html_element("div.IL9Cne a") %>% 
  html_text2()
)

# Datetime
(
  datetimes <- 
    articles %>% 
    html_element("time") %>% 
    html_attr("datetime") %>% 
    lubridate::as_datetime()
)

(
  urls <- 
    articles %>% 
    html_element("a.WwrzSb") %>% 
    html_attr("href") %>% 
    url_absolute("https://news.google.com/")
)

(
  auteur <- 
    articles %>% 
    html_element("div.bInasb span.PJK1m") %>% 
    html_text2() %>% 
    str_remove_all("^Par ")
)

# Proportion
tibble(src = sources) %>% 
  count(src, sort = TRUE) %>% 
  mutate(p = (n/sum(n))*100,
         p = str_c(p, "%"))

df_articles <- 
  tibble(sources = sources,
         titres = titres,
         datetimes = datetimes,
         auteur = auteur,
         urls = urls)

write_rds(df_articles, file = "MyData/df_articles.rds")
# faviconV2 = Le temps (13.6%)
# faviconV2_034 = La Tribune de Genève (10%)
# faviconV2_022 =  Le Blick(8.57%)
# Total = 32.87%

# remotes::install_github("rstudio/chromote")
# remotes::install_github("rundel/hayalbaz")
