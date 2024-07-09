# Libraries ----
pacman::p_load(tidyverse, rollama, tictoc, FactoMineR, factoextra)

# Option ----

# Data ----
text_top3 <- read_rds("MyData/text_top3.rds")

class_it <- function(mon_texte, model = NULL){
  
  mon_texte <- str_c("Texte: ", mon_texte)
  
  sys_1 <- 'Classe le texte en trois catégories, ne répond uniquement que le nom de la [catégorie] selon les modalité suivantes: "Très Positif", "Positif", "Neutre", "Négatif", "Très Négatif".' 
  sys_2 <- 'Ensuite indique si le texte parle du [sujet] "occupation des universités" en répondant par: "Oui" ou "Non".' 
  sys_3 <- 'Ensuite indique si la [position] du texte concercant le sujet "occupation des université": "Pour", "Contre", "Neutre" ou "NA".'
  sys_4 <- 'Indique le [type] de texte selon les catégories suivantes: "Information", "Opinion" ou "Entretien".'
  sys_5 <- 'Indique quelles [universités] sont mentionnées, indique "NA" quand aucune université est mentionnée.' 
  sys_6 <- 'Finalement, indique quels [acteurs] sont mentionnés, écrit "NA" quand personne n\'est mentionné.'
  final <- 'Voici un exemple de réponse: [catégorie]: Positif, [sujet]: Oui, [position]: Neutre, [type]: opinion, [université]: Université de Genève, [acteur]: Roger Federer'
  
  systemmsg <- 
    str_c(sys_1, sys_2, sys_3, sys_4, sys_5, sys_6, final,
          collapse = " ")
  
  test_query <- 
    tribble(
      ~role, ~content,
      "system", systemmsg,
      "user", "Texte: Je te partage mon avis; Aujourdhui je vais très bien. Et je vais à l\'université de Lausanne",
      "assistant", "[catégorie]: Positif, [sujet]: Non, [position]: Neutre, [type]: Opinion, [université]: Université de Lausanne, [acteurs]: NA",
      "user", "Texte: Il faut absolument que l'occupation de l'université de Genève cesse, cet acte est anti-démocratique et empêche le dialogue. Ce sont les parole raporté du Jean Christof, le chef du partie pour la liberté.",
      "assistant", "[catégorie]: Très négatif, [sujet]: Oui, [position]: Contre, [type]: Opinion, [université]: Université de Genève, [acteurs]: Jean Christof",
      "user", mon_texte
    )
  
  result <- query(test_query, model = model)
  
  return(result)
}

# Classification ----
class_top3 <- 
  text_top3 %>% 
  mutate(result = map(text, class_it))

write_rds(class_top3, file = "MyData/class_top3.rds")

# Embedding ----
class_top3 <- 
  class_top3 %>% 
  mutate(text = if_else(text == "", "vide", text))

embed_top3 <- embed_text(class_top3$text)

write_rds(embed_top3, file = "MyData/embed_top3.rds")

# cluster ----
pca_top3 <- PCA(embed_top3)

hcpc_top3 <- HCPC(pca_top3, nb.clust = -1)

# Save ----
## embed_top3
embed_top3 <- hcpc_top3$data.clust
write_rds(embed_top3, file = "MyData/embed_top3.rds")

class_top3$clust <- hcpc_top3$data.clust$clust 
write_rds(class_top3, file = "MyData/class_top3.rds")