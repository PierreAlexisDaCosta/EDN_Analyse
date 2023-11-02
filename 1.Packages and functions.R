# Analyse du critère de rang #########
# Annexe de la contribution CER
# 
# Ce code est accessible à tous. Si vous l'utilisez, merci de citer
# l'ANEMF (Association Nationale des Etudiants en Médecine de France)
# et son créateur Pierre-Alexis Da Costa,
# Chargé de Mission Double-Cursus et Recherche 2022-2024 à l'ANEMF.
#
# Ce code permet d'analyser le critère de rang que nous proposons.
# Il permet aussi de vérifier sa faisabilité.
# 
# Afin de jouer ce code, il est possible d'jouer chaque ligne du code 
# en appuyant sur le bouton run (en haut du script)
# OU
# en appuyant simultanément sur : Commande + Entrée
#
# Le code est divisé en trois parties/script :
# La création des packages et des fonctions qui permettront de simplifier le code
# 

# Charger les packages ####
# Les packages permettent d'installer un ensemble de fonctions indispensables pour l'analyse.
#
# Si les package n'ont pas été installés au préalable, il faudra effectuer la commande suivante :
# install.package("nom du package") et cela pour chaque package
# Exemple = install.packages(readxl).

library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(plyr)
library(magrittr)
library(pbapply)
library(ggplot2)
library(ggforce)
library(purrr)


# Création de fonctions pour l'analyse ####
# Pour la partie 2 Data Wrangling ####
# En plus des fonctions déjà créées dans les packages ci-dessus, il est également 
# Fonctions utilisées dans la partie 2. Data Wrangling 
# (qui signifie traitement/nettoyage des données)

# Cette fonction permet d'extraire le nom, le genre, le DES ainsi que le rang
# de l'étudiant ayant passé les ECN.
extracting_rank_specialty <-
  function(data){
    data %>%
      mutate(
        Nom_name = str_extract(Info, "^[^,]+"),
        Nom_parenthèse = sub("\\d+ ", "", Nom_name),
        Nom = str_replace_all(Nom_parenthèse, "\\(|\\)", ""),
        Rang = str_extract(Info, pattern = "\\d+"), # \\d pour numérique
        Genre = str_extract(Info, "M\\. |Mme"),
        DES = str_extract(Info, "\\w+( \\w+)*(?= (au|à|aux) [^.,]+)")
      ) %>%
      select(-c(Info, Nom_name,  Nom_parenthèse)) 
  }

# De manière similaire à la fonction permet d'extraire la subdivision
# de l'étudiant ayant passé les ECN.
extracting_subdivision <-
  function(data){
    data %>%
      mutate(
        Lieu = str_extract(Info, "(?:à|au CHU|aux)\\s(.*?)(?:\\.)"),
      )  %>%
      select(-Info) 
  }

# Source medshake
  
# Pour la partie 3 Analyse ####

# Fonction pour calculer le rang max d'une spécialité dans une subdivision
rang_max_analyse <- 
  function(data, rang_type) { 
    Rang_médian_utilité <-
      pbapply::pblapply(spécialité_var, FUN = function(group_id){
        
        print(group_id)
        
        data %>%
          filter(DES == group_id) %>%
          group_by(Lieu, Year) %>%
          dplyr::summarize(rang_max = max( {{rang_type}}),
                           nombre_de_poste = dplyr::n()) %>% 
          mutate(DES = group_id)
        
      }) %>%
      setNames(spécialité_var) %>%
      plyr::ldply(.id = "DES") 
  }

# Fonction pour calculer le rang max d'un poste (spécialité + subdivision) ####
fonction_calcul_décile <-
  function(rang_max_data, Année, Accessibilité_nombre, nombre_etudiant_Année){
    pbapply::pblapply(spécialité_var, FUN = function(group_id){
      
      print(group_id)
      
      condition_met <- FALSE
      
      number <-
        rang_max_data %>% 
        filter(DES == group_id) %>% # 
        filter(Year == {{Année}}) %>%
        group_by() %>%
        dplyr::summarize(Critère_A_ans_percentage = 0)
      
      test <-
        rang_max_data %>%
        filter(DES == group_id) %>% # group_id
        filter(Year == {{Année}}) %>%
        mutate(rang_critère = 0)
      
      while(!condition_met) {
        if (number$Critère_A_ans_percentage >= Accessibilité_nombre) { #définir son seuil d'accessibilité
          condition_met <- TRUE
        } else {
          # If the condition is not met, increment the number
          
          test <-
            test %>%
            mutate(rang_critère = rang_critère + (1 / nombre_etudiant_2022)) %>%
            mutate(Critère_A_ans_percentage = ifelse(rang_critère < rang_max, 0, 1))
          
          number <-
            test %>% 
            group_by() %>%
            dplyr::summarize(Critère_A_ans_percentage = round(mean(Critère_A_ans_percentage) * 100,0))
          
        }
      }
      
      test %>% 
        select(DES, rang_critère) %>% 
        dplyr::rename(seuil_pour_avoir_50_accessibilité = rang_critère) %>%
        unique() 
      
    }) %>%
      setNames(spécialité_var) %>%
      plyr::ldply(.id = "DES") %>%
      mutate(decile = 
               round(seuil_pour_avoir_50_accessibilité, 
                     digits = 2))
  }

# Pour la répresantion graphique ####
# Theme pour ggplot2 (package de visualisation) ####
mythemefortop10 <- theme( 
  panel.background = element_rect(fill = "white"),
  axis.line = element_line(color = "grey90"),
  legend.position = "top",
  legend.title = element_blank(),
  strip.background = element_rect(fill = "grey30"),
  strip.text = element_text(colour = "white"),
  text = element_text(face = "bold")
)

# Liste des specialités
des_list <- c(
  "hématologie",
  "oncologie",
  "urologie",
  "pédiatrie",
  "neurologie",
  "médecine cardiovasculaire",
  "anesthésieréanimation",
  "médecine nucléaire",
  "reconstructrice et esthétique",
  "dermatologie et vénéréologie",
  "médecine intensiveréanimation",
  "radiologie et imagerie médicale",
  "néphrologie",
  "ophtalmologie",
  "médecine interne et immunologie clinique",
  "maladies infectieuses et tropicales",
  "gynécologie obstétrique",
  "pneumologie",
  "psychiatrie",
  "médecine générale",
  "chirurgie viscérale et digestive",
  "chirurgie orthopédique et traumatologique",
  "chirurgie maxillofaciale",
  "chirurgie pédiatrique",
  "gynécologie médicale",
  "endocrinologiediabétologienutrition",
  "rhumatologie",
  "chirurgie cervicofaciale",
  "hépatogastroentérologie",
  "anatomie et cytologie pathologiques",
  "gériatrie",
  "neurochirurgie",
  "génétique médicale",
  "chirurgie vasculaire",
  "urgence",
  "médecine légale et expertises médicales",
  "médecine physique et de réadaptation",
  "chirurgie orale",
  "santé publique",
  "chirurgie thoracique et cardiovasculaire",
  "médecine et santé au travail",
  "médecine vasculaire",
  "biologie médicale",
  "allergologie"
)


spécialité_var <- c(
  "hématologie",
  "oncologie",
  "urologie",
  "pédiatrie",
  "neurologie",
  "médecine cardiovasculaire",
  "anesthésieréanimation",
  "médecine nucléaire",
  "reconstructrice et esthétique",
  "dermatologie et vénéréologie",
  "médecine intensiveréanimation",
  "radiologie et imagerie médicale",
  "néphrologie",
  "ophtalmologie",
  "médecine interne et immunologie clinique",
  "maladies infectieuses et tropicales",
  "gynécologie obstétrique",
  "pneumologie",
  "psychiatrie",
  "médecine générale",
  "chirurgie viscérale et digestive",
  "chirurgie orthopédique et traumatologique",
  "chirurgie maxillofaciale",
  "chirurgie pédiatrique",
  "gynécologie médicale",
  "endocrinologiediabétologienutrition",
  "rhumatologie",
  "chirurgie cervicofaciale",
  "hépatogastroentérologie",
  "anatomie et cytologie pathologiques",
  "gériatrie",
  "neurochirurgie",
  "génétique médicale",
  "chirurgie vasculaire",
  "urgence",
  "médecine légale et expertises médicales",
  "médecine physique et de réadaptation",
  "chirurgie orale",
  "santé publique",
  "chirurgie thoracique et cardiovasculaire",
  "médecine et santé au travail",
  "médecine vasculaire",
  "biologie médicale",
  "allergologie"
)


