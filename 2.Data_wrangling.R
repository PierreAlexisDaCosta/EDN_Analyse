# Cette seconde partie du code sert à extraire les données 
# des arrêtés d'affectations issus Legifrance.
#
# Une étape préliminaire à cette partie consiste à créer 
# un tableau excel avec une colonne avec un seule colonne nommée "Info" 
# (je l'ai nommé de cette manière mais nous pourrions l'appeler autrement)
#
#
# Cette partie est assez donc répétitive puisque j'extrais les données pour chaque ECN
# Puis je les fusionne dans un format long, qui me permettra de procéder à l'analyse 
# dans la troisième partie de ce code.
#
# Si vous souhaitez voir un dataframe, vous pouvez utiliser utiliser la fonction View
# Exemple View(nombre_etudiant_EN) peremttra de visualiser le tableau dans l'interface R.

# Nombre d'étudiant ####
# Avant de charger les données issues des ECN, nous avons besoin du nombre 
# étudiants qui ont été classés aux ECN.
# (source : medshake)

nombre_etudiant_2023 <- 9754 # C'est le nombre d'étudiant 
nombre_etudiant_2022 <- 9298
nombre_etudiant_2021 <- 9032
nombre_etudiant_2020 <- 8820
nombre_etudiant_2019 <- 8728
nombre_etudiant_2018 <- 8706
nombre_etudiant_2017 <- 8372 # Les données des ECN 2017 ont également été traitées mais n'ont pas utilisées par la suite
nombre_etudiant_EN <-
  data.frame(Année = c("2023", "2022", "2021", 
                       "2020", "2019", "2018"),
             Etudiants_classés = c("9754", "9298", "9032", 
                                   "8820", "8728", "8706")) %>%
  mutate_at(vars(Etudiants_classés), as.numeric)

# View(nombre_etudiant_EN) pour voir le tableau dans R

# EN signifie épreuves nationales 

# Visualisation graphique du nombre d'étudiant classés à l'ECN en fonction de l'année
plot_nombre_EN <-
  nombre_etudiant_EN %>%
  ggplot2::ggplot(aes(x = Année, 
             y = Etudiants_classés, group = 1)) + # j'indique quelles colonnes mettre dans les axes des abscisses et des ordonnées
  ggplot2::geom_point() +
  ggplot2::geom_line(color = "grey50", alpha = 0.6) +
  ggplot2::geom_text(aes(label = Etudiants_classés), 
                     hjust = -0.1, vjust = -1) + # Adjust hjust and vjust as needed
  ggplot2::ylim(1, (max(nombre_etudiant_EN$Etudiants_classés) + 250) ) +
  ylab("Nombre d'étudiants classés") +
  ggtitle("Evolution du nombre d'étudiants classés \n aux épreuves nationales") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  mythemefortop10 

plot_nombre_EN # pour voir le graphique (aussi appelé plot)

# Les deux lignes suivantes servent à enregistrer le graphique dans un fichier
# au format png.
# Il est possible de l'enregistrer au format pdf en indiquant l'extension
# .pdf à la fin du chemin d'accès "./"
# Il est également possible de modifier la taille et la hauteur (avec width et height).
ggsave(plot = plot_nombre_EN, 
       "./Results/plot_nombre_ECN.png", width = 6, height = 5) 

# Importation des données des ECN 2017 à 2023 ####
# EN 2023 #### 

# Source : Arrêté du 10 octobre 2023 relatif à l'affectation des étudiants 
# et des internes en médecine ayant satisfait aux épreuves classantes nationales 
# anonymes donnant accès au troisième cycle des études de médecine 
#organisées au titre de l'année universitaire 2023-2024

#J'importe le tableau des EN 2023 dans R
EN_2023 <-
  readxl::read_excel("./Raw_data/2023_Raw.xlsx") 

EN_2023$Info <- 
  str_replace_all(EN_2023$Info, "-", "")

# J'extrais le rang et la spécialité avec la fonction créée dans la première 
# partie du code
rank_specialty_EN_2023 <-
  extracting_rank_specialty(EN_2023)

EN_2023$Info <- 
  str_replace_all(EN_2023$Info, " \\(", "") #it's because of the 

# J'exporte la subdivision avec la fonction créée dans la première 
# partie du code
subdivision_EN_2023 <-
  extracting_subdivision(EN_2023)

#Je fusionne les colonnes des deux dataframe contenant le rang, la spécialité
# et la subdivision.
EN_2023_extracted <-
  rank_specialty_EN_2023 %>%
  cbind(subdivision_EN_2023) 

# Je créé des variables "propres" pour le code pour la colonne Lieu (=Subdivision)
EN_2023_extracted$Lieu <- 
  str_replace_all(EN_2023_extracted$Lieu, "au ", "")

EN_2023_extracted$Lieu <- 
  str_replace_all(EN_2023_extracted$Lieu, "à l'", "")

EN_2023_extracted$Lieu <- 
  str_replace_all(EN_2023_extracted$Lieu, "aux ", "")

EN_2023_extracted$Lieu <- 
  str_replace_all(EN_2023_extracted$Lieu,"\\.", "")

EN_2023_extracted$Rang <-
  as.numeric(EN_2023_extracted$Rang)

# Une fois les données extraites et mises au propre,
# j'indexe le rang de l'étudiant par le nombre d'étudiant classé.
# Pour ce faire, je créée une nouvelle colonne avec la fonction mutate()
# puis je divise le rang de l'étudiant par le nombre d'étudiant.
# Ici cela donne 1/9754, 2/9754, 3/9754 ... 9754/9754.
EN_2023_extracted <-
  EN_2023_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2023)

#EN_2022 ####
# Je réitère la même chose que pour les EN 2023 sur les EN 2022

# source : Arrêté du 13 octobre 2022 relatif à l'affectation des étudiants et 
# des internes en médecine ayant satisfait aux épreuves classantes nationales anonymes 
# donnant accès au troisième cycle des études de médecine organisées au titre de 
# l'année universitaire 2022-2023

EN_2022 <-
  readxl::read_excel("./Raw_data/2022_Raw.xlsx")

EN_2022$Info <- 
  str_replace_all(EN_2022$Info, "-", "")

rank_specialty_EN_2022 <-
  extracting_rank_specialty(EN_2022) #functions created

EN_2022$Info <- 
  str_replace_all(EN_2022$Info, " \\(", "") #it's because of the 

subdivision_EN_2022 <-
  extracting_subdivision(EN_2022)

EN_2022_extracted <-
  rank_specialty_EN_2022 %>%
  cbind(subdivision_EN_2022) 

EN_2022_extracted$Lieu <- 
  str_replace_all(EN_2022_extracted$Lieu, "au ", "")

EN_2022_extracted$Lieu <- 
  str_replace_all(EN_2022_extracted$Lieu, "à l'", "")

EN_2022_extracted$Lieu <- 
  str_replace_all(EN_2022_extracted$Lieu, "aux ", "")

EN_2022_extracted$Lieu <- 
  str_replace_all(EN_2022_extracted$Lieu,"\\.", "")

EN_2022_extracted$Rang <-
  as.numeric(EN_2022_extracted$Rang)

EN_2022_extracted <-
  EN_2022_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2022)

# EN 2021 ####
# Arrêté du 14 octobre 2021 relatif à l'affectation des étudiants et des 
# internes en médecine ayant satisfait aux épreuves classantes nationales anonymes
# donnant accès au troisième cycle des études de médecine organisées au titre de
# l'année universitaire 2021-2022
EN_2021 <-
  readxl::read_excel("./Raw_data/2021_Raw.xlsx")

EN_2021$Info <- 
  str_replace_all(EN_2021$Info, "-", "")

rank_specialty_EN_2021 <-
  extracting_rank_specialty(EN_2021) #functions created

EN_2021$Info <- 
  str_replace_all(EN_2021$Info, " \\(", "") #it's because of the 

subdivision_EN_2021 <-
  extracting_subdivision(EN_2021)

EN_2021_extracted <-
  rank_specialty_EN_2021 %>%
  cbind(subdivision_EN_2021) 

EN_2021_extracted$Lieu <- 
  str_replace_all(EN_2021_extracted$Lieu, "au ", "")

EN_2021_extracted$Lieu <- 
  str_replace_all(EN_2021_extracted$Lieu, "à l'", "")

EN_2021_extracted$Lieu <- 
  str_replace_all(EN_2021_extracted$Lieu, "aux ", "")

EN_2021_extracted$Lieu <- 
  str_replace_all(EN_2021_extracted$Lieu,"\\.", "")

EN_2021_extracted$Rang <-
  as.numeric(EN_2021_extracted$Rang)

EN_2021_extracted <-
  EN_2021_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2021)


# EN 2020 ####
# Arrêté du 28 septembre 2020 relatif à l'affectation des étudiants et des 
# internes en médecine ayant satisfait aux épreuves classantes nationales anonymes
# donnant accès au troisième cycle des études médicales organisées au titre de 
# l'année universitaire 2020-2021
EN_2020 <-
  readxl::read_excel("./Raw_data/2020_Raw.xlsx")

EN_2020$Info <- 
  str_replace_all(EN_2020$Info, "-", "")

rank_specialty_EN_2020 <-
  extracting_rank_specialty(EN_2020) #functions created

EN_2020$Info <- 
  str_replace_all(EN_2020$Info, " \\(", "") #it's because of the 

subdivision_EN_2020 <-
  extracting_subdivision(EN_2020)

EN_2020_extracted <-
  rank_specialty_EN_2020 %>%
  cbind(subdivision_EN_2020) 

EN_2020_extracted$Lieu <- 
  str_replace_all(EN_2020_extracted$Lieu, "au ", "")

EN_2020_extracted$Lieu <- 
  str_replace_all(EN_2020_extracted$Lieu, "à l'", "")

EN_2020_extracted$Lieu <- 
  str_replace_all(EN_2020_extracted$Lieu, "aux ", "")

EN_2020_extracted$Lieu <- 
  str_replace_all(EN_2020_extracted$Lieu,"\\.", "")

EN_2020_extracted$Rang <-
  as.numeric(EN_2020_extracted$Rang)
        
EN_2020_extracted <-
  EN_2020_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2020)


# EN 2019 ####
# Arrêté du 4 octobre 2019 relatif à l'affectation des étudiants et des 
# internes en médecine ayant satisfait aux épreuves classantes nationales anonymes
# donnant accès au troisième cycle des études médicales organisées au titre de 
# l'année universitaire 2019-2020
EN_2019 <-
  readxl::read_excel("./Raw_data/2019_Raw.xlsx")

EN_2019$Info <- 
  str_replace_all(EN_2019$Info, "-", "")

rank_specialty_EN_2019 <-
  extracting_rank_specialty(EN_2019) #functions created

EN_2019$Info <- 
  str_replace_all(EN_2019$Info, " \\(", "") #it's because of the 

subdivision_EN_2019 <-
  extracting_subdivision(EN_2019)

EN_2019_extracted <-
  rank_specialty_EN_2019 %>%
  cbind(subdivision_EN_2019) 

EN_2019_extracted$Lieu <- 
  str_replace_all(EN_2019_extracted$Lieu, "au ", "")

EN_2019_extracted$Lieu <- 
  str_replace_all(EN_2019_extracted$Lieu, "à l'", "")

EN_2019_extracted$Lieu <- 
  str_replace_all(EN_2019_extracted$Lieu, "aux ", "")

EN_2019_extracted$Lieu <- 
  str_replace_all(EN_2019_extracted$Lieu,"\\.", "")

EN_2019_extracted$Rang <-
  as.numeric(EN_2019_extracted$Rang)

EN_2019_extracted <-
  EN_2019_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2019)

#EN_2018 ####
EN_2018 <-
  readxl::read_excel("./Raw_data/2018_Raw.xlsx")

EN_2018$Info <- 
  str_replace_all(EN_2018$Info, "-", "")

rank_specialty_EN_2018 <-
  extracting_rank_specialty(EN_2018) #functions created

EN_2018$Info <- 
  str_replace_all(EN_2018$Info, " \\(", "") #it's because of the 

subdivision_EN_2018 <-
  extracting_subdivision(EN_2018)

EN_2018_extracted <-
  rank_specialty_EN_2018 %>%
  cbind(subdivision_EN_2018) 

EN_2018_extracted$Lieu <- 
  str_replace_all(EN_2018_extracted$Lieu, "au ", "")

EN_2018_extracted$Lieu <- 
  str_replace_all(EN_2018_extracted$Lieu, "à l'", "")

EN_2018_extracted$Lieu <- 
  str_replace_all(EN_2018_extracted$Lieu, "aux ", "")

EN_2018_extracted$Lieu <- 
  str_replace_all(EN_2018_extracted$Lieu,"\\.", "")

EN_2018_extracted$Rang <-
  as.numeric(EN_2018_extracted$Rang)

EN_2018_extracted <-
  EN_2018_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2018)

#EN_2017 ####
# Les données des EN on
EN_2017 <-
  readxl::read_excel("./Raw_data/2017_Raw.xlsx")

EN_2017$Info <- 
  str_replace_all(EN_2017$Info, "-", "")

rank_specialty_EN_2017 <-
  extracting_rank_specialty(EN_2017) #functions created

EN_2017$Info <- 
  str_replace_all(EN_2017$Info, " \\(", "") #it's because of the 

subdivision_EN_2017 <-
  extracting_subdivision(EN_2017)

EN_2017_extracted <-
  rank_specialty_EN_2017 %>%
  cbind(subdivision_EN_2017) 

EN_2017_extracted$Lieu <- 
  str_replace_all(EN_2017_extracted$Lieu, "au ", "")

EN_2017_extracted$Lieu <- 
  str_replace_all(EN_2017_extracted$Lieu, "à l'", "")

EN_2017_extracted$Lieu <- 
  str_replace_all(EN_2017_extracted$Lieu, "aux ", "")

EN_2017_extracted$Lieu <- 
  str_replace_all(EN_2017_extracted$Lieu,"\\.", "")

EN_2017_extracted$Rang <-
  as.numeric(EN_2017_extracted$Rang)

EN_2017_extracted <-
  EN_2017_extracted %>% mutate(Rang_indexe = Rang / nombre_etudiant_2017)


# Saving data ####
# Je sauvegarde les dataframe sous forme de tableau Excel dans un fichier
# appellé Preprocessed_data
EN_2023_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2023_extracted.xlsx")

EN_2022_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2022_extracted.xlsx")

EN_2021_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2021_extracted.xlsx")

EN_2020_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2020_extracted.xlsx")

EN_2019_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2019_extracted.xlsx")

EN_2018_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2018_extracted.xlsx")

EN_2017_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_2017_extracted.xlsx")

# Création d'un format dit long des données issues des EN ####
# Maintenant que les données des EN (Epreuves nationales) ont été extraites
# Nous pouvons les fusionner sous la forme d'un long tableau

# Je rajoute une colone indiquant l'année de l'ECN pour chaque dataframe
EN_2023_extracted <-
  EN_2023_extracted %>% mutate(Year = 2023)
EN_2022_extracted <-
  EN_2022_extracted %>% mutate(Year = 2022)
EN_2021_extracted <-
  EN_2021_extracted %>% mutate(Year = 2021)
EN_2020_extracted <-
  EN_2020_extracted %>% mutate(Year = 2020)
EN_2019_extracted<-
  EN_2019_extracted %>% mutate(Year = 2019)
EN_2018_extracted <- 
  EN_2018_extracted %>% mutate(Year = 2018)
EN_2017_extracted <- 
  EN_2017_extracted %>% mutate(Year = 2017)

EN_all_extracted <-
  rbind(EN_2023_extracted,
        EN_2022_extracted, EN_2021_extracted, 
        EN_2020_extracted, EN_2019_extracted,
        EN_2018_extracted)
 
# Certains noms de subdivision divergaient légèrement d'une année à l'autre
# La suite du code permet d'avoir le même nom partout.
# Réunion
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de la Réunion", "CHU de la Réunion")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de La Réunion", "CHU de la Réunion")
#APHM
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "Assistance publique des hôpitde Marseille", "Assistance publique des hôpitde Marseille")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "Assistance Publique des Hôpitde Marseille", "Assistance publique des hôpitde Marseille")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "APHM", "Assistance publique des hôpitde Marseille")
#APHP
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "Assistance publiquehôpitde Paris", "Assistance publiquehôpitde Paris")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "Assistance PubliqueHôpitde Paris", "Assistance publiquehôpitde Paris")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "APHP", "Assistance publiquehôpitde Paris")
# hospices civils de Lyon
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "hospices civils de Lyon", "hospices civils de Lyon")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "Hospices Civils de Lyon", "hospices civils de Lyon")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "HCL", "hospices civils de Lyon")
# Martinique
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de la Martinique/PointeàPitre", "CHU de la Martinique/PointeàPitre")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de La Martinique/PointeàPitre", "CHU de la Martinique/PointeàPitre")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de la Martinique / PointeàPitre", "CHU de la Martinique/PointeàPitre")
EN_all_extracted$Lieu <- str_replace(EN_all_extracted$Lieu, "CHU de la Guadeloupe", "CHU de la Martinique/PointeàPitre")

# Enfin, j'enregistre le tableau excel en format long dans le fichier preprocessed 
EN_all_extracted %>%
  writexl::write_xlsx("./Preprocessed_data/EN_all_extracted.xlsx")


