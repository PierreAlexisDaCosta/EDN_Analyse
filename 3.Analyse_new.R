# Cette troisième partie est l'analyse du critère de rang en tant que tel
# Charger les données du format  ####
EN_all_extracted <-
  readxl::read_excel("./Preprocessed_data/EN_all_extracted.xlsx")

# Attention les rangs sont indexés sur le nombre de candidat (cf Partie 2) 
# donc ils sont tous compris entre 0 et 1.

# Calculer le rang max d'un poste (spécialité dans une subdivision) #####

# Je calcule ensuite le rang max pour lequel un étudiant a obtenu chaque poste
# Un poste = spécialité dans une subdivision donnée
rang_max_all_EN <- 
  EN_all_extracted %>% 
  rang_max_analyse(rang_type = Rang_indexe) # fonction crée dans la partie 2.Data_wrangling
rang_max_all_EN %>% writexl::write_xlsx("./Results/rang_max_all_EN.xlsx")
# Cette fonction prend automatiquement le Rang_indexe, il est cependant possible le changer
# en indicant la colonne Rang comme ceci :
# 
#   rang_max_all_EN <- 
#       EN_all_extracted %>% 
#          rang_max_analyse(rang_type = Rang)

# Critère A ####
# Ensuite je calcule
année_var <-
  rang_max_all_EN %$%
  unique(Year)
année_id <-
  rang_max_all_EN %$%
  unique(Year)

# Calcul du rang pour lequel l'étudiant a accès à 50% des subdivisions
# Le rang indexé est appelé par la colonne décile 
décile_50_all <-
  pbapply::pblapply(année_var, FUN = function(année_id){
  print(année_id)
  décile_50 <-
  rang_max_all_EN %>% 
  fonction_calcul_décile(Année = année_id,
                         Accessibilité_nombre = 50,
                         nombre_etudiant_Année = nombre_etudiant_2022)
  
  }) %>%
  setNames(année_var) %>%
  plyr::ldply(.id = "Year") %>%
  dplyr::rename(décile_50 = decile)

décile_30_all <-
  pbapply::pblapply(année_var, FUN = function(année_id){
    print(année_id)
    décile_50 <-
      rang_max_all_EN %>% 
      fonction_calcul_décile(Année = année_id,
                             Accessibilité_nombre = 30,
                             nombre_etudiant_Année = nombre_etudiant_2022)
    
  }) %>%
  setNames(année_var) %>%
  plyr::ldply(.id = "Year")

décile_30_all <-
  décile_30_all %>% 
  dplyr::rename(seuil_pour_avoir_30_accessibilité = seuil_pour_avoir_50_accessibilité) %>%
  dplyr::rename(décile_30 = decile)


# Fusion des deux dataframes (calcul à 30% et à 50%)

décile_30_50_2023_merge <-
  décile_50_all %>% 
  left_join(décile_30_all, by = c("DES", "Year")) %>%
  filter(Year != 2023) 


# Calcul du critère A de 3 à 5 ans ####
# Nous faisons la moyenne du rang indexé que nous avons trouvé au dessus
décile_30_50_all_2023_3_ans <-
  décile_30_50_2023_merge %>%
  filter(Year != 2018 &
           Year != 2019) %>%
  dplyr::group_by(DES) %>%
  dplyr::summarize(décile_50_moyenne_3ans = round(mean(décile_50), 2),
                   décile_30_moyenne_3ans = round(mean(décile_30),2))

décile_30_50_all_2023_4_ans <-
  décile_30_50_2023_merge %>%
  filter(Year != 2018) %>%
  dplyr::group_by(DES) %>%
  dplyr::summarize(décile_50_moyenne_4ans = round(mean(décile_50), 2),
                   décile_30_moyenne_4ans = round(mean(décile_30),2))

décile_30_50_all_2023_5_ans <-
  décile_30_50_2023_merge %>%
  dplyr::group_by(DES) %>%
  dplyr::summarize(décile_50_moyenne_5ans = round(mean(décile_50), 2),
                   décile_30_moyenne_5ans = round(mean(décile_30),2))

décile_30_50_2023_long <-
  décile_30_50_2023_merge %>% 
  left_join(décile_30_50_all_2023_3_ans, by = "DES") %>%
  left_join(décile_30_50_all_2023_4_ans, by = "DES") %>%
  left_join(décile_30_50_all_2023_5_ans, by = "DES")
#J'enregistre le dataframe en format long dans les résultats sous la forme
# d'un tableur excel dans le fichier Results
décile_30_50_2023_long %>%
  writexl::write_xlsx("./Results/Critère_A_décile_30_50_2023_long.xlsx")

# Représentation graphique du calcul du critère A ####

Critère_A_graphique <-
  décile_30_50_2023_long %>% 
  filter(Year != "2017") %>%
  mutate(label_name = paste0(DES, "\n",
                      "- moyenne 3 derniers EN : \n",
                      décile_30_moyenne_3ans, " (30%) & ",
                      décile_50_moyenne_3ans, " (50%) \n - moyenne 5 derniers EN : \n",
                      décile_30_moyenne_5ans, " (30%) & ",
                      décile_50_moyenne_5ans, " (50%)")) %>%
  ggplot(aes(x = Year
             )) +
  geom_point(aes(y = décile_50)) +
  geom_line(aes(y = décile_50, group = 1, color = "décile_50")) + 
  # ggplot2::geom_text(aes(y = décile_50, label = décile_50), 
  #                    hjust = -0.1, vjust = -1) +
  geom_point(aes(y = décile_30)) +
  geom_line(aes(y = décile_30, group = 1, color = "décile_30")) + 
  facet_wrap(~ label_name) +
  ylab("Rang pour lequel un étudiant aurait eu accès à 50% des spécialités") +
  ggtitle("Représentation graphique du calcul du rang A pour l'ECN 2023") +
  mythemefortop10 +
  xlab("Année")

ggsave(plot = Critère_A_graphique, file = "./Results/Critère_A_graphique.png",
       height = 10, width = 15.5)

# Applicabilité du rang A ####

Critère_A_ECN2023 <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_5_ans, by = "DES") %>% 
  mutate(décile_30_moyenne_5ans = round(décile_30_moyenne_5ans * nombre_etudiant_2023),
         décile_50_moyenne_5ans = round(décile_50_moyenne_5ans * nombre_etudiant_2023),
         rang_max = rang_max * nombre_etudiant_2023) %>%
  filter(Year == 2023)
Critère_A_ECN2023 %>% writexl::write_xlsx("./Results/Critère_A_ECN2023.xlsx")

Critère_A_ECN2023_graphique <-
  Critère_A_ECN2023 %>% 
  mutate(label_name = paste0(DES, "\n",
                             " à l'ECN ", Year, "\n",
                             "Critère A (rang): ", "\n",
                             décile_30_moyenne_5ans, " (30%) & ",
                             décile_50_moyenne_5ans, " (50%)"
  )) %>%
  ggplot2::ggplot(aes(y = rang_max, x = Year, show.legend = FALSE)) +
  geom_jitter(alpha = 0.3)+
  geom_hline(aes(
                 yintercept = décile_50_moyenne_5ans), color = "cyan3",
             alpha = 0.5) +
  geom_hline(aes(
                 yintercept = décile_30_moyenne_5ans), color = "red2",
             alpha = 0.5) +
  ylab("Le rang max d'un étudiant dans une subdivision (en fonction du DES)") +
  ggtitle("Critère A (5ans) appliqué aux ECN 2023") + 
  theme(legend.position = "right") +
  facet_wrap(~ label_name) +
  mythemefortop10 +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggsave(plot = Critère_A_ECN2023_graphique, file = "./Results/Critère_A_ECN2023.png",
       height = 10, width = 15.5)

# pour 4 ans et 3 ans
Critère_A_ECN2023_graphique_3ans <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_3_ans, by = "DES") %>% 
  mutate(décile_30_moyenne_3ans = round(décile_30_moyenne_3ans * nombre_etudiant_2023),
         décile_50_moyenne_3ans = round(décile_50_moyenne_3ans * nombre_etudiant_2023),
         rang_max = rang_max * nombre_etudiant_2023) %>%
  filter(Year == 2023) %>%
  mutate(label_name = paste0(DES, "\n",
                             " à l'ECN ", Year, "\n",
                             "Critère A (rang): ", "\n",
                             décile_30_moyenne_3ans, " (30%) & ",
                             décile_50_moyenne_3ans, " (50%)"
  )) %>%
  ggplot2::ggplot(aes(y = rang_max, x = Year, show.legend = FALSE)) +
  geom_jitter(alpha = 0.3)+
  geom_hline(aes(
    yintercept = décile_50_moyenne_3ans), color = "cyan3",
    alpha = 0.5) +
  geom_hline(aes(
    yintercept = décile_30_moyenne_3ans), color = "red2",
    alpha = 0.5) +
  ylab("Le rang max d'un étudiant dans une subdivision (en fonction du DES)") +
  ggtitle("Critère A (3ans) appliqué aux ECN 2023") + 
  theme(legend.position = "right") +
  facet_wrap(~ label_name) +
  mythemefortop10 +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggsave(plot = Critère_A_ECN2023_graphique_3ans, file = "./Results/Critère_A_ECN2023_graphique_3ans.png",
       height = 10, width = 15.5)

Critère_A_ECN2023_graphique_4ans <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_4_ans, by = "DES") %>% 
  mutate(décile_30_moyenne_4ans = round(décile_30_moyenne_4ans * nombre_etudiant_2023),
         décile_50_moyenne_4ans = round(décile_50_moyenne_4ans * nombre_etudiant_2023),
         rang_max = rang_max * nombre_etudiant_2023) %>%
  filter(Year == 2023) %>%
  mutate(label_name = paste0(DES, "\n",
                             " à l'ECN ", Year, "\n",
                             "Critère A (rang): ", "\n",
                             décile_30_moyenne_4ans, " (30%) & ",
                             décile_50_moyenne_4ans, " (50%)"
  )) %>%
  ggplot2::ggplot(aes(y = rang_max, x = Year, show.legend = FALSE)) +
  geom_jitter(alpha = 0.3)+
  geom_hline(aes(
    yintercept = décile_50_moyenne_4ans), color = "cyan3",
    alpha = 0.5) +
  geom_hline(aes(
    yintercept = décile_30_moyenne_4ans), color = "red2",
    alpha = 0.5) +
  ylab("Le rang max d'un étudiant dans une subdivision (en fonction du DES)") +
  ggtitle("Critère A (4ans) appliqué aux ECN 2023") + 
  theme(legend.position = "right") +
  facet_wrap(~ label_name) +
  mythemefortop10 +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggsave(plot = Critère_A_ECN2023_graphique_4ans, file = "./Results/Critère_A_ECN2023_graphique_4ans.png",
       height = 10, width = 15.5)

# Place gagnées avec le critère A pour 5 ans ####
place_g <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_5_ans, by = "DES") %>%
  filter(Year =="2023") %>%
  mutate(place_gagnées_30 = ifelse(décile_30_moyenne_5ans - rang_max > 0, 
                                   round((décile_30_moyenne_5ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) %>%
  mutate(place_gagnées_50 = ifelse(décile_50_moyenne_5ans - rang_max > 0, 
                                   round((décile_50_moyenne_5ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) 
place_g_moyenne <-
  place_g %>%
  dplyr::group_by(DES, Year) %>%
  dplyr::summarize(place_gagnées_moyenne_30 = mean(place_gagnées_30, na.rm = T),
                   place_gagnées_moyenne_50 = mean(place_gagnées_50, na.rm = T))
place_g_moyenne$place_gagnées_moyenne_50 %>% mean()
plot_place_g <-
  place_g %>%   
  select(c(Year, DES, Lieu, 
           place_gagnées_30, 
           place_gagnées_50)) %>%
  pivot_longer(cols = c(place_gagnées_30, 
                        place_gagnées_50), 
               names_to = "calcul_du_pourcentage_30_ou_50", 
               values_to = "value") %>%
  left_join(place_g_moyenne, by = c("DES", "Year")) %>%
  mutate(label_name = paste0(DES, "\n",
                           "Place gagnées (EN 2023) : ", "\n",
                           round(place_gagnées_moyenne_30), " places (30%) & ", 
                           round(place_gagnées_moyenne_50), " places (50%)")) %>%
  ggplot(aes(x = calcul_du_pourcentage_30_ou_50, y = value, color = calcul_du_pourcentage_30_ou_50)) +
  geom_jitter(alpha = 0.75) +
  facet_wrap(~ label_name) +
  ggtitle("Places gagnées pour une subdivision par rapport au critère A (ECN 2022) (calculé avec les 5 dernières années)") +
  mythemefortop10 + 
  theme(legend.position= "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Différence entre le rang du Critère A et le rang max d'une subdivion dans une specialité")

ggsave(file = "./Results/Place_G_plot.png", plot = plot_place_g, width = 17, height = 13)

# Places gagnées avec el critère A pour 3 et 4 ans ####
place_g_3ans <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_3_ans, by = "DES") %>%
  filter(Year =="2023") %>%
  mutate(place_gagnées_30 = ifelse(décile_30_moyenne_3ans - rang_max > 0, 
                                   round((décile_30_moyenne_3ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) %>%
  mutate(place_gagnées_50 = ifelse(décile_50_moyenne_3ans - rang_max > 0, 
                                   round((décile_50_moyenne_3ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) 
place_g_moyenne_3ans <-
  place_g_3ans %>%
  dplyr::group_by(DES, Year) %>%
  dplyr::summarize(place_gagnées_moyenne_30 = mean(place_gagnées_30, na.rm = T),
                   place_gagnées_moyenne_50 = mean(place_gagnées_50, na.rm = T))

plot_place_g_3ans <-
  place_g_3ans %>%   
  select(c(Year, DES, Lieu, 
           place_gagnées_30, 
           place_gagnées_50)) %>%
  pivot_longer(cols = c(place_gagnées_30, 
                        place_gagnées_50), 
               names_to = "calcul_du_pourcentage_30_ou_50", 
               values_to = "value") %>%
  left_join(place_g_moyenne_3ans, by = c("DES", "Year")) %>%
  mutate(label_name = paste0(DES, "\n",
                             "Place gagnées (EN 2023) : ", "\n",
                             round(place_gagnées_moyenne_30), " places (30%) & ", 
                             round(place_gagnées_moyenne_50), " places (50%)")) %>%
  ggplot(aes(x = calcul_du_pourcentage_30_ou_50, y = value, color = calcul_du_pourcentage_30_ou_50)) +
  geom_jitter(alpha = 0.75) +
  facet_wrap(~ label_name) +
  ggtitle("Places gagnées pour une subdivision par rapport au critère A (ECN 2022) (calculé avec les 3 dernières années)") +
  mythemefortop10 + 
  theme(legend.position= "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Différence entre le rang du Critère A et le rang max d'une subdivion dans une specialité")

ggsave(file = "./Results/Place_G_plot_3ans.png", plot = plot_place_g_3ans, width = 17, height = 13)


place_g_4ans <-
  rang_max_all_EN %>%
  mutate_at(vars(Year), as.character) %>%
  left_join(décile_30_50_all_2023_4_ans, by = "DES") %>%
  filter(Year =="2023") %>%
  mutate(place_gagnées_30 = ifelse(décile_30_moyenne_4ans - rang_max > 0, 
                                   round((décile_30_moyenne_4ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) %>%
  mutate(place_gagnées_50 = ifelse(décile_50_moyenne_4ans - rang_max > 0, 
                                   round((décile_50_moyenne_4ans - rang_max) * nombre_etudiant_2022), 
                                   no = 0)) 
place_g_moyenne_4ans <-
  place_g_4ans %>%
  dplyr::group_by(DES, Year) %>%
  dplyr::summarize(place_gagnées_moyenne_30 = mean(place_gagnées_30, na.rm = T),
                   place_gagnées_moyenne_50 = mean(place_gagnées_50, na.rm = T))

plot_place_g_4ans <-
  place_g_4ans %>%   
  select(c(Year, DES, Lieu, 
           place_gagnées_30, 
           place_gagnées_50)) %>%
  pivot_longer(cols = c(place_gagnées_30, 
                        place_gagnées_50), 
               names_to = "calcul_du_pourcentage_30_ou_50", 
               values_to = "value") %>%
  left_join(place_g_moyenne_4ans, by = c("DES", "Year")) %>%
  mutate(label_name = paste0(DES, "\n",
                             "Place gagnées (EN 2023) : ", "\n",
                             round(place_gagnées_moyenne_30), " places (30%) & ", 
                             round(place_gagnées_moyenne_50), " places (50%)")) %>%
  ggplot(aes(x = calcul_du_pourcentage_30_ou_50, y = value, color = calcul_du_pourcentage_30_ou_50)) +
  geom_jitter(alpha = 0.75) +
  facet_wrap(~ label_name) +
  ggtitle("Places gagnées pour une subdivision par rapport au critère A (ECN 2022) (calculé avec les 4 dernières années)") +
  mythemefortop10 + 
  theme(legend.position= "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Différence entre le rang du Critère A et le rang max d'une subdivion dans une specialité")

ggsave(file = "./Results/Place_G_plot_4ans.png", plot = plot_place_g_4ans, width = 17, height = 13)


# Critère B ####
# Nous avons fait une représentation gr apique de l'évolution du rang indexé 
# du dernier étudiant pris dans une subdivision pour chaque spécialité
#rang_max_plot <-
create_des_plot_and_save <- 
  function(des) {
    plot <- 
      EN_all_extracted %>% 
      rang_max_analyse(rang_type = Rang_indexe) %>%
      mutate(label_name = paste0(DES, "\n", Lieu)) %>%
      filter(DES == des) %>%
      ggplot(aes(x = Year, y = rang_max, colour = Lieu)) +
      geom_point() +
      geom_line(aes(color = Lieu)) + 
      facet_wrap(~ Lieu) +
      ylab("Le dernier rang pour une spécialité dans une subdivision") +
      mythemefortop10 +
      theme(legend.position = "bottom",
            axis.text=element_text(size=8),
            axis.title=element_text(size= 15, face="bold"),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(des) +
      theme(legend.position = "none") +
      xlab("Année de l'ECN")
    
    
    # Specify the file name for saving (you can customize this)
    file_name <- paste0("./Results/Loop_rang_max/" ,des, "_plot.png")
    
    # Save the plot as an image file (e.g., PNG)
    ggsave(file_name, plot, width = 15, height = 10, dpi = 300)
  }

# Create and save plots for each DES
walk(des_list, create_des_plot_and_save)


# Sans le rang indexe (même chose que précedemment)
create_des_plot_and_save <- 
  function(des) {
    plot <- 
      EN_all_extracted %>% 
      rang_max_analyse(rang_type = Rang) %>%
      mutate(label_name = paste0(DES, "\n", Lieu)) %>%
      filter(DES == des) %>%
      ggplot(aes(x = Year, y = rang_max, colour = Lieu)) +
      geom_point() +
      geom_line(aes(color = Lieu)) + 
      facet_wrap(~ Lieu) +
      ylab("Le dernier rang pour une spécialité dans une subdivision") +
      mythemefortop10 +
      theme(legend.position = "bottom",
            axis.text=element_text(size=8),
            axis.title=element_text(size= 15, face="bold"),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(des) +
      theme(legend.position = "none") +
      xlab("Année de l'ECN")
    
    
    # Specify the file name for saving (you can customize this)
    file_name <- paste0("./Results/Loop_rang_max_non_indexe/" ,des, "_plot.png")
    
    # Save the plot as an image file (e.g., PNG)
    ggsave(file_name, plot, width = 15, height = 10, dpi = 300)
  }

# Create and save plots for each DES
walk(des_list, create_des_plot_and_save)
