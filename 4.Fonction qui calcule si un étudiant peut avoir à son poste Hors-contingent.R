# Pour avoir la liste des spécialtiés et des subdivosions

poste_HC <-
  function(Année,
         rang_étudiant, nombre_etudiant_classées,
         spécialité_désirée, 
         subdivision_désirée){
  
  rang_calculé <-
    rang_max_all_EN %>%
    filter(Year == Année) %>%
    left_join(
      décile_30_50_all_2023_5_ans, 
      by = c("DES"))
  
  rang_calculé_pour_la_spé <-
    rang_calculé %>%
    filter(DES == spécialité_désirée &
             Lieu == subdivision_désirée)
  
  if(rang_calculé_pour_la_spé$décile_50_moyenne_5ans >= (rang_étudiant / nombre_etudiant_classées)) {
    print("Test du critère A :")
    print(paste0("Rang de l'étudiant : ", rang_étudiant, " sur ",
                 nombre_etudiant_classées,
                 " étudiants classés aux épreuves nationales",
                 ", soit un rang indexé de ", round((rang_étudiant / nombre_etudiant_classées),4),
                 " & Rang du critère A : ", rang_calculé_pour_la_spé$décile_50_moyenne_5ans,
                 " --> Le critère A est donc validé/"))
    print("l'étudiant a accès à son poste hors-contingent.")
    
  } else{
    print(paste0("Rang de l'étudiant : ", rang_étudiant, " sur ",
                 nombre_etudiant_classées,
                 " étudiants classés aux épreuves nationales",
                 ", soit un rang indexé de ", round((rang_étudiant / nombre_etudiant_classées),4),
                 " & Rang du critère A : ", rang_calculé_pour_la_spé$décile_50_moyenne_5ans,
                 " --> Le critère A n'est pas validé."))
    print("Test du critère B :")
    if(rang_calculé_pour_la_spé$rang_max >= (rang_étudiant / nombre_etudiant_classées)){
      print(paste0("Rang de l'étudiant : ", rang_étudiant, " sur ",
                   nombre_etudiant_classées,
                   " étudiants classés aux épreuves nationales",
                   ", soit un rang indexé de ", round((rang_étudiant / nombre_etudiant_classées),4),
                   " & Rang du critère B : ", round(rang_calculé_pour_la_spé$rang_max, 4),
                   " --> Le critère B est donc validé l'étudiant a accès à son poste hors-contingent"))
    }
    else{
      print(paste0("Rang de l'étudiant : ", rang_étudiant, " sur ",
                   nombre_etudiant_classées,
                   " étudiants classés aux épreuves nationales",
                   ", soit un rang indexé de ", round((rang_étudiant / nombre_etudiant_classées),4),
                   " & Rang du critère B : ", round(rang_calculé_pour_la_spé$rang_max,4),
                   " --> Le critère B n'est pas validé également."))
      print("l'étudiant n'a pas accès à son poste hors-contingent.")
    }
  }
}

rang_max_all_EN %$% unique(DES)
rang_max_all_EN %$% unique(Lieu)

poste_HC(Année = "2023",
         rang_étudiant = 5000, nombre_etudiant_classées = 10000,
         spécialité_désirée = "anesthésieréanimation", 
         subdivision_désirée = "CHU de Strasbourg")

# github configuration ####
library(usethis)
library(gitcreds)
usethis::edit_git_config() #Veryfy username and mail
usethis::use_git()
 usethis::create_github_token()
# # This will take you to the appropriate page on the GitHub website,
# # where you’ll give your token a name and copy it (don’t lose it because
# # it will never appear again!).
# Token_name : CER_Critère de rang
# Token : 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
gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)


