library(readxl)
library(dplyr)
library(officer)


#=========================================
# PARAMETRES
#=========================================

fichier_excel <- "DATA FINAL.xlsx"
template_word <- "Rapport_template.docx"
dossier_sortie <- "Rapport"
dir.create(dossier_sortie, showWarnings = FALSE)

#=========================================
# LECTURE DES DONNEES
#=========================================

resultat <- read_excel(
  fichier_excel,
  sheet = "Resultat"
)
resultat <- resultat %>%
  filter(!is.na(prelevement_id))

elevage <- read_excel(
  fichier_excel,
  sheet = "Elevage"
)


#=========================================
# CONCLUSION NEGATIVE
#=========================================

conclusion_neg <- "Aucune souche d’entérobactérie productrice de BLSE ou de carbapénémase n’a été détectée dans votre élevage. Ces résultats indiquent l’absence de résistance aux céphalosporines de 3ème et 4ème génération ainsi qu’aux carbapénèmes."

#=========================================
# SUIVI DES ELEVAGES DEJA TRAITES
#=========================================

deja_fait <- c()

nb_rapports <- 0

#=========================================
# BOUCLE PRINCIPALE
#=========================================

for(i in 1:nrow(resultat)) {

  id <- resultat[[2]][i]

  if(id %in% deja_fait) {
    next
  }

  deja_fait <- c(deja_fait, id)

  #---------------------------------------
  # Regroupe Toutes les lignes de cet élevage
  #---------------------------------------

  bloc <- resultat[resultat[[2]] == id, ]
  m <- nrow(bloc)
  print(id)
  print(m)
  print(bloc[,2])

  cat("Elevage :", id,"| Nombre d'échantillons :", m,"\n")

  #---------------------------------------
  # Vérification présence positif
  #---------------------------------------

  positif <- any(
    bloc$Pos_neg == 1,
    na.rm = TRUE
  )

  if(positif) {

    cat(
      ">>> Cas à traiter manuellement : ",
      id,
      "\n"
    )

  }
  else{


  #---------------------------------------
  # Recherche des infos élevage
  #---------------------------------------

  infos <- elevage[
    elevage[[1]] == id,
  ]

  if(nrow(infos) == 0) {

    cat(
      "Aucune information trouvée pour :",
      id,
      "\n"
    )

    }else{

  #---------------------------------------
  # Données du rapport
  #---------------------------------------

  nom_elevage <- as.character(
    infos[[6]][1]
  )

  eleveur <- as.character(
    infos[[7]][1]
  )

  date_prel <- format(
    as.Date(infos[[2]][1]),
    "%d/%m/%Y"
  )

  preleveur <- as.character(
    infos[[9]][1]
  )

  ojd <- format(
    Sys.Date(),
    "%d/%m/%Y"
  )

  #---------------------------------------
  # Création document
  #---------------------------------------

  doc <- read_docx(template_word)

  doc <- body_replace_all_text(
    doc,
    old_value = "<<ELEVAGE>>",
    new_value = nom_elevage
  )

  doc <- body_replace_all_text(
    doc,
    old_value = "<<ELEVEUR>>",
    new_value = eleveur
  )

  doc <- body_replace_all_text(
    doc,
    old_value = "<<DATE>>",
    new_value = date_prel
  )

  doc <- body_replace_all_text(
    doc,
    old_value = "CONCLU_PLACEHOLDER",
    new_value = conclusion_neg
  )
  doc <- body_replace_all_text(
    doc,
    old_value = "PRELEVEUR_PLACEHOLDER",
    new_value = preleveur
  )
  doc <- body_replace_all_text(
    doc,
    old_value = "jjmmaaaa",
    new_value = ojd
  )
  #---------------------------------------
  # Nom du fichier
  #---------------------------------------

  nom_fichier <- gsub(
    "[/:\\\\]",
    "-",
    nom_elevage
  )

  chemin_sortie <- file.path(
    dossier_sortie,
    paste0(
      nom_fichier,
      ".docx"
    )
  )

  #---------------------------------------
  # Sauvegarde
  #---------------------------------------

  print(
    doc,
    target = chemin_sortie
  )

  nb_rapports <- nb_rapports + 1

  cat(
    "Rapport généré : ",
    nom_elevage,
    "\n"
  )

    }
  }
}

#=========================================
# FIN
#=========================================

cat(
  "\n",
  nb_rapports,
  "rapport(s) négatif(s) généré(s)\n"
)

