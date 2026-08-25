#=========================================
# Library
#=========================================
library(readxl)
library(dplyr)
library(gt)
library(scales)
#=========================================
# Base de données
#=========================================
ID<- read_excel("DATA_Final.xlsx", sheet = 3)
ID<-ID[,1:10]
Culture<-read_excel("DATA_Final.xlsx", sheet = 1)
Culture <- Culture %>%
  filter(if_any(c(PN_ESBL, PN_CARBA), ~ . == 1))
Culture<-Culture[, c(13, 9, 11)]

ID<- left_join(Culture, ID, by = names(ID)[1])
ID<- as.data.frame(ID)
ID$contigs <- NULL
ID$genome_size <- NULL
ID$N50 <- NULL
#=========================================
# Table ID
#=========================================
table_identification <- ID %>%
  transmute(
    isolat_ID,
    #PN
    'Culture_ESBL'  = ifelse(PN_ESBL == 1, "Positive", "Negative"),
    'Culture_CARBA' = ifelse(PN_CARBA == 1, "Positive", "Negative"),
    # MALDI
    Identification_MALDI = `ID-maldi`,
    Score_MALDI,

    Niveau_MALDI = case_when(
      is.na(Score_MALDI) ~ "ND",
      #Score_MALDI == 0 ~ "Non Déterminé",
      Score_MALDI >= 2.30 ~ "Espèce++",
      Score_MALDI >= 2.00 ~ "Espèce",
      Score_MALDI >= 1.70 ~ "Genre",
      TRUE ~ "Non fiable"
    ),

    # MLST
    Scheme = scheme,
    ST,

    # Gambit
    Identification_Gambit = Gambit

  )%>%
  # isolats retenus
  slice(1:17)

# Création de la figure-table
fig_identification <- table_identification %>%

  gt() %>%

  tab_spanner(
    label = md("**Culture Selective**"),
    columns = c(
      Culture_ESBL,
      Culture_CARBA,
    )
  ) %>%

  tab_spanner(
    label = md("**MALDI-TOF**"),
    columns = c(
      Identification_MALDI,
      Score_MALDI,
      Niveau_MALDI
    )
  ) %>%

  tab_spanner(
    label = md("**MLST**"),
    columns = c(
      Scheme,
      ST
    )
  ) %>%

  tab_spanner(
    label = md("**Gambit**"),
    columns = c(
      Identification_Gambit
    )
  ) %>%

  cols_label(
    isolat_ID = "Isolat",
    Culture_ESBL="Culture",
    Culture_CARBA="Culture",
    Identification_MALDI = "Identification",
    Score_MALDI = "Score",
    Niveau_MALDI = "Niveau",
    Scheme = "Schéma",
    ST = "ST",
    Identification_Gambit = "Identification"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey85"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Score_MALDI, Niveau_MALDI),
      rows = Niveau_MALDI == "Non Déterminé"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a9850"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(
      columns = c(Score_MALDI, Niveau_MALDI),
      rows = Niveau_MALDI == "Espèce++"
    )
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "#66bd63"),
      cell_text(color = "white", weight = "bold")
  ),
    locations = cells_body(
      columns = c(Score_MALDI, Niveau_MALDI),
      rows = Niveau_MALDI == "Espèce"
    )
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "#fdae61"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(
      columns = c(Score_MALDI, Niveau_MALDI),
      rows = Niveau_MALDI == "Genre"
    )
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "#d73027"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(
      columns = c(Score_MALDI, Niveau_MALDI),
      rows = Niveau_MALDI == "Non fiable"
    )
  ) %>%


  opt_row_striping() %>%

  tab_header(
    title = md("**Identification des isolats par MALDI-TOF, MLST et Gambit**")
  ) %>%

  tab_source_note(
    source_note = md(
      "*Les niveaux de confiance MALDI sont basés sur les seuils d'identification Bruker Biotyper.*"
    )
  ) %>%

  tab_options(
    table.font.size = px(12),
    heading.align = "center",
    table.width = pct(100)
  )

fig_identification

