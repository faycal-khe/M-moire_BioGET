#=========================================
# Phénotype
#=========================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#=========================================
# Importation
#=========================================

res <- read_excel("DATA Final.xlsx", sheet = 1)

#=========================================
# Tableau phénotypique
#=========================================

pheno <- res %>%
  select(
    isolat_ID,
    amo_20,
    tgc_15,
    tic_75,
    col_10,
    cox_5,
    amc_30,
    caz_10,
    gmn_10,
    amp_10,
    fep_30,
    etp_10,
    ipm_10,
    ofx_5,
    cip_5,
    sxt_25,
    tet_30,
    synergie = `synergie...30`
  ) %>%

  mutate(
    across(
      -c(isolat_ID, synergie),
      ~ suppressWarnings(as.numeric(.))
    )
  ) %>%

  filter(
    rowSums(
      !is.na(
        select(
          .,
          -isolat_ID,
          -synergie
        )
      )
    ) > 0
  ) %>%

  mutate(
    Groupe = ifelse(
      synergie == "YES",
      "BLSE +",
      "BLSE -"
    )
  ) %>%

  arrange(desc(Groupe))

#=========================================
# Passage au format long
#=========================================

pheno_long <- pheno %>%
  pivot_longer(
    cols = c(
      amo_20,
      tgc_15,
      tic_75,
      col_10,
      cox_5,
      amc_30,
      caz_10,
      gmn_10,
      amp_10,
      fep_30,
      etp_10,
      ipm_10,
      ofx_5,
      cip_5,
      sxt_25,
      tet_30
    ),
    names_to = "Antibiotique",
    values_to = "Diametre"
  )
pheno_long <- pheno_long %>%
  mutate(
    Categorie = case_when(

      # Pénicillines
      Antibiotique == "amo_20" & Diametre >= 19 ~ "S",
      Antibiotique == "amo_20" & Diametre < 19 ~ "R",

      Antibiotique == "amp_10" & Diametre >= 14 ~ "S",
      Antibiotique == "amp_10" & Diametre < 14 ~ "R",

      Antibiotique == "tic_75" & Diametre >= 23 ~ "S",
      Antibiotique == "tic_75" & Diametre < 20 ~ "R",
      Antibiotique == "tic_75" ~ "I",

      Antibiotique == "amc_30" & Diametre >= 19 ~ "S",
      Antibiotique == "amc_30" & Diametre < 19 ~ "R",

      # Céphalosporines
      Antibiotique == "cox_5" & Diametre >= 20 ~ "S",
      Antibiotique == "cox_5" & Diametre < 17 ~ "R",
      Antibiotique == "cox_5" ~ "I",

      Antibiotique == "caz_10" & Diametre >= 22 ~ "S",
      Antibiotique == "caz_10" & Diametre < 19 ~ "R",
      Antibiotique == "caz_10" ~ "I",

      Antibiotique == "fep_30" & Diametre >= 27 ~ "S",
      Antibiotique == "fep_30" & Diametre < 21 ~ "R",
      Antibiotique == "fep_30" ~ "I",

      # Fluoroquinolones
      Antibiotique == "cip_5" & Diametre >= 26 ~ "S",
      Antibiotique == "cip_5" & Diametre < 24 ~ "R",
      Antibiotique == "cip_5" ~ "I",

      Antibiotique == "ofx_5" & Diametre >= 24 ~ "S",
      Antibiotique == "ofx_5" & Diametre < 22 ~ "R",
      Antibiotique == "ofx_5" ~ "I",

      # Aminosides
      Antibiotique == "gmn_10" & Diametre >= 17 ~ "S",
      Antibiotique == "gmn_10" & Diametre < 14 ~ "R",
      Antibiotique == "gmn_10" ~ "I",

      # Carbapénèmes
      Antibiotique == "ipm_10" & Diametre >= 22 ~ "S",
      Antibiotique == "ipm_10" & Diametre < 16 ~ "R",
      Antibiotique == "ipm_10" ~ "I",

      Antibiotique == "etp_10" & Diametre >= 25 ~ "S",
      Antibiotique == "etp_10" & Diametre < 22 ~ "R",
      Antibiotique == "etp_10" ~ "I",

      # Tétracycline
      Antibiotique == "tet_30" & Diametre >= 18 ~ "S",
      Antibiotique == "tet_30" & Diametre < 15 ~ "R",
      Antibiotique == "tet_30" ~ "I",

      # SXT
      Antibiotique == "sxt_25" & Diametre >= 14 ~ "S",
      Antibiotique == "sxt_25" & Diametre < 11 ~ "R",
      Antibiotique == "sxt_25" ~ "I",

      # Tigécycline
      Antibiotique == "tgc_15" & Diametre >= 18 ~ "S",
      Antibiotique == "tgc_15" & Diametre < 15 ~ "R",
      Antibiotique == "tgc_15" ~ "I",

      # Colistine
      Antibiotique == "col_10" & Diametre >= 15 ~ "S",
      Antibiotique == "col_10" & Diametre < 15 ~ "R",

      TRUE ~ NA_character_
    )
  )
#=========================================
# Ordre des antibiotiques
#=========================================

pheno_long$Antibiotique <- factor(
  pheno_long$Antibiotique,
  levels = c(
    "amo_20",
    "amp_10",
    "tic_75",
    "amc_30",
    "cox_5",
    "caz_10",
    "fep_30",
    "cip_5",
    "ofx_5",
    "gmn_10",
    "ipm_10",
    "etp_10",
    "tet_30",
    "col_10",
    "sxt_25",
    "tgc_15"
  ),
  labels = c(
    "AMO",
    "AMP",
    "TIC",
    "AMC",
    "CTX",
    "CAZ",
    "FEP",
    "CIP",
    "OFX",
    "GEN",
    "IPM",
    "ETP",
    "TET",
    "COL",
    "SXT",
    "TGC"
  )
)

#=========================================
# Heatmap
#=========================================

p=ggplot(
  pheno_long,
  aes(
    x = Antibiotique,
    y = isolat_ID,
    fill = Categorie
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +

  geom_text(
    aes(label = Diametre),
    size = 3,
    fontface = "bold"
  ) +

  scale_fill_manual(
    values = c(
      "S" = "#1B9E77",
      "I" = "#E6AB02",
      "R" = "#D62628"
    ),
    labels = c(
      "S" = "Sensible",
      "I" = "Sensible à forte exposition",
      "R" = "Résistant"
    ),
    name = "Catégorie"
  ) +

  labs(
    title = "Profil phénotypique des isolats",
    #subtitle = "Classification S / I / R",
    x = "Antibiotiques",
    y = "Isolats ID",
    fill = "Légende"
  ) +

  theme_minimal(base_size = 12) +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    panel.grid = element_blank()
  )
plot(p)
