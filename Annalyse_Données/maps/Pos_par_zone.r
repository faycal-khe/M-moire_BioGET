# ============================================================
#4.4 total corrigé
# ============================================================
# ============================================================
# Carte du TAUX D'ECHANTILLONNAGE par zone (n échantillonné / n total)
# La Réunion — complémentaire à carte_elevages_reunion.R
# Sources : feuille "Elevage" (échantillonnés) et feuille "AAA"
#           colonne "SECTEUR" (tous les élevages gérés par le vétérinaire)
#
# Couvre les 24 communes de l'île : les zones sans aucun élevage
# (ni échantillonné, ni suivi par le vétérinaire) sont affichées en
# gris avec le libellé "0/0" ; les zones suivies par le vétérinaire
# mais non échantillonnées sont affichées en gris avec "0/n" (sans
# pourcentage) ; les zones échantillonnées sont colorées selon le
# taux, avec le libellé "n/n (pourcentage)".
# ============================================================

# --- 1. Packages -------------------------------------------------
pkgs <- c("readxl", "dplyr", "stringr", "sf", "ggplot2", "scales", "ggrepel")
a_installer <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(a_installer) > 0) install.packages(a_installer)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 2. Chemin du fichier -----------------------------------------

# --- 3. Table de correspondance zone -> commune(s) -----------------
zone_communes <- tibble::tribble(
  ~ville,                                ~commune,
  "SALAZIE",                             "Salazie",
  "SAINT JOSEPH",                        "Saint-Joseph",
  "AVIRONS/ SAINT LEU",                  "Avirons",
  "AVIRONS/ SAINT LEU",                  "Saint-Leu",
  "ETANG SALE/SAINT LOUIS",              "Étang-Salé",
  "ETANG SALE/SAINT LOUIS",              "Saint-Louis",
  "SAINT PIERRE / PETITE ILE",           "Saint-Pierre",
  "SAINT PIERRE / PETITE ILE",           "Petite-Île",
  "PLAINE DES PALMISTES/ SAINTE ANNE",   "Plaine-des-Palmistes",
  "PLAINE DES PALMISTES/ SAINTE ANNE",   "Saint-Benoît",
  "TROIS BASSINS/ SAINT PAUL",           "Trois-Bassins",
  "TROIS BASSINS/ SAINT PAUL",           "Saint-Paul",
  "SAINT ANDRE / SAINT BENOIT",          "Saint-André",
  "LE TAMPON/PLAINE DES CAFRES",         "Tampon",
  "SAINT PHILIPPE",                      "Saint-Philippe",
  "SAINT ANDRE / SAINT BENOIT",          "Bras-Panon",
  "CILAOS",                              "Cilaos",
  "ENTRE-DEUX",                          "Entre-Deux",
  "TROIS BASSINS/ SAINT PAUL",           "Possession",
  "TROIS BASSINS/ SAINT PAUL",           "Port",
  "SAINT-DENIS",                         "Saint-Denis",
  "SAINTE-MARIE",                        "Sainte-Marie",
  "SAINTE-ROSE",                         "Sainte-Rose",
  "SAINT ANDRE / SAINT BENOIT",          "Sainte-Suzanne"
) %>%
mutate(ville = str_squish(ville))

zones_valides <- unique(zone_communes$ville)

# --- 4. Elevages ECHANTILLONNES (feuille "Elevage", colonne "ville") --
elevages <- read_excel("DATA_Final.xlsx", sheet = 2)
n_echantillonne <- elevages %>%
  filter(!is.na(ville)) %>%
  mutate(ville = str_squish(ville)) %>%
  count(ville, name = "n_echantillonne")

# --- 5. Elevages TOTAUX suivis par le vétérinaire (feuille "AAA") -----
#suivi_veto <- read_excel("DATA_Final.xlsx", sheet = 5)

#suivi_veto_clean <- suivi_veto %>%
  #rename(secteur = 3) %>%
  #mutate(secteur = str_squish(secteur))

#n_total <- suivi_veto_clean %>%
  #filter(secteur %in% zones_valides) %>%
  #count(secteur, name = "n_total") %>%
  #rename(ville = secteur)

#secteurs_ecartes <- suivi_veto_clean %>%
  #filter(!is.na(secteur), !secteur %in% zones_valides)
#if (nrow(secteurs_ecartes) > 0) {
  #warning(nrow(secteurs_ecartes), " ligne(s) de la feuille AAA ont un ",
         # "SECTEUR non reconnu et ont été écartées : ",
          #paste(unique(secteurs_ecartes$secteur), collapse = ", "))
#}

# --- 5. Prévalence par zone --
Pos_neg <- read_excel("DATA_Final.xlsx", sheet = 1)
Pos_neg <- Pos_neg[Pos_neg$Pos_neg == 1,2]
Pos_neg<- Pos_neg[1:16,]
Pos_neg <- left_join(Pos_neg, elevages[, c("prelevement_id", "ville")], by = "prelevement_id")
Pos_neg<-table(Pos_neg[,2])
Pos_neg<-as.data.frame(Pos_neg)
names(Pos_neg)[2] <- "n_pos"

toutes_classes <- data.frame(zones_valides)
names(toutes_classes)[1] <- "ville"

Pos_neg <- bind_rows(Pos_neg, toutes_classes) %>%
  distinct(ville, .keep_all = TRUE)
Pos_neg$n_pos[is.na(Pos_neg$n_pos)] <- 0

n_bis <- bind_rows(n_echantillonne, toutes_classes) %>%
  distinct(ville, .keep_all = TRUE)
n_bis$n_echantillonne[is.na(n_bis$n_echantillonne)] <- 0

Pos_neg<-left_join(Pos_neg,n_bis)
Pos_neg$nouvelle_colonne <- "N.A"
# --- 6. Table complète (toutes les zones, même à 0/0) ----------------
taux_zone <- Pos_neg %>%
  mutate(
    taux = ifelse(n_echantillonne > 0, n_pos / n_echantillonne, NA_real_),
    valeur_couleur = ifelse(n_pos > 0, taux, NA_real_),
    label = ifelse(
      n_pos > 0,
      paste0(
        ville, "\n",
        n_pos, "/", n_echantillonne,
        " (", percent(taux, accuracy = 1), ")"
      ),
      paste0(
        ville, "\n",
        n_pos, "/", n_echantillonne
      )
    )
  )

print(taux_zone)

# --- 7. Fond de carte des communes de La Réunion --------------------
communes_reunion <- st_read(
  "https://france-geojson.gregoiredavid.fr/repo/departements/974-la-reunion/communes-974-la-reunion.geojson",
  quiet = TRUE
)

# Jointure "robuste" : on compare les noms en majuscules et sans espaces
# superflus, pour éviter qu'une casse différente entre le fond de carte
# et la table de correspondance ne fasse disparaître une commune (trou
# sur la carte). Le nom affiché reste celui du fond de carte ("nom").
normaliser <- function(x) str_squish(str_to_upper(x))

communes_reunion <- communes_reunion %>%
  mutate(commune_key = normaliser(nom))

zone_communes <- zone_communes %>%
  mutate(commune_key = normaliser(commune))

# Vérification : communes du fond de carte sans correspondance
communes_non_couvertes <- setdiff(communes_reunion$commune_key, zone_communes$commune_key)
if (length(communes_non_couvertes) > 0) {
  warning("Communes du fond de carte non reconnues dans zone_communes (",
          "vérifiez l'orthographe exacte) : ",
          paste(communes_non_couvertes, collapse = ", "))
}

communes_zone <- communes_reunion %>%
  left_join(zone_communes %>% select(commune_key, ville), by = "commune_key") %>%
  left_join(taux_zone, by = "ville")

# --- 8. Dissolution des communes en zones ---------------------------
# IMPORTANT : on garde TOUJOURS le fond de carte complet (communes_reunion)
# en couche de base, même pour les communes qui n'auraient pas matché
# (ville = NA) -> plus aucun "trou" possible sur la carte, elles
# apparaîtront simplement sans étiquette, en gris clair.
zones_sf <- communes_zone %>%
  filter(!is.na(ville)) %>%
  group_by(ville, n_pos, n_echantillonne, taux, valeur_couleur, label) %>%
  summarise(.groups = "drop")

zones_centroides <- st_centroid(zones_sf) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

# --- 9. Carte du taux d'échantillonnage ------------------------------
palette_carte1 <- c("#fee8c8", "#fdbb84", "#e34a33", "#7f0000")

p=ggplot() +
  geom_sf(data = communes_reunion, fill = "grey93", color = "white", linewidth = 0.3) +
  geom_sf(data = zones_sf, aes(fill = valeur_couleur), color = "white", linewidth = 0.4) +
  geom_label(
    data = zones_centroides,
    aes(x = lon, y = lat, label = label),
    size = 2.3, label.size = 0, fill = alpha("white", 0.8), color = "grey15",
    label.padding = unit(0.15, "lines"),
    box.padding = 0.4, point.padding = 0.1,
    min.segment.length = 0, segment.color = "grey50", segment.size = 0.3,
    max.overlaps = Inf, seed = 1
  ) +
  scale_fill_gradientn(
    colours = palette_carte1,
    limits = c(0, 1),
    labels = percent,
    na.value = "grey93",
    name = "Taux\nd'échantillonnage"
  ) +
  labs(
    title = "Taux d'échantillonnage par zone — La Réunion",
    subtitle = "Élevages échantillonnés / élevages suivis par le vétérinaire (feuille AAA)\nEn gris : zones non échantillonnées (0/n) ou sans élevage recensé (0/0)",
    caption = "Sources : DATA_Final.xlsx, feuilles \"Elevage\" et \"AAA\""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave("/Users/faycalkhetim/Desktop/STAGE/Annalyse Données/maps/carte_taux_pos.png",plot = p, width = 12, height = 10, dpi = 320, bg = "white")
plot(p)
