#=========================================================================
# ANALYSE DES FACTEURS DE RISQUE — Bactéries résistantes (ESBL / E-CP)
# Source : DATA_Final.xlsx, feuille "Annalyse Facteur de risque"
#
# Protocole :
#   1. Analyse descriptive (prévalences + description des élevages)
#   2. Analyse univariée (Variables qualitatives khi2/Fisher ; Variables quantitatives Shapiro puis Student/Mann-Whitney)
#      -> variables retenues si p < 0.20 Le seuil p < 0,20 a été choisi pour la présélection des variables candidates au modèle multivarié afin de ne pas écarter des facteurs potentiellement associés au résultat et susceptibles de devenir significatifs après ajustement
#   3. Vérification de colinéarité (VIF, seuil 5-10)
#   4. Analyse multivariée (régression logistique, OR ajustés, IC95%) Si le nombre de cas positifs est faible (< 10), utilisation d'une régression logistique de Firth.
#   5. Validation du modèle (Hosmer-Lemeshow, AUC ROC)
#   6. Analyses secondaires : Global (ESBL ou E-CP), ESBL seul, E-CP seul
#      -> régression de Firth si peu de cas positifs
#=========================================================================

# --- 0. Packages ---------------------------------------------------------
pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "purrr", "tibble",
          "broom", "car", "pROC", "ResourceSelection", "logistf")
a_installer <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(a_installer) > 0) install.packages(a_installer)
invisible(lapply(pkgs, library, character.only = TRUE))




# =========================================================================
# 1. LECTURE ET RECONSTRUCTION DES NOMS DE COLONNES (en-tête sur 3 lignes)
# =========================================================================
# Ligne 1 = catégorie (IDENTIFICATION, BIOSECURITE, ...) -> ignorée
# Ligne 2 = nom de variable (NA si colonne "enfant" d'un groupe)
# Ligne 3 = sous-modalité (Oui/Non générique, ou nom de sous-catégorie
#           comme "Bovin", "Shotapen", "E-CP"...)

entete <- read_excel("DATA Final.xlsx", sheet = 4, range = cell_rows(2:3), col_names = FALSE)
ligne_var <- as.character(entete[1, ])
ligne_sub <- as.character(entete[2, ])

# Complète les noms de variable manquants (colonnes "enfants" d'un groupe)
# (équivalent d'un "forward fill" / na.locf, en base R)
remplir_avant <- function(x) {
  dernier <- NA_character_
  vapply(x, function(v) {
    if (!is.na(v) && v != "NA") dernier <<- v
    dernier
  }, character(1))
}
ligne_var_remplie <- remplir_avant(ligne_var)

nettoyer <- function(x) {
  x <- str_squish(x)
  # Retire les accents (les noms de variables serviront dans des formules R,
  # plus sûr sans caractères accentués)
  x <- chartr("àâäÀÂÄéèêëÉÈÊËîïÎÏôöÔÖùûüÙÛÜçÇ",
              "aaaAAAeeeeEEEEiiIIooOOuuuUUUcC", x)
  x <- str_replace_all(x, "[^A-Za-z0-9_()'-]", "")
  x <- str_replace_all(x, "[ '()/-]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x
}

nom_colonne <- purrr::map2_chr(ligne_var_remplie, ligne_sub, function(var, sub) {
  var <- nettoyer(var)
  if (is.na(sub) || sub %in% c("Oui/Non", "NA", "")) {
    var
  } else {
    paste0(var, "_", nettoyer(sub))
  }
})
nom_colonne <- make.unique(nom_colonne, sep = "_")

data_brute <- read_excel("DATA Final.xlsx", sheet = 4, skip = 3, col_names = nom_colonne)

nrow(data_brute)
length(unique(data_brute$Elevage_ID))
cat("Dimensions des données :", nrow(data_brute), "élevages x", ncol(data_brute), "colonnes\n")

# =========================================================================
# 2. VARIABLES DE RESULTAT (OUTCOMES)
# =========================================================================
# D = "Bactérie résistante_E-BSL" (ESBL), E = "Bactérie résistante_E-CP"
noms_esbl <- names(data_brute)[str_detect(names(data_brute), "E_BSL|E_BLSE")]
noms_ecp  <- names(data_brute)[str_detect(names(data_brute), "E_CP$")]
stopifnot(length(noms_esbl) == 1, length(noms_ecp) == 1)

data_brute <- data_brute %>%
  rename(ESBL = all_of(noms_esbl), ECP = all_of(noms_ecp)) %>%
  mutate(
    ESBL = as.integer(ESBL),
    ECP  = as.integer(ECP),
    GLOBAL = as.integer(ESBL == 1 | ECP == 1)
  )

# =========================================================================
# 3. SELECTION DES VARIABLES EXPLICATIVES CANDIDATES
# =========================================================================
# Exclues : identifiant, date, outcomes eux-mêmes, et variables texte libre
# à trop de modalités (noms de produits commerciaux) qui ne sont pas
# exploitables en khi2/Fisher.
vars_exclues_manuelles <- c("Elevage_ID", "Date", "ESBL", "ECP", "GLOBAL")

MAX_MODALITES <- 10  # seuil au-delà duquel une variable qualitative est écartée

classer_variable <- function(x) {
  x_non_na <- x[!is.na(x)]
  if (length(x_non_na) == 0) return("vide")
  if (is.numeric(x_non_na) && length(unique(x_non_na)) > 2) return("quantitative")
  "qualitative"
}

types_variables <- data_brute %>%
  select(-all_of(vars_exclues_manuelles)) %>%
  map_chr(classer_variable)

vars_quanti <- names(types_variables[types_variables == "quantitative"])
vars_quali  <- names(types_variables[types_variables == "qualitative"])

# Ecarte les qualitatives à trop de modalités, ou sans variance
vars_quali_ecartees <- vars_quali[map_int(data_brute[vars_quali], ~ n_distinct(.x, na.rm = TRUE)) > MAX_MODALITES |
                                    map_int(data_brute[vars_quali], ~ n_distinct(.x, na.rm = TRUE)) < 2]
vars_quali <- setdiff(vars_quali, vars_quali_ecartees)

if (length(vars_quali_ecartees) > 0) {
  cat("Variables qualitatives écartées (>", MAX_MODALITES, "modalités, ou sans variance) :\n")
  cat(paste(" -", vars_quali_ecartees), sep = "\n")
}

vars_candidates <- c(vars_quanti, vars_quali)
cat("\n", length(vars_candidates), "variables explicatives candidates (",
    length(vars_quanti), "quantitatives,", length(vars_quali), "qualitatives)\n\n")

# =========================================================================
# 4. FONCTIONS DU PROTOCOLE
# =========================================================================

## --- 4.1 Analyse descriptive ------------------------------------------
analyse_descriptive <- function(data, outcome, label_outcome) {
  cat("====================================================\n")
  cat("ANALYSE DESCRIPTIVE —", label_outcome, "\n")
  cat("====================================================\n")
  n <- nrow(data)
  n_pos <- sum(data[[outcome]] == 1, na.rm = TRUE)
  cat(sprintf("Prévalence %s : %d / %d élevages = %.1f%%\n\n",
              label_outcome, n_pos, n, 100 * n_pos / n))

  cat("--- Description des élevages (variables quantitatives) ---\n")
  desc_quanti <- data %>%
    select(all_of(vars_quanti)) %>%
    summarise(across(everything(),
                     list(n = ~sum(!is.na(.x)),
                          moyenne = ~mean(.x, na.rm = TRUE),
                          ecart_type = ~sd(.x, na.rm = TRUE),
                          mediane = ~median(.x, na.rm = TRUE)),
                     .names = "{.col}__{.fn}")) %>%
    pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
    pivot_wider(names_from = stat, values_from = value)
  print(desc_quanti, n = Inf)

  cat("\n--- Description des élevages (variables qualitatives, %) ---\n")
  for (v in vars_quali) {
    cat("\n", v, ":\n")
    print(round(prop.table(table(data[[v]], useNA = "ifany")) * 100, 1))
  }
  invisible(desc_quanti)
}

## --- 4.2 Analyse univariée ---------------------------------------------
test_univarie <- function(data, var, outcome) {
  x <- data[[var]]
  y <- data[[outcome]]
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]

  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(tibble(variable = var, test = "ignoré (sans variance)", p_value = NA_real_))
  }

  if (var %in% vars_quanti) {
    # Normalité par groupe (Shapiro-Wilk)
    p_shapiro <- tryCatch({
      s0 <- if (sum(y == 0) >= 3 && sum(y == 0) <= 5000) shapiro.test(x[y == 0])$p.value else NA
      s1 <- if (sum(y == 1) >= 3 && sum(y == 1) <= 5000) shapiro.test(x[y == 1])$p.value else NA
      c(s0, s1)
    }, error = function(e) c(NA, NA))

    normal <- all(!is.na(p_shapiro)) && all(p_shapiro > 0.05)

    if (normal) {
      test <- tryCatch(t.test(x ~ y), error = function(e) NULL)
      nom_test <- "Student (t-test)"
    } else {
      test <- tryCatch(wilcox.test(x ~ y), error = function(e) NULL)
      nom_test <- "Mann-Whitney"
    }
    p <- if (!is.null(test)) test$p.value else NA_real_

  } else {
    tab <- table(x, y)
    attendus_ok <- tryCatch({
      chi <- suppressWarnings(chisq.test(tab))
      all(chi$expected >= 5)
    }, error = function(e) FALSE)

    if (attendus_ok) {
      test <- suppressWarnings(chisq.test(tab))
      nom_test <- "Khi2"
    } else {
      test <- tryCatch(
        fisher.test(tab, simulate.p.value = nrow(tab) > 2 || ncol(tab) > 2),
        error = function(e) NULL
      )
      nom_test <- "Fisher exact"
    }
    p <- if (!is.null(test)) test$p.value else NA_real_
  }

  tibble(variable = var, test = nom_test, p_value = p)
}

analyse_univariee <- function(data, outcome) {
  map_dfr(vars_candidates, ~ test_univarie(data, .x, outcome)) %>%
    arrange(p_value)
}

## --- 4.3 Vérification de colinéarité (VIF) ------------------------------
reduire_colinearite <- function(data, vars, outcome, seuil = 5) {
  vars_restantes <- vars
  repeat {
    if (length(vars_restantes) < 2) break
    formule <- as.formula(paste(outcome, "~", paste(vars_restantes, collapse = " + ")))
    modele <- tryCatch(glm(formule, data = data, family = binomial), error = function(e) NULL)
    if (is.null(modele)) break

    vif_vals <- tryCatch(car::vif(modele), error = function(e) NULL)
    if (is.null(vif_vals)) break

    # car::vif renvoie un vecteur (numérique) ou une matrice (facteurs à >2 niveaux, colonne GVIF)
    if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]

    if (max(vif_vals) <= seuil) break
    var_a_retirer <- names(which.max(vif_vals))
    cat("Retrait pour colinéarité (VIF =", round(max(vif_vals), 1), ") :", var_a_retirer, "\n")
    vars_restantes <- setdiff(vars_restantes, var_a_retirer)
  }
  vars_restantes
}

## --- 4.4 Analyse multivariée --------------------------------------------
modele_multivarie <- function(data, vars, outcome, firth = FALSE) {
  formule <- as.formula(paste(outcome, "~", paste(vars, collapse = " + ")))

  if (firth) {
    modele <- logistf::logistf(formule, data = data)
    resultats <- tibble(
      variable = names(coef(modele)),
      OR = exp(coef(modele)),
      IC95_inf = exp(modele$ci.lower),
      IC95_sup = exp(modele$ci.upper),
      p_value = modele$prob
    )
  } else {
    modele <- glm(formule, data = data, family = binomial)
    ic <- suppressMessages(confint(modele))
    resultats <- tibble(
      variable = names(coef(modele)),
      OR = exp(coef(modele)),
      IC95_inf = exp(ic[, 1]),
      IC95_sup = exp(ic[, 2]),
      p_value = summary(modele)$coefficients[, "Pr(>|z|)"]
    )
  }
  list(modele = modele, resultats = resultats)
}

## --- 4.5 Validation du modèle -------------------------------------------
valider_modele <- function(modele, data, outcome) {
  if (inherits(modele, "logistf")) {
    cat("Modèle de Firth : pas de test de Hosmer-Lemeshow / AUC standard ",
        "(effectifs trop faibles) — se limiter aux OR/IC95%.\n")
    return(invisible(NULL))
  }
  cat("\n--- Test de Hosmer-Lemeshow ---\n")
  hl <- tryCatch(
    ResourceSelection::hoslem.test(modele$y, fitted(modele), g = 10),
    error = function(e) NULL
  )
  if (!is.null(hl)) print(hl) else cat("Non calculable (effectifs trop faibles).\n")
  cat("\n--- Aire sous la courbe ROC (AUC) ---\n")

  mf <- model.frame(modele)

  roc_obj <- pROC::roc(
    response = model.response(mf),
    predictor = fitted(modele),
    quiet = TRUE
  )

  cat("AUC =", round(as.numeric(pROC::auc(roc_obj)), 3), "\n")
  #cat("\n--- Aire sous la courbe ROC (AUC) ---\n")
 # roc_obj <- pROC::roc(data[[outcome]], fitted(modele), quiet = TRUE)
  #cat("AUC =", round(as.numeric(pROC::auc(roc_obj)), 3), "\n")
  #plot(roc_obj, main = paste("Courbe ROC —", outcome))
  #invisible(roc_obj)
}

# =========================================================================
# 5. PIPELINE COMPLET POUR UN OUTCOME DONNE
# =========================================================================
analyse_facteurs_risque <- function(data, outcome, label_outcome, seuil_firth = 10) {

  analyse_descriptive(data, outcome, label_outcome)

  cat("\n====================================================\n")
  cat("ANALYSE UNIVARIEE —", label_outcome, "\n")
  cat("====================================================\n")
  resultats_univaries <- analyse_univariee(data, outcome)
  print(resultats_univaries, n = Inf)

  vars_retenues <- resultats_univaries %>%
    filter(!is.na(p_value), p_value < 0.20) %>%
    pull(variable)
  cat("\n", length(vars_retenues), "variables retenues (p < 0.20) :\n")
  cat(paste(" -", vars_retenues), sep = "\n")

  if (length(vars_retenues) == 0) {
    cat("\nAucune variable retenue : analyse multivariée non réalisable.\n")
    return(invisible(NULL))
  }

  cat("\n====================================================\n")
  cat("VERIFICATION DE COLINEARITE (VIF) —", label_outcome, "\n")
  cat("====================================================\n")
  vars_finales <- reduire_colinearite(data, vars_retenues, outcome)
  cat("\nVariables finales retenues pour le modèle :\n")
  cat(paste(" -", vars_finales), sep = "\n")

  n_positifs <- sum(data[[outcome]] == 1, na.rm = TRUE)
  utiliser_firth <- n_positifs < seuil_firth
  if (utiliser_firth) {
    cat("\n(", n_positifs, "cas positifs < ", seuil_firth,
        "-> régression de Firth utilisée à la place du GLM standard)\n")
  }

  cat("\n====================================================\n")
  cat("ANALYSE MULTIVARIEE —", label_outcome, "\n")
  cat("====================================================\n")
  resultat_modele <- modele_multivarie(data, vars_finales, outcome, firth = utiliser_firth)
  print(resultat_modele$resultats, n = Inf)

  cat("\n====================================================\n")
  cat("VALIDATION DU MODELE —", label_outcome, "\n")
  cat("====================================================\n")
  valider_modele(resultat_modele$modele, data, outcome)

  list(
    univarie = resultats_univaries,
    vars_finales = vars_finales,
    modele = resultat_modele$modele,
    resultats = resultat_modele$resultats
  )
}

# =========================================================================
# 6. EXECUTION : GLOBAL, ESBL SEUL, E-CP SEUL
# =========================================================================
resultats_global <- analyse_facteurs_risque(data_brute, "GLOBAL", "ESBL ou E-CP (global)")
#resultats_esbl   <- analyse_facteurs_risque(data_brute, "ESBL",   "ESBL seul")
#resultats_ecp    <- analyse_facteurs_risque(data_brute, "ECP",    "E-CP seul")
