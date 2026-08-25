library(readxl)

library(dplyr)

library(broom)

#setwd("D:/OneDrive/OneDrive - Cirad/stagiaires/KHETIM_Faycal/rapport de stage/code/")

# 2. Read and prepare the data
setwd("/Users/faycalkhetim/Desktop/STAGE/Annalyse Données")
df <- read_excel("Annalyse_risque.xlsx")
# bidouillage pour rendre binaire
df$Marche_Avant[df$Marche_Avant == "Partiel"] <- "Oui"
df$Marche_Avant[df$Marche_Avant == "Oui"] <- 1
df$Marche_Avant[df$Marche_Avant == "Non"] <- 0
df$visite_veto[df$visite_veto == "occsionnelle"] <- "Frequente"
df$Type_traitement[df$Type_traitement == "Mixte"] <- "Collectif"
df$Isolement_Malades[df$Isolement_Malades == "Partiel"] <- "Non"

# 3. Define all your variable categories

farm_vars <- c("Nb_batiment", "Type_elevage", "Nb_porcs", "Nb_Truie",

               "Nb_verrats", "Nb_bandes", "annee_installation")

geo_vars <- c("Distance_Eau_Inf100m", "Elevage_Porcin_Inf500m", "Elevage_Autre_Espece_Inf500m")

animal_vars <- c("Bovin", "Ovin/Caprin", "Volaille", "Chien/chat",

                 "Lapin", "Equide", "Oiseau", "Peroquet")

morbidity_vars <- c("patho_digestive", "patho_respiratoire", "patho_urinaire",

                    "patho_cutanee", "Patho_repro", "Boiterie", "Canibalisme")

atb_vars <- c("atb_penicilline", "Shotapen", "Penijectyl", "atb_amoxicilline",

              "Kelamoxyl", "Vetrimoxin", "atb_phenicoles", "Florfenicol",

              "atb_sulfamides", "Sulfacycline", "atb_Lincosamides", "Lincospectin",

              "atb_macrolide", "PHARMASIN", "HUVEXXIN", "atb_Pleuromutilines",

              "VETMULIN", "atb_Tetracyclines", "LABIMYCIN","Frequence_atb_par_mois","Type_traitement")
netoy_var <- c("Utilisation_Desinfectant","Alternance_Desinfectant","Utilisation_Detergent")
biosecur_var <- c("Marche_Avant","visite_veto","Isolement_Malades","Duree_Quarantaine_(jours)","Type_traitement")

# 4. Prepare the Data

# We create a list of everything EXCEPT the categorical text variables (Type_elevage)

# so we can safely force them to numeric without deleting text data.
df$Utilisation_Desinfectant

all_vars <- c(farm_vars, geo_vars, animal_vars, morbidity_vars, atb_vars,biosecur_var,netoy_var)

numeric_vars <- setdiff(all_vars, c("Type_elevage","visite_veto","Isolement_Malades"))
df_clean <- df %>%

  mutate(

    Outcome_EBSL = as.numeric(`E-BSL`),

    # Safely force all expected numeric columns to numbers

    across(all_of(numeric_vars), ~ as.numeric(as.character(.))),

    # Ensure Type_elevage is treated as a categorical factor

    Type_elevage = as.factor(Type_elevage),
   # Marche_Avant = as.factor(Marche_Avant),
    visite_veto = as.factor(visite_veto),
    Frequence_atb_par_mois = as.factor(Frequence_atb_par_mois),
    Isolement_Malades = as.factor(Isolement_Malades),
    Type_traitement = as.factor(Type_traitement),

  )

# 5. Create an empty list to store the results

results_list <- list()

# 6. Loop through all variables

for (var in all_vars) {



  # Determine the category for the final table

  if (var %in% farm_vars) { cat_name <- "Farm Characteristics" }

  else if (var %in% geo_vars) { cat_name <- "Geographical Characteristics" }

  else if (var %in% netoy_var) { cat_name <- "Cleaning Characteristics"}

  else if (var %in% biosecur_var) { cat_name <- "Biosecurity Practises" }

  else if (var %in% animal_vars) { cat_name <- "Presence of Other Animals" }

  else if (var %in% morbidity_vars) { cat_name <- "Farm Morbidity History" }

  else { cat_name <- "Antibiotic Use History" }



  # Build the formula dynamically

  formula_str <- paste0("Outcome_EBSL ~ `", var, "`")
  #formula_str <- paste0("`",var," ~ Outcome_EBSL`")


  # Fit the model, catching any errors from complete separation

  model <- tryCatch({

    glm(as.formula(formula_str), data = df_clean, family = binomial(link = "logit"))

  }, warning = function(w) {

    suppressWarnings(glm(as.formula(formula_str), data = df_clean, family = binomial(link = "logit")))

  }, error = function(e) NULL)



  # Extract the results

  if (!is.null(model)) {

    # Inside the loop, extract the ORs and ADD the sample size

    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%

      filter(term != "(Intercept)") %>%

      mutate(

        Category = cat_name,

        Variable = var,

        Sub_Level = gsub(paste0("^`?", var, "`?"), "", term),

        N_Observations = nobs(model),  # <--- THIS TELLS YOU THE EXACT SAMPLE SIZE USED

        Crude_OR = round(estimate, 2),

        CI_95_Lower = round(conf.low, 2),

        CI_95_Upper = round(conf.high, 2),

        p_value = round(p.value, 3)

      ) %>%

      select(Category, Variable, Sub_Level, N_Observations, Crude_OR, CI_95_Lower, CI_95_Upper, p_value)



    results_list[[var]] <- res

  }

}
final_table <- bind_rows(results_list)

print(final_table, n = Inf)
#----
tab1 <- as.matrix(table(df_clean$Outcome_EBSL,df_clean$Marche_Avant))
tab2 <- as.matrix(table(df_clean$Outcome_EBSL, df_clean$Distance_Eau_Inf100m))
#-----
#Corrélation
#-------
tab <- table(df_clean$Distance_Eau_Inf100m, df_clean$Marche_Avant)
tab
chisq.test(tab)
fisher.test(tab)


#-------
#Multivariée
#------
library(pwr)
power <- function(tab){

  n0 <- sum(tab[,1])
  n1 <- sum(tab[,2])

  p0 <- tab[2,1] / n0
  p1 <- tab[2,2] / n1

  h <- ES.h(p0, p1)

  pwr.2p2n.test(
    h = h,
    n1 = n0,
    n2 = n1,
    sig.level = 0.05
  )
}
power(tab1)
power(tab2)
# 7. Bind and view the final table
a=final_table[final_table$p_value<0.10,2]
form <- as.formula(paste("Outcome_EBSL ~", a$Variable[1], "+", a$Variable[2]))
mod=glm(form, data = df_clean,family = binomial)

# Tableau OR + IC95%
res <- as.data.frame(exp(cbind(
  OR = coef(mod),
  confint(mod)
)))

colnames(res) <- c("OR", "IC95_inf", "IC95_sup")

# p-values
res$pvalue <- summary(mod)$coefficients[,4]

# Arrondi
res <- round(res, 3)

# Fusion IC
res$`OR (IC95%)` <- paste0(
  res$OR, " (",
  res$IC95_inf, "-",
  res$IC95_sup, ")"
)

# Tableau final
res_final <- res[, c("OR (IC95%)", "pvalue")]
res_final
