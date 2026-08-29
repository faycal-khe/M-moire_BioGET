n <- 12

res <- prop.test(n, 89)
p <- res$estimate
ic <- res$conf.int
cat(
  sprintf(
    "La prévalence observée est de %.1f %% (IC95%% : %.1f - %.1f %%) sur %d échantillons analysés.",
    100*p,
    100*ic[1],
    100*ic[2],
    89
  )
)

# Test binomial exact
res <- binom.test(x = 12, n = 89,p=0.05)

# Résultats
p <- as.numeric(res$estimate)
ic <- res$conf.int

cat(
  sprintf(
    "Prévalence observée : %.1f %% (IC95%% : %.1f - %.1f %%)",
    100*p, 100*ic[1], 100*ic[2]
  )
)
#---------

#prop

#---------

# ==========================
# Vos données
# ==========================

n <- 89           # taille de votre étude
x <- 12           # nombre de positifs observés

n1 <- 30        # taille étude 1
x1 <- 16         # nombre de positifs étude 1

n2 <- 30         # taille étude 2
x2 <- 15       # nombre de positifs étude 2

# ==========================
# Prévalences
# ==========================

p <- x/n
p1 <- x1/n1
p2 <- x2/n2

p
p1
p2
# moyenne
m=x/n
m

#IC
binom.test(x,n)
# ==========================
# Comparaison avec étude 1
# ==========================

test1 <- prop.test(
  x = c(x, x1),
  n = c(n, n1),
  alternative = "two.sided",
  correct = FALSE
)

test1
#===================
# Comparaison avec étude 2
# ==========================

test2 <- prop.test(
  x = c(x, x2),
  n = c(n, n2),
  alternative = "two.sided",
  correct = FALSE
)

test2
#----------



library(ggplot2)

# --- Données ---
# Remplacez par vos valeurs réelles
etudes <- data.frame(
  etude = factor(c("Étude 1", "Étude 2", "Notre étude"),
                 levels = c("Étude 1", "Étude 2", "Notre étude")),
  prevalence = c(p1, p2, x/n),
  n = c(n1, n2, n)
)

#IC
ic <- t(mapply(function(x, n) {
  res <- binom.test(x, n)$conf.int
  c(lower = res[1], upper = res[2])
}, x = c(x1, x2, x), n = c(n1, n2, n)))

etudes$lower <- ic[, "lower"]
etudes$upper <- ic[, "upper"]

# --- p-values issues de vos tests ---
p_test1 <- test1$p.value  # Étude 1 vs Notre étude
p_test2 <- test2$p.value  # Étude 2 vs Notre étude

signif_label <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
}

# --- Graphique ---
ggplot(etudes, aes(x = etude, y = prevalence, fill = etude)) +
  scale_fill_brewer(palette = ("Paired"))+
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  geom_text(aes(label = paste0(round(prevalence*100, 1), "%")),
            vjust = -2.5, size = 4) +
  # Barre de significativité Étude1 vs Notre étude
  annotate("segment", x = 1, xend = 3, y = max(etudes$upper) + 0.05,
           yend = max(etudes$upper) + 0.05) +
  annotate("text", x = 2, y = max(etudes$upper) + 0.07,
           label = signif_label(p_test1), size = 5) +
  # Barre de significativité Étude2 vs Notre étude
  annotate("segment", x = 2, xend = 3, y = max(etudes$upper) + 0.12,
           yend = max(etudes$upper) + 0.12) +
  annotate("text", x = 2.5, y = max(etudes$upper) + 0.14,
           label = signif_label(p_test2), size = 5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(etudes$upper) + 0.2)) +
  labs(title = "Évolution de la prévalence en bactérie résistante aux antibiotique en élevage porcin",
       x = NULL, y = "Prévalence en élevage") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",plot.title= element_text(hjust=0.5) )

#---------

#ANOVA

#---------
library(tidyverse)
library(binom)

region <- c(
  "Saint_Andre_Benoit",
  "Plaine_Palmistes_Sainte_Anne",
  "Salazie",
  "Avirons_Saint_Leu",
  "Etang_Sale_Saint_Louis",
  "Saint_Pierre_Petite_Ile",
  "Saint_Joseph",
  "Trois_Bassins_Saint_Paul",
  "Tampon_Plaine_Cafres"
)

pos <- c(3,2,2,3,3,1,2,0,0)
neg <- c(3,7,14,10,8,9,13,7,4)
df <- data.frame(
  region = region,
  pos = pos,
  neg = neg
)
tab <- df[,2:3]
fisher.test(tab)


# total par région
df$n <- df$pos + df$neg

# prévalence
df$prop <- df$pos / df$n

# IC95%
ic <- binom.confint(
  x = df$pos,
  n = df$n,
  methods = "exact"
)

df$lower <- ic$lower
df$upper <- ic$upper

df
ggplot(df,
       aes(x = reorder(region, prop),
           y = prop)) +

  geom_col(fill = "#E67E22") +

  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    width = 0.15
  ) +

  coord_flip() +

  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "",
    y = "Prévalence par commune",
  )
  #+annotate(
   # "text",
   # x = 2,
    #y = 0.5,
    #label = "Test exact de Fisher : p = 0,47",
    #1hjust = 0
#  )+

  theme_bw()


