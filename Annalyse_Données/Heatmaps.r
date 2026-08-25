
required <- c("readxl", "dplyr", "tidyr", "stringr", "circlize", "grid")
new <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  }
  BiocManager::install("ComplexHeatmap", update = FALSE, ask = FALSE)
}
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(ComplexHeatmap); library(circlize); library(grid)
})


# --- 1. Lecture antibiogramme (feuille "Resultat") ----------------------
atb_cols <- c("amo_20","amp_10","tic_75","amc_30","cox_5","caz_10","fep_30",
              "cip_5","ofx_5","gmn_10","ipm_10","etp_10","tet_30","sxt_25",
              "tgc_15","col_10")
atb_labels <- c(isolat_ID="isolat_ID",amo_20="Amoxicillin", amp_10="Ampicillin", tic_75="Ticarcillin",
                amc_30="Amox+ acid.clav", cox_5="Cefoxitin", caz_10="Ceftazidime",
                fep_30="Cefepime", cip_5="Ciprofloxacin", ofx_5="Ofloxacin",
                gmn_10="Gentamicin", ipm_10="Imipenem", etp_10="Ertapenem",
                tet_30="Tetracycline", sxt_25="Trim-sulfa",
                tgc_15="Tigecycline", col_10="Colistin")


pheno <- read_excel("DATA_Final.xlsx", sheet = 1)

pheno <- pheno %>%
  filter(!is.na(isolat_ID)) %>%
  select(isolat_ID, all_of(atb_cols)) %>%
  mutate(across(all_of(atb_cols), as.numeric))


#norm_id <- function(x) x %>% str_to_upper() %>%
#  str_remove("^FK-") %>% str_remove("^MV-") %>% str_replace_all("-", ".")
#base_id <- function(x) str_remove(x, "\\.[A-Z]{1,3}$")
#pheno <- pheno %>% mutate(key = norm_id(isolat_ID), base = base_id(key))

# --- 2. Classification S/I/R — seuils EUCAST fournis --------------------
SIR <- pheno %>%
  pivot_longer(all_of(atb_cols), names_to = "Antibiotique", values_to = "Diametre") %>%
  mutate(SIR = case_when(
    Antibiotique == "amo_20" & Diametre >= 19 ~ "S",
    Antibiotique == "amo_20" & Diametre <  19 ~ "R",
    Antibiotique == "amp_10" & Diametre >= 14 ~ "S",
    Antibiotique == "amp_10" & Diametre <  14 ~ "R",
    Antibiotique == "tic_75" & Diametre >= 23 ~ "S",
    Antibiotique == "tic_75" & Diametre <  20 ~ "R",
    Antibiotique == "tic_75" ~ "I",
    Antibiotique == "amc_30" & Diametre >= 19 ~ "S",
    Antibiotique == "amc_30" & Diametre <  19 ~ "R",
    # Céphalosporines
    Antibiotique == "cox_5" & Diametre >= 20 ~ "S",
    Antibiotique == "cox_5" & Diametre <  17 ~ "R",
    Antibiotique == "cox_5" ~ "I",
    Antibiotique == "caz_10" & Diametre >= 22 ~ "S",
    Antibiotique == "caz_10" & Diametre <  19 ~ "R",
    Antibiotique == "caz_10" ~ "I",
    Antibiotique == "fep_30" & Diametre >= 27 ~ "S",
    Antibiotique == "fep_30" & Diametre <  21 ~ "R",
    Antibiotique == "fep_30" ~ "I",
    # Fluoroquinolones
    Antibiotique == "cip_5" & Diametre >= 26 ~ "S",
    Antibiotique == "cip_5" & Diametre <  24 ~ "R",
    Antibiotique == "cip_5" ~ "I",
    Antibiotique == "ofx_5" & Diametre >= 24 ~ "S",
    Antibiotique == "ofx_5" & Diametre <  22 ~ "R",
    Antibiotique == "ofx_5" ~ "I",
    # Aminosides
    Antibiotique == "gmn_10" & Diametre >= 17 ~ "S",
    Antibiotique == "gmn_10" & Diametre <  14 ~ "R",
    Antibiotique == "gmn_10" ~ "I",
    # Carbapénèmes
    Antibiotique == "ipm_10" & Diametre >= 22 ~ "S",
    Antibiotique == "ipm_10" & Diametre <  16 ~ "R",
    Antibiotique == "ipm_10" ~ "I",
    Antibiotique == "etp_10" & Diametre >= 25 ~ "S",
    Antibiotique == "etp_10" & Diametre <  22 ~ "R",
    Antibiotique == "etp_10" ~ "I",
    # Tétracycline
    Antibiotique == "tet_30" & Diametre >= 18 ~ "S",
    Antibiotique == "tet_30" & Diametre <  15 ~ "R",
    Antibiotique == "tet_30" ~ "I",
    # SXT
    Antibiotique == "sxt_25" & Diametre >= 14 ~ "S",
    Antibiotique == "sxt_25" & Diametre <  11 ~ "R",
    Antibiotique == "sxt_25" ~ "I",
    # Tigécycline
    Antibiotique == "tgc_15" & Diametre >= 18 ~ "S",
    Antibiotique == "tgc_15" & Diametre <  15 ~ "R",
    Antibiotique == "tgc_15" ~ "I",
    # Colistine
    Antibiotique == "col_10" & Diametre >= 18 ~ "S",
    Antibiotique == "col_10" & Diametre <  15 ~ "R",
    Antibiotique == "col_10" ~ "I",
    TRUE ~ NA_character_
  ))

#réunie les ligne par ID
SIR <- SIR %>%
  select(isolat_ID,Antibiotique, SIR) %>%
  pivot_wider(names_from = Antibiotique, values_from = SIR)
colnames(SIR) <- atb_labels[colnames(SIR)]

# --- 3. Lecture génotype (feuille "Sheet 1") -----------------------------
geno_raw  <- read_excel("DATA_Final.xlsx", sheet = 3)
geno_raw  <- geno_raw[!apply(is.na(geno_raw), 1, all), c(1,10,8,18:68)]
geno_raw  <- geno_raw[2:17,]
gene_cols <- names(geno_raw)[4:54]
geno=geno_raw
geno[, 4:54] <- ifelse(
  is.na(geno[, 4:54]), 0,
  ifelse(geno[, 4:54] == "C", 1, 2)
)
sp_labels <- c(
  "e-coli" = "E. coli",
  "klebsiella pneumoniae" = "K.pneumoniae",
  "klebsiella oxytoca" = "K.oxytoca",
  "enterobacter cloacae complex  (enterobacter roggenkampii)" = "E.cloacae complex",
  "enterobacter cloacae complex (enterobacter asburiae)" = "E.cloacae complex",

  "serratia fonticola" = "S.fonticola"
)

geno$Gambit <- sp_labels[geno$Gambit]
sel_genes <- gene_cols #permet de modifier les gènes d'interet si besoin.
#geno <- geno_raw %>%
 # filter(!is.na(ID)) %>%
  #mutate(key = norm_id(ID), base = base_id(key)) %>%
  #mutate(across(all_of(gene_cols), ~ as.integer(!is.na(.) & . != "")))


# --- 4. Appariement phénotype / génotype (ID exact, puis base) ----------

fuse <- left_join(geno,SIR, by = names(SIR)[1])
fuse <- as.data.frame(fuse)

pheno_mat  <- as.matrix(fuse[, names(SIR[2:17])]);  rownames(pheno_mat)  <- fuse$isolat_ID
geno_mat <- as.matrix(fuse[, sel_genes]); rownames(geno_mat) <- fuse$isolat_ID
storage.mode(geno_mat) <- "integer"

# --- 5. Palettes Heatmap -----------------------------------------------
sir_colo  <- c("S" = "#639922", "I" = "#EF9F27", "R" = "#D85A30")
gene_colo <- c("0" = "grey92", "1" = "#3C3489","2" = "#3C3489")

###
#CLUSTERISATION
###


atb_order <- c(
  "Amoxicillin", "Ampicillin", "Ticarcillin", "Amox+ acid.clav",
  "Cefoxitin", "Ceftazidime", "Cefepime",
  "Imipenem", "Ertapenem",
  "Ciprofloxacin", "Ofloxacin",
  "Gentamicin",
  "Tetracycline", "Tigecycline",
  "Trim-sulfa",
  "Colistin"
)

# Réordonner les colonnes
sir_mat <- pheno_mat[, atb_order]

# Famille de chaque antibiotique
atb_group <- c(
  rep("Penicillins", 4),
  rep("Cephalosporins", 3),
  rep("Carbapenems", 2),
  rep("Fluoroquinolones", 2),
  "Aminosides",
  rep("Tetracyclines", 2),
  "Sulfonamides",
  "Polymyxins"
)
atb_group <- factor(atb_group, levels = unique(atb_group))
sp_group <- c(
  rep("E.Coli", 8),
  rep("K.Pneumoniae", 3),
  rep("K.Oxytoca", 2),
  rep("E.complexe", 3),
  "S.Fonticola"
)
sp_group <-factor(sp_group, levels = unique(sp_group))

# Heatmaps phéno

library(data.table)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
sm=sir_mat
sm <- as.data.frame(sm)
sm$isolat_ID=row.names(sm)
setDT(sm)
sm2 <- melt(sm,id.vars = "isolat_ID")
df=data.frame(cbind(colnames(sir_mat),atb_group))
colnames(df)=c("variable","group")
df2=data.frame(group=1:8,group_name=c("Penicillins","Cephalosporins","Carbapenems","Fluoroquinolones","Aminosides","Tetracyclines","Sulfonamides","Polymyxins"))
sm2= left_join(sm2,df,by="variable")
sm2$group=as.numeric(sm2$group)
sm2=left_join(sm2,df2,by="group")
sm2=left_join(sm2,geno[,1:3],by="isolat_ID")
sm2$group_name=factor(sm2$group_name,levels = c("Penicillins","Cephalosporins","Carbapenems","Fluoroquinolones","Aminosides","Tetracyclines","Sulfonamides","Polymyxins"))
#Eliminer les ligne N.A ( les souches séquencé plusieurs fois par ex: 29FP.c)
sm2<-sm2 %>% filter(!is.na(value))
ordre_isolats <- sm2 %>%
  distinct(isolat_ID, Gambit) %>%
  arrange(Gambit, isolat_ID) %>%
  pull(isolat_ID)
sm2$isolat_ID <- factor(
  sm2$isolat_ID,
  levels = rev(ordre_isolats)
)
sm2$Gambit<-paste(sm2$Gambit," : ST",sm2$ST)


species_lookup <- sm2 %>%
  distinct(isolat_ID, Gambit) %>%
  deframe()  # vecteur nommé : names = isolat_ID, values = Gambit

# S'assurer que l'ordre correspond aux niveaux du facteur isolat_ID
species_lookup <- species_lookup[levels(sm2$isolat_ID)]

ht_pheno_c <- ggplot(sm2, aes(x = variable, y = isolat_ID, fill = value, label = Gambit)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = sir_colo) +
  scale_y_discrete(
    sec.axis = dup_axis(name = NULL, labels = species_lookup)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    axis.text.y.right = element_text(size = 8,hjust = 0,face = "italic"),
    panel.grid = element_blank(),
    scale_fill_brewer(palette="Set2")
  ) +
  facet_grid(~group_name, scale = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 20)) +
  theme(strip.text = element_text(size = 4)) +
  labs(x = NULL, y = "Isolat")

dev.off()
print(ht_pheno_c)






# --- 6. Heatmaps (phénotype S/I/R + génotype juxtaposés) -----------------
genes_sorted <- c(
  # Penicillins (3)
  "blaSHV-11", "blaTEM-1",
  # Cephalosporins (15)
  "blaACT", "blaACT-109", "blaCTX-M-1", "blaCTX-M-15", "blaFONA-7",
  "blaOKP-A", "blaSFDC", "blaOXY-1-9", "blaSHV-33",
  # Carbapenems (1)
  "blaIMI-1",
  # Fluoroquinolones (2)
  "qnrS1", "qnrB1",
  # Aminosides (7)
  "aadA1", "aadA5", "aph(3'')-Ib", "aph(6)-Id",
  "aadA2", "aph(3')-Ia",
  # Tetracyclines (3)
  "tet(A)", "tet(B)", "tet(D)",
  # Sulfonamides (3)
  "sul2", "sul1",
  # Trimethoprim (3)
  "dfrA14", "dfrA17", "dfrA12",
  # Phenicols (5)
  "catA1", "catA2",
  # Fosfomycin (2)
  "fosA", "fosA2",
  # Macrolides (1)
  "mph(A)",
  # Efflux_multidrug (4)
  "oqxA", "oqxB", "oqxA3", "oqxB19",
  # Quaternary_ammonium (2)
  "qacEdelta1"
)

gene_group <- c(
  rep("Penicillins",2),
  rep("Cephalosporins", 9),
  rep("Carbapenems", 1),
  rep("Fluoroquinolones", 2),
  rep("Aminosides", 7),
  rep("Tetracyclines", 3),
  rep("Sulfonamides", 3),
  rep("Trimethoprim", 3),
  rep("Phenicols", 5),
  rep("Fosfomycin", 2),
  rep("Macrolides", 1),
  rep("Efflux_multidrug", 4),
  rep("Quaternary_ammonium", 2)
)
gene_group <- factor(gene_group, levels = unique(gene_group))
geno_mat <- geno_mat[, genes_sorted]
library(data.table)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
sf=geno_mat
sf <- as.data.frame(sf)
sf$isolat_ID=row.names(sf)
setDT(sf)
sf2 <- melt(sf,id.vars = "isolat_ID")
ds=data.frame(cbind(colnames(geno_mat),gene_group))
colnames(ds)=c("variable","group")
ds2=data.frame(group=1:13,group_name=unique(gene_group))
sf2= left_join(sf2,ds,by="variable")
sf2$group=as.numeric(sf2$group)
sf2=left_join(sf2,ds2,by="group")
sf2=left_join(sf2,geno[,1:3],by="isolat_ID")
sf2$group_name=factor(sf2$group_name,levels = unique(gene_group))
ordre_isolats <- sf2 %>%
  distinct(isolat_ID, Gambit) %>%
  arrange(Gambit, isolat_ID) %>%
  pull(isolat_ID)

sf2$isolat_ID <- factor(
  sf2$isolat_ID,
  levels = rev(ordre_isolats)
)
sf2$Gambit<-paste(sf2$Gambit," : ST",sf2$ST)


species_lookup <- sf2 %>%
  distinct(isolat_ID, Gambit) %>%
  deframe()  # vecteur nommé : names = isolat_ID, values = Gambit

# S'assurer que l'ordre correspond aux niveaux du facteur isolat_ID
species_lookup <- species_lookup[levels(sf2$isolat_ID)]
#Eliminer les ligne N.A ( les souches séquencé plusieurs fois par ex: 29FP.c)
 # vecteur nommé : names = isolat_ID, values = Gambit

ht_geno_c <- ggplot(sf2, aes(x = variable, y = isolat_ID, fill = factor(value), label = Gambit)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(
    data = sf2 %>% filter(value == 1),
    aes(label = "C"),
    color = "white",
    size = 2,
    fontface = "bold",
    key_glyph = "text"
  )+
  scale_fill_manual(
    values = gene_colo,
    name   = "Détection du gène",
    labels = c("0" = "Absent", "1" = "Chromosome", "2" = "Plasmide")  # à ajuster selon le sens réel
  ) +
  scale_y_discrete(
    sec.axis = dup_axis(name = NULL, labels = species_lookup)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    axis.text.y.right = element_text(size = 8, hjust = 0, face = "italic"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 6,angle=0)
  ) +
  facet_grid(~group_name, scale = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 20)) +
  labs(x = NULL, y = "Isolat")
dev.off()
print(ht_geno_c)






