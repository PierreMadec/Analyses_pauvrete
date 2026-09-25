# ==============================================================================
# oaxaca_tp.R
#
# Décomposition Oaxaca-Blinder (2 voies, coefficients poolés) de l'évolution
# de la pauvreté laborieuse entre deux périodes.
#
# Périodes :
#   t1 = 2016-2018  (avant la remontée du taux de pauvreté laborieuse)
#   t2 = 2021-2023  (période récente de hausse)
#
# Question : la hausse du taux de pauvreté parmi les travailleurs (PR en emploi)
# est-elle due à un effet de COMPOSITION (profil des travailleurs) ou à un
# effet de COEFFICIENTS (même profil → plus de pauvreté) ?
#
# Méthode : décomposition de Oaxaca-Blinder à 2 voies avec coefficients poolés
# (Fortin, Lemieux & Firpo 2011 ; Cotton 1988 ; Reimers 1983).
# On utilise β_pool (LPM estimé sur t1+t2 combinés) comme référence pour
# éviter le terme d'interaction et le biais de choix du groupe de référence.
#
#   ΔȲ = (X̄₂ - X̄₁)β_pool          [effet composition "explained"]
#      + [X̄₂(β₂-β_pool) + X̄₁(β_pool-β₁)]  [effet coefficients "unexplained"]
#
# Les deux termes somment à ΔȲ sans terme d'interaction résiduel.
#
# Variables : biactivite + typmen (pas acteu_cj, toujours défini),
#             age_cat, sexe_cat, diplome, immi_cat (si disponibles).
#
# Figures produites :
#   oaxaca_tp_barre   — décomposition globale composition / coefficients
#   oaxaca_tp_detail  — effet composition détaillé par variable
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

# ==============================================================================
# 1. Base : PR en emploi, 18-64 ans, avec configuration ménage via biactivite
# ==============================================================================

.pm  <- read.csv("data/parametres_macro.csv")
.ipc <- setNames(.pm$ipc, as.character(.pm$annee))
.s05 <- seuils_annuels$seuil_std[seuils_annuels$annee == 2005]

base_oax <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi",
         age_num >= 18, age_num <= 64) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std), !is.na(typmen), !is.na(biactivite)) |>
  mutate(
    pauvre = as.integer(nivviem < seuil_std),
    # seuil ancré : 60 % de la médiane de 2005, indexé sur l'inflation
    seuil_anc  = as.numeric(.s05 * .ipc[as.character(annee)] / .ipc["2005"]),
    pauvre_anc = as.integer(nivviem < seuil_anc),
    # Configuration ménage robuste (biactivite toujours défini pour PR en emploi)
    config = case_when(
      typmen == "Personne seule"           ~ "Seul sans enfant",
      typmen == "Famille monoparentale"    ~ "Parent seul",
      typmen == "Couple sans enfant"   & biactivite == "Bi-actif" ~ "Couple bi-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite == "Bi-actif" ~ "Couple bi-actif av.enf.",
      typmen == "Couple sans enfant"   & biactivite != "Bi-actif" ~ "Couple mono-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite != "Bi-actif" ~ "Couple mono-actif av.enf.",
      TRUE ~ "Autre"
    )
  ) |>
  filter(config != "Autre")

# Diplôme et PCS harmonisés (cf. R/extract_dipl_pcs.R)
f_dp <- file.path(path_fig, "dipl_pcs.rds")
if (!file.exists(f_dp) && dir.exists("/Users/pierremadec/Documents/ERFS_backup")) {
  source("R/extract_dipl_pcs.R", local = FALSE)
}
if (file.exists(f_dp)) {
  base_oax <- base_oax |>
    mutate(ident = as.character(ident)) |>
    left_join(readRDS(f_dp) |> mutate(ident = as.character(ident)),
              by = c("annee", "ident"))
}

# Périodes de comparaison
annees_t1 <- 2010:2011   # période de référence (fenêtre de deux ans, cf. annees_t2)
annees_t2 <- 2023:2024   # période récente
annees_dispo <- unique(base_oax$annee)
annees_t1 <- annees_t1[annees_t1 %in% annees_dispo]
annees_t2 <- annees_t2[annees_t2 %in% annees_dispo]

if (length(annees_t1) == 0 || length(annees_t2) == 0) {
  message("Périodes t1 ou t2 non disponibles — oaxaca_tp ignoré.")
  stop("Années manquantes")
}

periodes_label <- sprintf("%d-%d vs %d-%d",
  min(annees_t1), max(annees_t1), min(annees_t2), max(annees_t2))
cat(sprintf("Comparaison : %s\n", periodes_label))

d1 <- base_oax |> filter(annee %in% annees_t1)
d2 <- base_oax |> filter(annee %in% annees_t2)

# ==============================================================================
# 2. Sélection dynamique des covariables disponibles dans les deux périodes
# ==============================================================================

# Spécification retenue : on contrôle la qualification (diplôme, PCS), sans quoi
# l'effet de composition est biaisé — la population en emploi s'est fortement
# qualifiée sur la période. `diplome` (data_all) n'existe qu'à partir de 2017 :
# c'est `dipl_h` (harmonisé, cf. extract_dipl_pcs.R) qui est utilisé.
vars_cand <- c("config", "age_cat", "sexe_cat", "immi_cat",
               "statut_occ", "dipl_h", "pcs_h")

ok_dans <- function(df, v) {
  if (!v %in% names(df)) return(FALSE)
  col <- df[[v]]
  if (mean(is.na(col)) >= 0.5) return(FALSE)
  if (is.factor(col) || is.character(col))
    length(unique(na.omit(as.character(col)))) > 1
  else
    var(col, na.rm = TRUE) > 0
}

vars_ok <- vars_cand[sapply(vars_cand, function(v) ok_dans(d1, v) && ok_dans(d2, v))]
cat(sprintf("Covariables retenues : %s\n", paste(vars_ok, collapse = ", ")))

if (length(vars_ok) == 0) stop("Aucune covariable disponible")

formule <- as.formula(paste("pauvre ~", paste(vars_ok, collapse = " + ")))

# Nettoyage listwise
nettoyer <- function(df) {
  df |>
    select(pauvre, wprm, all_of(vars_ok)) |>
    filter(if_all(all_of(vars_ok), ~!is.na(.))) |>
    mutate(wprm = wprm / mean(wprm))
}
d1c <- nettoyer(d1)
d2c <- nettoyer(d2)
cat(sprintf("n t1 = %d  |  n t2 = %d\n", nrow(d1c), nrow(d2c)))

# ==============================================================================
# 3. Estimation LPM (période 1, période 2, poolée)
# ==============================================================================

m1   <- lm(formule, data = d1c, weights = wprm)
m2   <- lm(formule, data = d2c, weights = wprm)
m_pl <- lm(formule, data = bind_rows(
              d1c |> mutate(wprm = wprm * nrow(d1c) / (nrow(d1c) + nrow(d2c))),
              d2c |> mutate(wprm = wprm * nrow(d2c) / (nrow(d1c) + nrow(d2c)))
           ), weights = wprm)

# Aligner matrices de design sur les colonnes communes
X1 <- model.matrix(formule, data = d1c)
X2 <- model.matrix(formule, data = d2c)
cols_comm <- intersect(colnames(X1), colnames(X2))
X1 <- X1[, cols_comm, drop = FALSE]
X2 <- X2[, cols_comm, drop = FALSE]

w1 <- d1c$wprm / sum(d1c$wprm)
w2 <- d2c$wprm / sum(d2c$wprm)
Xbar1 <- colSums(X1 * w1)
Xbar2 <- colSums(X2 * w2)

b1   <- replace(coef(m1)[cols_comm],   is.na(coef(m1)[cols_comm]),   0)
b2   <- replace(coef(m2)[cols_comm],   is.na(coef(m2)[cols_comm]),   0)
bpl  <- replace(coef(m_pl)[cols_comm], is.na(coef(m_pl)[cols_comm]), 0)

# ==============================================================================
# 4. Décomposition 2-voies avec coefficients poolés (sans terme d'interaction)
#
#   ΔȲ = composition + coefficients
#   composition  = (X̄₂ - X̄₁) · β_pool
#   coefficients = ΔȲ - composition
#                = X̄₂(β₂ - β_pool) + X̄₁(β_pool - β₁)
# ==============================================================================

gap_obs     <- weighted.mean(d2c$pauvre, d2c$wprm) -
               weighted.mean(d1c$pauvre, d1c$wprm)
composition  <- as.numeric((Xbar2 - Xbar1) %*% bpl)
coefficients_ <- gap_obs - composition

cat(sprintf(
  "\nGap observé  : %+.3f pts\nComposition  : %+.3f pts\nCoefficients : %+.3f pts\n",
  gap_obs * 100, composition * 100, coefficients_ * 100
))

# ==============================================================================
# 5. Décomposition de l'effet composition par variable (avec β_pool)
# ==============================================================================

variable_de_col <- function(col, vars) {
  for (v in vars) if (col == v || startsWith(col, v)) return(v)
  NA_character_
}

contrib_compo_detail <- tibble(
  col     = cols_comm,
  delta_X = Xbar2 - Xbar1,
  beta_pl = bpl,
  contrib = (Xbar2 - Xbar1) * bpl * 100
) |>
  mutate(variable = sapply(col, variable_de_col, vars = vars_ok)) |>
  filter(!is.na(variable)) |>
  group_by(variable) |>
  summarise(contrib = sum(contrib), .groups = "drop") |>
  arrange(contrib) |>
  mutate(
    labs_vars = case_when(
      variable == "config"   ~ "Configuration du ménage",
      variable == "age_cat"  ~ "Âge",
      variable == "sexe_cat" ~ "Sexe",
      variable == "diplome"  ~ "Diplôme",
      variable == "immi_cat" ~ "Origine migratoire",
      variable == "statut_occ" ~ "Statut d'occupation du logement",
      variable == "dipl_h"   ~ "Diplôme",
      variable == "pcs_h"    ~ "Catégorie socioprofessionnelle",
      TRUE ~ variable
    ),
    labs_vars = fct_reorder(labs_vars, contrib),
    sens      = if_else(contrib >= 0,
                        "Hausse de la pauvreté laborieuse",
                        "Baisse de la pauvreté laborieuse"),
    tooltip   = paste0(labs_vars, "\nContribution : ", sprintf("%+.2f", contrib), " pts de %"),
    data_id   = paste0("comp_", variable)
  )

# ==============================================================================
# 6. Figure A : décomposition aux DEUX seuils (relatif et ancré)
#
# L'effet de composition ne dépend pas du seuil retenu (les caractéristiques de
# la population sont les mêmes) : c'est le résultat robuste. L'effet de
# coefficients, lui, change de signe — il mesure la course entre le revenu d'un
# profil donné et la médiane, et prolonge au niveau individuel l'écart déjà
# visible entre les deux courbes de la figure « seuil relatif / seuil ancré ».
# ==============================================================================

decomposer <- function(y) {
  net <- function(df) df |>
    transmute(pauvre = .data[[y]], wprm, across(all_of(vars_ok))) |>
    filter(if_all(all_of(vars_ok), ~!is.na(.)), !is.na(pauvre)) |>
    mutate(wprm = wprm / mean(wprm))
  a <- net(d1); b <- net(d2)
  f <- as.formula(paste("pauvre ~", paste(vars_ok, collapse = " + ")))
  mp <- lm(f, bind_rows(a |> mutate(wprm = wprm * nrow(a) / (nrow(a) + nrow(b))),
                        b |> mutate(wprm = wprm * nrow(b) / (nrow(a) + nrow(b)))),
           weights = wprm)
  Xa <- model.matrix(f, a); Xb <- model.matrix(f, b)
  cc <- intersect(colnames(Xa), colnames(Xb))
  Xba <- colSums(Xa[, cc, drop = FALSE] * (a$wprm / sum(a$wprm)))
  Xbb <- colSums(Xb[, cc, drop = FALSE] * (b$wprm / sum(b$wprm)))
  bp  <- replace(coef(mp)[cc], is.na(coef(mp)[cc]), 0)
  gap <- weighted.mean(b$pauvre, b$wprm) - weighted.mean(a$pauvre, a$wprm)
  comp <- as.numeric((Xbb - Xba) %*% bp)
  c(gap = gap, composition = comp, coefficients = gap - comp)
}

dec_rel <- decomposer("pauvre")
dec_anc <- decomposer("pauvre_anc")
cat(sprintf("\nSeuil relatif : gap %+0.2f | comp %+0.2f | coef %+0.2f\n",
            100*dec_rel["gap"], 100*dec_rel["composition"], 100*dec_rel["coefficients"]))
cat(sprintf("Seuil ancré   : gap %+0.2f | comp %+0.2f | coef %+0.2f\n",
            100*dec_anc["gap"], 100*dec_anc["composition"], 100*dec_anc["coefficients"]))

decomp_global <- tibble(
  effet  = factor(rep(c("Composition\n(profil des travailleurs)",
                        "Coefficients\n(à profil donné)"), 2),
                  levels = c("Composition\n(profil des travailleurs)",
                             "Coefficients\n(à profil donné)")),
  seuil  = factor(rep(c("Seuil relatif", "Seuil ancré (pouvoir d'achat constant)"), each = 2),
                  levels = c("Seuil relatif", "Seuil ancré (pouvoir d'achat constant)")),
  valeur = 100 * c(dec_rel["composition"], dec_rel["coefficients"],
                   dec_anc["composition"], dec_anc["coefficients"])
) |>
  mutate(tooltip = sprintf("%s — %s : %+.2f pt", gsub("\n", " ", effet), seuil, valeur),
         data_id = paste0(effet, seuil))

g_oaxaca_barre <- ggplot(decomp_global, aes(x = effet, y = valeur, fill = seuil)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(aes(label = sprintf("%+.1f", valeur),
                vjust = ifelse(valeur >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.7), size = 3.2, colour = "grey20") +
  scale_fill_manual(values = c("Seuil relatif" = "#2674DD",
                               "Seuil ancré (pouvoir d'achat constant)" = "#8D30D4"),
                    name = NULL) +
  scale_y_continuous(labels = label_number(suffix = " pts"), expand = expansion(mult = 0.16)) +
  labs(
    x = NULL, y = "contribution à la variation du taux de pauvreté (points)",
    caption = sprintf(
      "Source : INSEE, ERFS, calculs de l'auteur.\n%s. Champ : PR en emploi, 18-64 ans. Variation totale : %+.2f pt au seuil relatif, %+.2f pt au seuil ancré.\n",
      periodes_label, 100*dec_rel["gap"], 100*dec_anc["gap"])
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g_oaxaca_barre, file.path(path_fig, "oaxaca_tp_barre.rds"))
cat("oaxaca_tp_barre : ok\n")

# ==============================================================================
# 7. Figure B : détail de l'effet composition par variable
# ==============================================================================

g_oaxaca_detail <- ggplot(
  contrib_compo_detail,
  aes(x = labs_vars, y = contrib, fill = sens)
) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Hausse de la pauvreté laborieuse" = "#e31a1c",
    "Baisse de la pauvreté laborieuse" = "#1f78b4"
  ), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    x       = NULL,
    y       = "Contribution à l'effet composition (pts de %)",
    caption = sprintf(
      "Source : INSEE, ERFS, calculs de l'auteur.\n%s. Champ : PR en emploi, 18-64 ans.\n",
      periodes_label
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_oaxaca_detail, file.path(path_fig, "oaxaca_tp_detail.rds"))
cat("oaxaca_tp_detail : ok\n")

# ==============================================================================
# 8. Figure C : détail de l'effet coefficients par variable
#
# Pour chaque variable j, contribution à l'effet coefficient :
#   contrib_coef_j = X̄_pool_j × (β₂_j - β₁_j)
#
# où X̄_pool = (X̄₁ + X̄₂) / 2.
# La somme sur toutes les variables ≈ effet coefficients total
# (l'écart résiduel est attribué au terme constant).
# ==============================================================================

Xbar_pool <- (Xbar1 + Xbar2) / 2

contrib_coef_detail <- tibble(
  col         = cols_comm,
  Xbar_pool   = Xbar_pool,
  delta_beta  = b2 - b1,
  contrib     = Xbar_pool * (b2 - b1) * 100
) |>
  mutate(variable = sapply(col, variable_de_col, vars = vars_ok)) |>
  filter(!is.na(variable)) |>   # retire l'intercept
  group_by(variable) |>
  summarise(contrib = sum(contrib), .groups = "drop") |>
  arrange(contrib) |>
  mutate(
    labs_vars = case_when(
      variable == "config"   ~ "Configuration du ménage",
      variable == "age_cat"  ~ "Âge",
      variable == "sexe_cat" ~ "Sexe",
      variable == "diplome"  ~ "Diplôme",
      variable == "immi_cat" ~ "Origine migratoire",
      variable == "statut_occ" ~ "Statut d'occupation du logement",
      variable == "dipl_h"   ~ "Diplôme",
      variable == "pcs_h"    ~ "Catégorie socioprofessionnelle",
      TRUE ~ variable
    ),
    labs_vars = fct_reorder(labs_vars, contrib),
    sens      = if_else(contrib >= 0,
                        "Hausse de la pauvreté laborieuse",
                        "Baisse de la pauvreté laborieuse"),
    tooltip   = paste0(labs_vars,
                       "\nContribution : ", sprintf("%+.2f", contrib), " pts de %"),
    data_id   = paste0("coef_", variable)
  )

g_oaxaca_coef_detail <- ggplot(
  contrib_coef_detail,
  aes(x = labs_vars, y = contrib, fill = sens)
) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Hausse de la pauvreté laborieuse" = "#e31a1c",
    "Baisse de la pauvreté laborieuse" = "#1f78b4"
  ), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    x       = NULL,
    y       = "Contribution à l'effet coefficients (pts de %)",
    caption = sprintf(
      "Source : INSEE, ERFS, calculs de l'auteur.\n%s. Champ : PR en emploi, 18-64 ans.\n",
      periodes_label
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_oaxaca_coef_detail, file.path(path_fig, "oaxaca_tp_coef_detail.rds"))
cat("oaxaca_tp_coef_detail : ok\n")

# ==============================================================================
# 9. Figure D : taux de pauvreté conditionnel par configuration — t1 vs t2
#
# Slope chart : pour chaque config, evolution du taux de pauvreté
# entre les deux périodes. Montre directement pour qui l'emploi protège
# mieux ou moins bien.
# ==============================================================================

rates_by_config <- bind_rows(
  d1c |> mutate(periode = periodes_label |> sub(" vs .*", "", x = _)),
  d2c |> mutate(periode = periodes_label |> sub(".* vs ", "", x = _))
) |>
  filter("config" %in% names(d1c)) |>
  group_by(periode, config) |>
  summarise(
    taux  = weighted.mean(pauvre, wprm) * 100,
    n_obs = n(),
    .groups = "drop"
  ) |>
  filter(n_obs >= 30)

# Labels courts
lab_config <- c(
  "Seul sans enfant"           = "Seul·e s.enf.",
  "Parent seul"                = "Parent seul",
  "Couple bi-actif s.enf."     = "Bi-actif s.enf.",
  "Couple bi-actif av.enf."    = "Bi-actif av.enf.",
  "Couple mono-actif s.enf."   = "Mono-actif s.enf.",
  "Couple mono-actif av.enf."  = "Mono-actif av.enf."
)

# Extraire les deux labels de période
p1_lab <- sub(" vs .*", "", periodes_label)
p2_lab <- sub(".* vs ", "", periodes_label)

if (nrow(rates_by_config) > 0 && all(c(p1_lab, p2_lab) %in% rates_by_config$periode)) {

  # Pré-calculer TOUS les tooltips dans le data frame pour éviter
  # les références à des variables d'environnement dans aes()
  # (qui échoueraient au rechargement du RDS dans une nouvelle session).
  p1_lab_val <- p1_lab   # copies locales comme valeurs scalaires figées
  p2_lab_val <- p2_lab

  rates_wide <- rates_by_config |>
    pivot_wider(names_from = periode, values_from = c(taux, n_obs)) |>
    rename(taux_t1 = paste0("taux_", p1_lab_val),
           taux_t2 = paste0("taux_", p2_lab_val)) |>
    mutate(
      delta       = taux_t2 - taux_t1,
      config_lb   = coalesce(lab_config[config], config),
      config_lb   = fct_reorder(config_lb, taux_t2),
      sens        = if_else(delta >= 0,
                            "Hausse du taux de pauvreté",
                            "Baisse du taux de pauvreté"),
      # Tooltips pré-calculés : aucune référence à une variable hors dataframe
      tooltip_t2  = paste0(config, "\n",
                           p1_lab_val, " : ", round(taux_t1, 1), " %\n",
                           p2_lab_val, " : ", round(taux_t2, 1), " %\n",
                           "Variation : ", sprintf("%+.1f", delta), " pt"),
      tooltip_t1  = paste0(config, "\n",
                           p1_lab_val, " : ", round(taux_t1, 1), " %"),
      data_id     = paste0("slope_", config),
      data_id_t1  = paste0("slope_", config, "_t1")
    )

  pal_pds <- c(
    "Hausse du taux de pauvreté" = "#e31a1c",
    "Baisse du taux de pauvreté" = "#1f78b4"
  )

  g_oaxaca_profils <- ggplot(rates_wide, aes(y = config_lb)) +
    # Segment reliant les deux périodes
    geom_segment(aes(x = taux_t1, xend = taux_t2,
                     yend = config_lb, colour = sens),
                 linewidth = 1.2, arrow = arrow(length = unit(4, "pt"))) +
    # Points t1 — tooltip_t1 est une colonne du data frame
    geom_point_interactive(
      aes(x = taux_t1, tooltip = tooltip_t1, data_id = data_id_t1),
      colour = "grey40", size = 3.5
    ) +
    # Points t2 (colorés) — tooltip_t2 et data_id sont des colonnes
    geom_point_interactive(
      aes(x = taux_t2, colour = sens,
          tooltip = tooltip_t2, data_id = data_id),
      size = 4
    ) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey60") +
    scale_colour_manual(values = pal_pds, guide = "none") +
    scale_x_continuous(labels = label_number(suffix = " %")) +
    labs(
      x       = "Taux de pauvreté des PR en emploi (%)",
      y       = NULL,
      caption = sprintf(
        "Source : INSEE, ERFS, calculs de l'auteur.\n%s. Champ : PR en emploi, 18-64 ans.\nLe point gris = %s, la flèche colorée = %s.",
        periodes_label, p1_lab, p2_lab
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
    )

  saveRDS(g_oaxaca_profils, file.path(path_fig, "oaxaca_tp_profils.rds"))
  cat("oaxaca_tp_profils : ok\n")

} else {
  cat("AVERTISSEMENT : variable config absente de d1c/d2c — oaxaca_tp_profils non créé\n")
}

cat("\n=== oaxaca_tp.R terminé ===\n")
