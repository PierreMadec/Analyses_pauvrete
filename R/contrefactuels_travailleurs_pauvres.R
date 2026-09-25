# ==============================================================================
# contrefactuels_travailleurs_pauvres.R
#
# Quantifie, par configuration de ménage, quel levier sortirait effectivement
# le travailleur pauvre de la pauvreté. Pour chaque travailleur pauvre (PR en
# emploi, ménage sous le seuil), on simule trois leviers et on teste si le
# niveau de vie repasse au-dessus du seuil :
#
#   A. Temps plein au même taux horaire : salaire -> salaire ETP
#   B. Temps plein au Smic              : salaire -> max(SMIC, salaire ETP)
#   C. Second revenu d'activité         : +1 SMIC temps plein (ménages
#      mono-actifs en couple uniquement — un second adulte existe mais est inactif)
#
# Le gain de revenu disponible induit par chaque levier est estimé à partir de
# la maquette SoFi [@pucci_sofi_2022], qui simule précisément RSA, prime
# d'activité, aides au logement et impôt sur le revenu (contrairement à un
# taux de reprise forfaitaire). Pour chaque configuration, une courbe
# salaire -> niveau de vie a été construite en balayant le salaire d'un
# ménage archétype (âges d'enfants représentatifs, locataire) sur 2021-2023
# (cf. python/build_sofi_curves.R et python/build_sofi_second_revenu.py). On
# n'utilise de cette courbe que la PENTE entre le salaire actuel et le
# salaire contrefactuel — pas son niveau absolu, qui dépend de caractéristiques
# de l'archétype (loyer, etc.) non alignées sur celles du ménage ERFS — et on
# applique ce gain marginal au niveau de vie réellement observé du ménage.
#
# Figure produite (figure/) :
#   tp_contrefactuel — part des travailleurs pauvres sortis du seuil, par levier
#                      et par configuration (2022-2024)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"

trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))
pm   <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
smic_an <- setNames(pm$smic_net * 12, as.character(pm$annee))

sofi_curves <- read_csv(file.path(path_fig, "sofi_curves.csv"), show_col_types = FALSE)
sofi_2nd    <- read_csv(file.path(path_fig, "sofi_second_revenu.csv"), show_col_types = FALSE)

# ── Interpolation sur les courbes SoFi ─────────────────────────────────────────
niveau_vie_sofi <- function(config_sofi, annee_, salaire_mensuel) {
  d_courbe <- sofi_curves |> filter(config == config_sofi, annee == annee_)
  if (nrow(d_courbe) == 0) return(NA_real_)
  approx(d_courbe$salaire_mensuel, d_courbe$niveau_vie,
         xout = pmin(pmax(salaire_mensuel, 0), max(d_courbe$salaire_mensuel)),
         rule = 2)$y
}

# Point de grille le plus proche du salaire de l'adulte 1 (levier second revenu)
niveau_vie_sofi_2eme_revenu <- function(config_sofi, annee_, salaire_adulte1_mensuel, salaire_adulte2_mensuel) {
  d_grille <- sofi_2nd |> filter(config == config_sofi, annee == annee_)
  if (nrow(d_grille) == 0) return(NA_real_)
  sal1_points <- sort(unique(d_grille$salaire_adulte1))
  sal1_proche <- sal1_points[which.min(abs(sal1_points - salaire_adulte1_mensuel))]
  d_sal1 <- d_grille |> filter(salaire_adulte1 == sal1_proche)
  approx(d_sal1$salaire_adulte2, d_sal1$niveau_vie,
         xout = pmin(pmax(salaire_adulte2_mensuel, 0), max(d_sal1$salaire_adulte2)),
         rule = 2)$y
}

config_sofi_mono <- function(nb_enfants) {
  case_when(
    nb_enfants == "1 enfant"    ~ "Famille monoparentale 1 enfant",
    nb_enfants == "2 enfants"   ~ "Famille monoparentale 2 enfants",
    nb_enfants == "3+ enfants"  ~ "Famille monoparentale 3 enfants",
    TRUE                        ~ "Famille monoparentale 1 enfant"  # repli, cas résiduels
  )
}

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, wprm, nivviem, nb_uci, revdispm, typmen, biactivite, ppa, nb_enfants)

lab_levels <- c("Personne seule", "Famille monoparentale",
                "Couple mono-actif sans enfant", "Couple mono-actif avec enfant(s)",
                "Couple bi-actif avec enfant(s)")

base <- trav |>
  inner_join(hh, by = c("annee", "ident")) |>
  left_join(s |> select(annee, seuil_std), by = "annee") |>
  mutate(
    pauvre  = nivviem < seuil_std,
    smic    = smic_an[as.character(annee)],
    sal_etp = ifelse(!is.na(txtp) & txtp > 0 & temps_partiel == 1,
                     salaires_i / (txtp / 100), salaires_i),
    ppa         = coalesce(as.numeric(ppa), 0),
    couple_mono = biactivite != "Bi-actif" &
                  typmen %in% c("Couple sans enfant", "Couple avec enfant(s)"),
    config = case_when(
      typmen == "Personne seule"                                  ~ "Personne seule",
      typmen == "Famille monoparentale"                           ~ "Famille monoparentale",
      typmen == "Couple sans enfant"    & biactivite != "Bi-actif" ~ "Couple mono-actif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif" ~ "Couple mono-actif avec enfant(s)",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif" ~ "Couple bi-actif avec enfant(s)",
      TRUE ~ NA_character_),
    config = factor(config, levels = lab_levels),
    config_sofi = case_when(
      config == "Famille monoparentale" ~ config_sofi_mono(nb_enfants),
      TRUE ~ as.character(config)
    )
  )

wpp <- base |>
  filter(lpr == 1, emploi == 1, age >= 18, age <= 64, pauvre,
         !is.na(config), annee %in% 2022:2024, !is.na(salaires_i))

# ── Simulation des leviers via les courbes SoFi ─────────────────────────────────
# Salaire mensuel actuel, ETP et SMIC (la maquette raisonne en mensuel).
wpp <- wpp |>
  mutate(
    sal_mens_actuel = salaires_i / 12,
    sal_mens_etp     = sal_etp / 12,
    sal_mens_smic    = pmax(smic, sal_etp) / 12
  )

# Gain de niveau de vie SoFi = variation de niveau de vie sur la courbe de
# l'archétype entre salaire actuel et salaire contrefactuel (cf. en-tête :
# seule la pente est utilisée, appliquée au niveau de vie réel du ménage).
# La maquette raisonne en mensuel (salaires et niveau de vie) ; nivviem et
# seuil_std sont annuels -> on annualise le gain (x12).
gain_sofi <- function(config_sofi, annee_, sal_avant, sal_apres) {
  mapply(function(cs, an, s0, s1) {
    if (is.na(cs) || is.na(an)) return(NA_real_)
    12 * (niveau_vie_sofi(cs, an, s1) - niveau_vie_sofi(cs, an, s0))
  }, config_sofi, annee_, sal_avant, sal_apres)
}

gain_sofi_2eme <- function(config_sofi, annee_, sal1, couple_mono_) {
  mapply(function(cs, an, s1, cm) {
    if (!isTRUE(cm)) return(NA_real_)
    12 * (niveau_vie_sofi_2eme_revenu(cs, an, s1, smic_an[as.character(an)] / 12) -
      niveau_vie_sofi_2eme_revenu(cs, an, s1, 0))
  }, config_sofi, annee_, sal1, couple_mono_)
}

simu <- wpp |>
  mutate(
    gain_A = gain_sofi(config_sofi, annee, sal_mens_actuel, sal_mens_etp),
    gain_B = gain_sofi(config_sofi, annee, sal_mens_actuel, sal_mens_smic),
    gain_C = gain_sofi_2eme(config_sofi, annee, sal_mens_actuel, couple_mono),
    nv_A = nivviem + gain_A,
    nv_B = nivviem + gain_B,
    nv_C = ifelse(couple_mono, nivviem + gain_C, NA_real_),
    sort_A = as.integer(nv_A >= seuil_std),
    sort_B = as.integer(nv_B >= seuil_std),
    sort_C = ifelse(couple_mono, as.integer(nv_C >= seuil_std), NA_integer_)
  )

res <- simu |>
  group_by(config) |>
  summarise(
    `Passer à temps plein\n(même taux horaire)` = 100 * weighted.mean(sort_A, wprm, na.rm = TRUE),
    `Temps plein au Smic`                        = 100 * weighted.mean(sort_B, wprm, na.rm = TRUE),
    `Ajouter un second revenu\n(1 Smic)`         = 100 * weighted.mean(sort_C, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-config, names_to = "levier", values_to = "pct") |>
  filter(!is.nan(pct)) |>
  mutate(
    levier = factor(levier, levels = c("Passer à temps plein\n(même taux horaire)",
                                       "Temps plein au Smic",
                                       "Ajouter un second revenu\n(1 Smic)")),
    tooltip = paste0(config, "\n", gsub("\n", " ", levier), " : ",
                     round(pct), " % sortis du seuil"),
    data_id = paste0(gsub("[^a-z]", "", tolower(config)), "_",
                     gsub("[^a-z]", "", tolower(levier)))
  )

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}

pal_lev <- c("Passer à temps plein\n(même taux horaire)" = "#2674DD",
             "Temps plein au Smic"                        = "#08BAB7",
             "Ajouter un second revenu\n(1 Smic)"         = "#8D30D4")

g_cf <- ggplot(res, aes(x = config, y = pct, fill = levier)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = pal_lev) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 100)) +
  coord_flip() +
  labs(y = "part des travailleurs pauvres de la configuration sortis du seuil",
       x = NULL,
       caption = paste0(
         "Source : INSEE, ERFS 2022-2024 ; Pucci M., SoFi (2022), calculs de l'auteur.\n",
         "Gain de revenu disponible simulé par la maquette SoFi (RSA, prime d'activité, ",
         "aides au logement, impôt sur le revenu). Le second revenu n'est simulé que ",
         "pour les couples mono-actifs.\nChamp : PR en emploi, 18-64 ans, ménage pauvre.")) +
  theme_erfs() +
  theme(legend.text = element_text(size = 8))
saveRDS(g_cf, file.path(path_fig, "tp_contrefactuel.rds"))
cat("tp_contrefactuel : ok\n")

cat("Taux de sortie par levier et configuration (via courbes SoFi) :\n")
print(as.data.frame(res |> select(config, levier, pct) |> mutate(pct = round(pct))), row.names = FALSE)

# ==============================================================================
# Focus monoparents : le taux de sortie "temps plein au SMIC" (≈ 40 % en
# moyenne) masque-t-il un écart selon le nombre d'enfants ? L'échelle des
# unités de consommation (0,3 UC par enfant) n'étant pas linéaire dans le
# revenu nécessaire pour franchir le seuil, un même levier salarial devrait
# sortir davantage de familles à 1 enfant que de familles à 3 enfants ou plus.
# ==============================================================================

simu_mono <- simu |>
  filter(config == "Famille monoparentale", !is.na(nb_enfants))

res_enfants <- simu_mono |>
  group_by(nb_enfants) |>
  summarise(
    pct   = 100 * weighted.mean(sort_B, wprm, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  filter(n_obs >= 30) |>
  mutate(
    nb_enfants = factor(nb_enfants, levels = c("0 enfant", "1 enfant", "2 enfants", "3+ enfants")),
    tooltip = paste0(nb_enfants, "\nTemps plein au Smic : ", round(pct), " % sortis du seuil"),
    data_id = paste0("mono_enf_", nb_enfants)
  )

g_cf_mono_enfants <- ggplot(res_enfants, aes(x = nb_enfants, y = pct)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       fill = "#e31a1c", width = 0.6) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 100)) +
  labs(
    x = NULL,
    y = "Familles monoparentales sorties du seuil\npar un temps plein au SMIC",
    caption = paste0(
      "Source : INSEE, ERFS 2022-2024, calculs de l'auteur.\n",
      "Simulation statique, nette de la reprise de prime d'activité. ",
      "Champ : PR en emploi, familles monoparentales pauvres, 18-64 ans.")) +
  theme_erfs()
saveRDS(g_cf_mono_enfants, file.path(path_fig, "tp_cf_monoparent_enfants.rds"))
cat("tp_cf_monoparent_enfants : ok\n")
print(as.data.frame(res_enfants |> select(nb_enfants, pct, n_obs)), row.names = FALSE)

cat("\n=== contrefactuels_travailleurs_pauvres.R terminé ===\n")
