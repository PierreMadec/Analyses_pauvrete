# ==============================================================================
# typologie_tp.R
#
# Approche individuelle de la pauvreté laborieuse :
# balaye toutes les configurations ménage d'un travailleur pauvre.
#
# Typologie croisée (6 profils) sur les PR en emploi :
#   1. Seul·e sans enfant              (typmen = Personne seule)
#   2. Parent seul avec enfant(s)      (typmen = Famille monoparentale)
#   3. Couple bi-actif sans enfant     (typmen = Couple sans enfant,  biactivite = Bi-actif)
#   4. Couple bi-actif avec enfant(s)  (typmen = Couple avec enfant(s), biactivite = Bi-actif)
#   5. Couple mono-actif sans enfant   (typmen = Couple sans enfant,  biactivite ≠ Bi-actif)
#   6. Couple mono-actif avec enfant(s)(typmen = Couple avec enfant(s), biactivite ≠ Bi-actif)
#
# Note : on utilise `biactivite` (toujours défini dans data_all pour les PR)
# plutôt que `acteu_cj` qui peut être NA dans certaines vagues ERFS.
# `typmen` encode déjà la présence d'enfants → pas besoin de nb_enfants.
#
# Figures produites :
#   tp3_taux_config  — taux de pauvreté par configuration (lignes 2005-2023)
#   tp3_compo_tp     — composition des PR en emploi pauvres (barres empilées)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

caption_base <- paste0(
  "Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
  "Champ : personnes de référence du ménage en emploi, 18-64 ans.\n",
  "Bi-actif : PR en emploi avec conjoint en emploi. Mono-actif : conjoint inactif ou au chômage."
)

pal_config <- c(
  "Seul·e sans enfant"            = "#2674DD",
  "Parent seul avec enfant(s)"    = "#08BAB7",
  "Couple bi-actif sans enfant"   = "#E91422",
  "Couple bi-actif avec enf."     = "#8D30D4",
  "Couple mono-actif sans enfant" = "#757575",
  "Couple mono-actif avec enf."   = "#D79700"
)

# ==============================================================================
# 1. Construction de la base typologisée
#    On utilise biactivite (Bi-actif / Mono-actif / Sans emploi) défini dans
#    data_all, et typmen pour la structure familiale.
# ==============================================================================

tp_config_base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi",
         age_num >= 18, age_num <= 64) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std), !is.na(typmen), !is.na(biactivite)) |>
  mutate(
    pauvre = (nivviem < seuil_std),
    config = case_when(
      typmen == "Personne seule"           ~ "Seul·e sans enfant",
      typmen == "Famille monoparentale"    ~ "Parent seul avec enfant(s)",
      typmen == "Couple sans enfant"  & biactivite == "Bi-actif"  ~ "Couple bi-actif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif"  ~ "Couple bi-actif avec enf.",
      typmen == "Couple sans enfant"  & biactivite != "Bi-actif"  ~ "Couple mono-actif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif"  ~ "Couple mono-actif avec enf.",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(config))

config_levels <- names(pal_config)
tp_config_base <- tp_config_base |>
  mutate(config = factor(config, levels = config_levels))

# ==============================================================================
# 2. Figure A : taux de pauvreté par configuration (lignes, 2005-2024)
# ==============================================================================

taux_config <- tp_config_base |>
  group_by(annee, config) |>
  summarise(
    taux  = 100 * sum(wprm * pauvre, na.rm = TRUE) / sum(wprm),
    n_obs = n(),
    .groups = "drop"
  ) |>
  filter(n_obs >= 50) |>
  mutate(
    tooltip = paste0(config, " — ", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(config, "_", annee)
  )

g_tp3_taux_config <- ggplot(
  taux_config,
  aes(x = annee, y = taux, colour = config, group = config)
) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = pal_config, drop = TRUE) +
  scale_x_continuous(breaks = seq(2005, 2025, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "taux de pauvreté (%)",
    colour  = NULL,
    caption = paste0(
      caption_base,
      "\nCouple bi-actif sans enfant : échantillon insuffisant (moins de 50 travailleurs ",
      "pauvres identifiés chaque année), non représenté."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_tp3_taux_config, file.path(path_fig, "tp3_taux_config.rds"))
cat("tp3_taux_config : ok\n")

# ==============================================================================
# 3. Figure B : composition des travailleurs pauvres par configuration (années clés)
# ==============================================================================

annees_dispo <- unique(tp_config_base$annee)
annees_cles  <- c(2010, 2015, 2019, 2024)[c(2010, 2015, 2019, 2024) %in% annees_dispo]

compo_tp <- tp_config_base |>
  filter(pauvre, annee %in% annees_cles) |>
  group_by(annee, config) |>
  summarise(poids = sum(wprm), .groups = "drop") |>
  group_by(annee) |>
  mutate(
    part    = 100 * poids / sum(poids),
    tooltip = paste0(config, " — ", annee, " : ", round(part, 1), " %"),
    data_id = paste0(config, "_", annee)
  ) |>
  ungroup()

g_tp3_compo_tp <- ggplot(
  compo_tp,
  aes(x = factor(annee), y = part, fill = config)
) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.65
  ) +
  scale_fill_manual(values = pal_config, drop = FALSE) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x       = NULL,
    y       = "Part dans les travailleurs pauvres (%)",
    fill    = NULL,
    caption = caption_base
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_tp3_compo_tp, file.path(path_fig, "tp3_compo_tp.rds"))
cat("tp3_compo_tp : ok\n")

cat("\n=== typologie_tp.R terminé ===\n")
