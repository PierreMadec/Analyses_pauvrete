# ==============================================================================
# interaction_typmen_emploi.R
#
# Extension 3 : Interaction configuration de ménage × conditions d'emploi
#
# Question : les configurations de ménage vulnérables (familles monoparentales,
# couples mono-actifs) sont-elles davantage concentrées dans les emplois
# précaires ? Cette concentration s'est-elle aggravée ?
#
# Pour chaque configuration et chaque année :
#   - Part du temps partiel parmi les travailleurs
#   - Part des emplois précaires (CDD + intérim) parmi les travailleurs
#   - Distribution PCS (part "bas" = Ouvriers + Employés)
#
# Figures produites :
#   interaction_tpspartiel.rds  — % temps partiel par config (2005-2023)
#   interaction_precarite.rds   — % CDD/intérim par config (2015-2023)
#   interaction_pcs.rds         — % ouvriers+employés par config (2015-2023)
#   interaction_heatmap.rds     — heatmap (config × condition) en t0 et t1
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
  "Source : INSEE, ERFS, calculs de l'auteur.\n",
  "Champ : personnes de référence du ménage (PR) en emploi, 18-64 ans."
)

# Palette configurations (cohérente avec oaxaca_tp.R)
configs_ordre <- c(
  "Parent seul",
  "Couple mono-actif av.enf.",
  "Couple mono-actif s.enf.",
  "Seul sans enfant",
  "Couple bi-actif av.enf.",
  "Couple bi-actif s.enf."
)

pal_config <- c(
  "Parent seul"               = "#e31a1c",
  "Couple mono-actif av.enf." = "#ff7f00",
  "Couple mono-actif s.enf."  = "#fdbf6f",
  "Seul sans enfant"          = "#6a3d9a",
  "Couple bi-actif av.enf."   = "#1f78b4",
  "Couple bi-actif s.enf."    = "#a6cee3"
)

# ==============================================================================
# 1. Base : PR en emploi 18-64 avec configuration
# ==============================================================================

base_int <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi",
         !is.na(age_num), age_num >= 18, age_num <= 64,
         !is.na(wprm), wprm > 0,
         !is.na(typmen), !is.na(biactivite)) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std)) |>
  mutate(
    pauvre = as.integer(nivviem < seuil_std),
    config = case_when(
      typmen == "Personne seule"             ~ "Seul sans enfant",
      typmen == "Famille monoparentale"      ~ "Parent seul",
      typmen == "Couple sans enfant"   & biactivite == "Bi-actif" ~ "Couple bi-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite == "Bi-actif" ~ "Couple bi-actif av.enf.",
      typmen == "Couple sans enfant"   & biactivite != "Bi-actif" ~ "Couple mono-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite != "Bi-actif" ~ "Couple mono-actif av.enf.",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(config)) |>
  mutate(
    config      = factor(config, levels = configs_ordre),
    tps_partiel = tpstrav == 2,      # TRUE si temps partiel
    precaire    = type_contrat %in% c("CDD", "Intérim"),
    pcs_bas     = pcs_cat %in% c("Employes", "Ouvriers")
  )

# ==============================================================================
# 2. FIGURE 1 — Part du temps partiel par configuration (longue période)
# ==============================================================================

tp_partiel_config <- base_int |>
  filter(!is.na(tpstrav)) |>
  group_by(annee, config) |>
  summarise(
    part_tp = weighted.mean(tps_partiel, wprm, na.rm = TRUE) * 100,
    n_obs   = n(),
    .groups = "drop"
  ) |>
  filter(n_obs >= 30) |>
  mutate(
    tooltip = paste0(config, " — ", annee, "\n",
                     "Temps partiel : ", round(part_tp, 1), " %"),
    data_id = paste0(config, "_tp_", annee)
  )

g_int_tp <- ggplot(
  tp_partiel_config,
  aes(x = annee, y = part_tp, colour = config, group = config)
) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = pal_config, drop = FALSE) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, NA)) +
  labs(
    y       = "Part des travailleurs à temps partiel (%)",
    colour  = NULL,
    caption = caption_base
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_int_tp, file.path(path_fig, "interaction_tpspartiel.rds"))
cat("interaction_tpspartiel : ok\n")

# ==============================================================================
# 3. FIGURE 2 — Part des emplois précaires par configuration
# ==============================================================================

annees_precaire <- sort(unique(base_int$annee[!is.na(base_int$type_contrat)]))
cat(sprintf("Années disponibles pour type_contrat : %s\n",
            paste(annees_precaire, collapse = ", ")))

if (length(annees_precaire) >= 3) {
  tp_precaire_config <- base_int |>
    filter(!is.na(type_contrat), annee %in% annees_precaire) |>
    group_by(annee, config) |>
    summarise(
      part_prec = weighted.mean(precaire, wprm, na.rm = TRUE) * 100,
      n_obs     = n(),
      .groups   = "drop"
    ) |>
    filter(n_obs >= 30) |>
    mutate(
      tooltip = paste0(config, " — ", annee, "\n",
                       "CDD + Intérim : ", round(part_prec, 1), " %"),
      data_id = paste0(config, "_prec_", annee)
    )

  g_int_prec <- ggplot(
    tp_precaire_config,
    aes(x = annee, y = part_prec, colour = config, group = config)
  ) +
    geom_line_interactive(linewidth = 1.1) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
    scale_colour_manual(values = pal_config, drop = FALSE) +
    scale_x_continuous(breaks = annees_precaire) +
    scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, NA)) +
    labs(
      y       = "Part des travailleurs en CDD ou intérim (%)",
      colour  = NULL,
      caption = paste0(caption_base, "\n(variable disponible à partir de 2015 environ)")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
    )

  saveRDS(g_int_prec, file.path(path_fig, "interaction_precarite.rds"))
  cat("interaction_precarite : ok\n")
} else {
  cat("AVERTISSEMENT : type_contrat insuffisant — interaction_precarite non créé\n")
}

# ==============================================================================
# 4. FIGURE 3 — Heatmap config × condition en t0 et t1
#    Représentation matricielle :
#      - lignes = configurations de ménage
#      - colonnes = conditions d'emploi (tp, précarité, pcs_bas)
#      - couleur = part (%) — deux panneaux : t0 et t1
# ==============================================================================

annees_t0_heat <- base_int$annee[base_int$annee >= 2015 & base_int$annee <= 2017 &
                                    !is.na(base_int$tpstrav) & !is.na(base_int$pcs_cat)]
annees_t0_heat <- unique(annees_t0_heat)
annees_t1_heat <- base_int$annee[base_int$annee >= 2021 &
                                    !is.na(base_int$tpstrav) & !is.na(base_int$pcs_cat)]
annees_t1_heat <- unique(annees_t1_heat)

if (length(annees_t0_heat) > 0 && length(annees_t1_heat) > 0) {

  calc_heatrow <- function(df_sub, label_periode) {
    df_sub |>
      filter(!is.na(tpstrav), !is.na(pcs_cat)) |>
      group_by(config) |>
      summarise(
        `Temps partiel`   = weighted.mean(tps_partiel, wprm, na.rm = TRUE) * 100,
        `Emploi précaire` = weighted.mean(precaire,    wprm, na.rm = TRUE) * 100,
        `Ouvriers/Employés` = weighted.mean(pcs_bas,   wprm, na.rm = TRUE) * 100,
        .groups = "drop"
      ) |>
      pivot_longer(-config, names_to = "condition", values_to = "part") |>
      mutate(periode = label_periode)
  }

  heat_t0 <- base_int |>
    filter(annee %in% annees_t0_heat, !is.na(type_contrat)) |>
    calc_heatrow(sprintf("%d-%d", min(annees_t0_heat), max(annees_t0_heat)))

  heat_t1 <- base_int |>
    filter(annee %in% annees_t1_heat, !is.na(type_contrat)) |>
    calc_heatrow(sprintf("%d-%d", min(annees_t1_heat), max(annees_t1_heat)))

  heat_data <- bind_rows(heat_t0, heat_t1) |>
    mutate(
      periode   = factor(periode, levels = c(unique(heat_t0$periode),
                                              unique(heat_t1$periode))),
      condition = factor(condition,
                         levels = c("Temps partiel", "Emploi précaire",
                                    "Ouvriers/Employés")),
      tooltip   = paste0(config, " — ", condition, "\n",
                         periode, " : ", round(part, 1), " %"),
      data_id   = paste0(config, "_", condition, "_", periode)
    )

  g_heatmap <- ggplot(
    heat_data,
    aes(x = condition, y = config, fill = part)
  ) +
    geom_tile_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      colour = "white", linewidth = 0.5
    ) +
    geom_text(
      aes(label = paste0(round(part, 0), "%")),
      size = 3.5, colour = "white", fontface = "bold"
    ) +
    facet_wrap(~periode, ncol = 2) +
    scale_fill_gradient(
      low = "#deebf7", high = "#08519c",
      name = "Part (%)",
      limits = c(0, NA)
    ) +
    scale_y_discrete(limits = rev(configs_ordre)) +
    labs(
      x       = NULL,
      y       = NULL,
      caption = paste0(
        caption_base, "\n",
        "Lecture : chaque case indique la part (%) des travailleurs de cette configuration ",
        "dans cette condition d'emploi."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid     = element_blank(),
      axis.text.x    = element_text(angle = 20, hjust = 1),
      strip.text     = element_text(face = "bold"),
      legend.position = "right",
      plot.caption   = element_text(size = 7.5, colour = "grey50", hjust = 0)
    )

  saveRDS(g_heatmap, file.path(path_fig, "interaction_heatmap.rds"))
  cat("interaction_heatmap : ok\n")
} else {
  cat("AVERTISSEMENT : données insuffisantes pour heatmap\n")
}

cat("\n=== interaction_typmen_emploi.R terminé ===\n")
