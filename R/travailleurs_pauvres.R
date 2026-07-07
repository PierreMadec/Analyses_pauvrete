# ==============================================================================
# travailleurs_pauvres.R
#
# Analyses complémentaires pour l'article "Pourquoi le travail protège-t-il
# de moins en moins bien de la pauvreté ?"
#
# Prérequis : data_all et seuils_annuels produits par decomposition_pauvrete.R
#
# Figures produites (sauvegardées dans figure/) :
#   tp2_taux_sexe      — taux de pauvreté des travailleurs par sexe (2005-2023)
#   tp2_intensite      — intensité de la pauvreté laborieuse vs ensemble
#   tp2_genre          — part H/F dans travailleurs pauvres vs non pauvres
#   tp2_diplome        — structure par diplôme (pauvres vs non pauvres)
#   tp2_immi           — part immigrés (pauvres vs non pauvres)
#   tp2_tpspartiel     — temps partiel et sous-emploi (2015-2023)
#   tp2_actcj          — activité du conjoint (pauvres vs non pauvres)
#   tp2_nb_enfants     — nombre d'enfants (pauvres vs non pauvres)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

# ==============================================================================
# Chargement des données si nécessaire
# ==============================================================================
resolve_script <- function(filename) {
  candidates <- c(
    file.path("R", filename),
    filename,
    file.path("..", "R", filename)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0)
    stop(filename, " introuvable. Lancez depuis la racine du projet.")
  found[1]
}

if (!exists("data_all") || !exists("seuils_annuels")) {
  rds_data   <- resolve_script("../figure/data_all.rds")
  rds_seuils <- resolve_script("../figure/seuils_annuels.rds")
  if (file.exists(rds_data) && file.exists(rds_seuils)) {
    message("Chargement de data_all depuis les RDS sauvegardés...")
    data_all       <<- readRDS(rds_data)
    seuils_annuels <<- readRDS(rds_seuils)
  } else {
    stop(paste(
      "data_all introuvable. Lancez d'abord decomposition_pauvrete.R",
      "depuis la racine du projet pour générer figure/data_all.rds."
    ))
  }
}

# Palette commune
pal_deux  <- c("#1f78b4", "#e31a1c")
pal_trois <- c("#1f78b4", "#33a02c", "#e31a1c")

path_fig <- "figure"
if (!dir.exists(path_fig)) dir.create(path_fig)

# Thème commun
theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      plot.caption       = element_text(size = 8, colour = "grey50", hjust = 0),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.title.x       = element_blank()
    )
}

caption_base <- paste0(
  "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
  "Travailleurs pauvres : individus en emploi (acteu) dans un ménage dont le ",
  "niveau de vie est inférieur à 60 % du niveau de vie médian."
)

# ==============================================================================
# Base travailleurs (tous actifs en emploi, toutes années)
# ==============================================================================

tp_base_full <- data_all |>
  filter(acteu_ind == "Emploi") |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std)

dates_cles <- c(2010, 2015, 2019, 2023)

# ==============================================================================
# 1. Taux de pauvreté des travailleurs par sexe — longue période
# ==============================================================================

taux_tp_sexe <- tp_base_full |>
  filter(!is.na(sexe_cat)) |>
  group_by(annee, sexe_cat) |>
  summarise(taux = 100 * sum(wprm * pauvre, na.rm = TRUE) / sum(wprm), .groups = "drop") |>
  mutate(
    tooltip = paste0(sexe_cat, " — ", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(sexe_cat, "_", annee)
  )

# Taux global (tous travailleurs)
taux_tp_global <- tp_base_full |>
  group_by(annee) |>
  summarise(taux = 100 * sum(wprm * pauvre, na.rm = TRUE) / sum(wprm), .groups = "drop") |>
  mutate(
    sexe_cat = factor("Ensemble"),
    tooltip  = paste0("Ensemble — ", annee, " : ", round(taux, 1), " %"),
    data_id  = paste0("Ensemble_", annee)
  )

taux_tp_plot <- bind_rows(taux_tp_sexe, taux_tp_global)

g_tp2_taux_sexe <- ggplot(taux_tp_plot,
                           aes(x = annee, y = taux, colour = sexe_cat,
                               group = sexe_cat)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = c("Homme" = "#1f78b4", "Femme" = "#e31a1c",
                                  "Ensemble" = "#6a3d9a")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "Taux de pauvreté (%)",
    caption = caption_base
  ) +
  theme_erfs()

saveRDS(g_tp2_taux_sexe, file.path(path_fig, "tp2_taux_sexe.rds"))
cat("tp2_taux_sexe : ok\n")

# ==============================================================================
# 2. Intensité de la pauvreté laborieuse
#    Écart moyen au seuil (en % du seuil) — travailleurs pauvres vs ensemble pauvres
# ==============================================================================

pauvres_base <- data_all |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(nivviem < seuil_std) |>
  mutate(gap = (seuil_std - nivviem) / seuil_std * 100)

# Travailleurs pauvres (individus en emploi)
intensite_trav <- pauvres_base |>
  filter(acteu_ind == "Emploi") |>
  group_by(annee) |>
  summarise(gap_moyen = weighted.mean(gap, wprm, na.rm = TRUE), .groups = "drop") |>
  mutate(groupe = "Travailleurs pauvres")

# Ensemble de tous les pauvres (toutes activités confondues)
intensite_ens <- pauvres_base |>
  group_by(annee) |>
  summarise(gap_moyen = weighted.mean(gap, wprm, na.rm = TRUE), .groups = "drop") |>
  mutate(groupe = "Ensemble des pauvres")

intensite_tp <- bind_rows(intensite_trav, intensite_ens) |>
  mutate(
    tooltip = paste0(groupe, " — ", annee, " : ", round(gap_moyen, 1), " %"),
    data_id = paste0(groupe, "_", annee)
  )

g_tp2_intensite <- ggplot(intensite_tp,
                           aes(x = annee, y = gap_moyen, colour = groupe,
                               group = groupe)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = c("Travailleurs pauvres"  = "#e31a1c",
                                  "Ensemble des pauvres" = "#1f78b4")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "Écart moyen au seuil (% du seuil)",
    caption = caption_base
  ) +
  theme_erfs()

saveRDS(g_tp2_intensite, file.path(path_fig, "tp2_intensite.rds"))
cat("tp2_intensite : ok\n")

# ==============================================================================
# 3. Genre — composition des travailleurs pauvres vs non pauvres
# ==============================================================================

tp_genre <- tp_base_full |>
  filter(!is.na(sexe_cat), annee %in% dates_cles) |>
  mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
  group_by(annee, pauvre, groupe, sexe_cat) |>
  summarise(poids = sum(wprm), .groups = "drop") |>
  group_by(annee, pauvre) |>
  mutate(
    part    = 100 * poids / sum(poids),
    tooltip = paste0(sexe_cat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
    data_id = paste0(sexe_cat, "_", pauvre, "_", annee)
  ) |>
  ungroup()

g_tp2_genre <- ggplot(
  tp_genre |> filter(sexe_cat == "Femme"),
  aes(x = factor(annee), y = part, fill = groupe, group = groupe)
) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "dodge", width = 0.6
  ) +
  scale_fill_manual(values = c("Travailleurs pauvres"     = "#e31a1c",
                                "Travailleurs non pauvres" = "#1f78b4")) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 60)) +
  labs(
    y       = "Part des femmes dans le groupe (%)",
    caption = caption_base
  ) +
  theme_erfs()

saveRDS(g_tp2_genre, file.path(path_fig, "tp2_genre.rds"))
cat("tp2_genre : ok\n")

# ==============================================================================
# 4. Diplôme — structure des travailleurs pauvres vs non pauvres
# ==============================================================================

if (any(!is.na(tp_base_full$diplome))) {

  tp_diplome <- tp_base_full |>
    filter(!is.na(diplome), annee %in% dates_cles) |>
    mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
    group_by(annee, pauvre, groupe, diplome) |>
    summarise(poids = sum(wprm), .groups = "drop") |>
    group_by(annee, pauvre) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(diplome, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(diplome, "_", pauvre, "_", annee)
    ) |>
    ungroup()

  g_tp2_diplome <- ggplot(tp_diplome,
                           aes(x = factor(annee), y = part, fill = diplome)) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.6
    ) +
    facet_wrap(~groupe) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(
      y       = "Part dans le groupe (%)",
      fill    = "Niveau de diplôme",
      caption = caption_base
    ) +
    theme_erfs() +
    theme(legend.position = "bottom", legend.title = element_text())

  saveRDS(g_tp2_diplome, file.path(path_fig, "tp2_diplome.rds"))
  cat("tp2_diplome : ok\n")
} else {
  cat("AVERTISSEMENT : variable diplome absente — tp2_diplome non créé\n")
}

# ==============================================================================
# 5. Immigration — part des immigrés parmi les travailleurs pauvres vs non pauvres
# ==============================================================================

if (any(!is.na(tp_base_full$immi_cat))) {

  tp_immi <- tp_base_full |>
    filter(!is.na(immi_cat), annee %in% dates_cles) |>
    mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
    group_by(annee, pauvre, groupe, immi_cat) |>
    summarise(poids = sum(wprm), .groups = "drop") |>
    group_by(annee, pauvre) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(immi_cat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(immi_cat, "_", pauvre, "_", annee)
    ) |>
    ungroup()

  g_tp2_immi <- ggplot(
    tp_immi |> filter(immi_cat == "Immigre"),
    aes(x = factor(annee), y = part, fill = groupe, group = groupe)
  ) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "dodge", width = 0.6
    ) +
    scale_fill_manual(values = c("Travailleurs pauvres"     = "#e31a1c",
                                  "Travailleurs non pauvres" = "#1f78b4")) +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(
      y       = "Part des travailleurs immigrés dans le groupe (%)",
      caption = caption_base
    ) +
    theme_erfs()

  saveRDS(g_tp2_immi, file.path(path_fig, "tp2_immi.rds"))
  cat("tp2_immi : ok\n")
} else {
  cat("AVERTISSEMENT : variable immi_cat absente — tp2_immi non créé\n")
}

# ==============================================================================
# 6. Temps partiel subi et sous-emploi (disponible à partir de 2015)
# ==============================================================================

annees_tps <- c(2015, 2017, 2019, 2021, 2023)

tp_tpspartiel <- tp_base_full |>
  filter(!is.na(tpstrav), annee %in% annees_tps) |>
  mutate(
    groupe        = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres"),
    temps_partiel = tpstrav == 2  # 2 = temps partiel dans l'ERFS
  ) |>
  group_by(annee, pauvre, groupe) |>
  summarise(
    part_tp   = 100 * sum(wprm * temps_partiel, na.rm = TRUE) / sum(wprm),
    part_sous = 100 * sum(wprm * (!is.na(sousemplr) & sousemplr == 1), na.rm = TRUE) /
                      sum(wprm * temps_partiel, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tooltip_tp   = paste0(groupe, " — ", annee, "\nTemps partiel : ", round(part_tp, 1), " %"),
    tooltip_sous = paste0(groupe, " — ", annee, "\nSous-emploi (parmi TP) : ", round(part_sous, 1), " %"),
    data_id      = paste0(pauvre, "_", annee)
  )

# Graphique : part du temps partiel
g_tp2_tpspartiel <- ggplot(tp_tpspartiel,
                            aes(x = factor(annee), y = part_tp, fill = groupe)) +
  geom_col_interactive(
    aes(tooltip = tooltip_tp, data_id = data_id),
    position = "dodge", width = 0.6
  ) +
  scale_fill_manual(values = c("Travailleurs pauvres" = "#e31a1c", "Travailleurs non pauvres" = "#1f78b4"), drop = FALSE, na.value = "grey50") +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "Part en emploi à temps partiel (%)",
    caption = paste0(caption_base,
                     "\nTemps partiel : variable tpstrav = 2. ",
                     "Sous-emploi : tpstrav = 2 et sousemplr = 1.")
  ) +
  theme_erfs()

saveRDS(g_tp2_tpspartiel, file.path(path_fig, "tp2_tpspartiel.rds"))
cat("tp2_tpspartiel : ok\n")

# ==============================================================================
# 7. Activité du conjoint — pauvres vs non pauvres
# ==============================================================================

if (any(!is.na(tp_base_full$acteu_cj))) {

  tp_actcj <- tp_base_full |>
    filter(!is.na(acteu_cj), lpr == 1, annee %in% dates_cles) |>
    mutate(
      groupe   = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres"),
      actcj_lb = case_when(
        acteu_cj == "Emploi"  ~ "Conjoint en emploi",
        acteu_cj == "Chomage" ~ "Conjoint au chômage",
        acteu_cj == "Inactif" ~ "Conjoint inactif",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(actcj_lb)) |>
    group_by(annee, pauvre, groupe, actcj_lb) |>
    summarise(poids = sum(wprm), .groups = "drop") |>
    group_by(annee, pauvre) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(actcj_lb, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(actcj_lb, "_", pauvre, "_", annee)
    ) |>
    ungroup()

  g_tp2_actcj <- ggplot(tp_actcj,
                         aes(x = factor(annee), y = part, fill = actcj_lb)) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.6
    ) +
    facet_wrap(~groupe) +
    scale_fill_manual(values = c(
      "Conjoint en emploi"  = "#1f78b4",
      "Conjoint au chômage" = "#ff7f00",
      "Conjoint inactif"    = "#e31a1c"
    )) +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(
      y       = "Répartition selon l'activité du conjoint (%)",
      fill    = NULL,
      caption = paste0(caption_base,
                       "\nRestreint aux PR en emploi vivant en couple.")
    ) +
    theme_erfs()

  saveRDS(g_tp2_actcj, file.path(path_fig, "tp2_actcj.rds"))
  cat("tp2_actcj : ok\n")
} else {
  cat("AVERTISSEMENT : variable acteu_cj absente — tp2_actcj non créé\n")
}

# ==============================================================================
# 8. Nombre d'enfants — pauvres vs non pauvres (PR en emploi)
# ==============================================================================

if (any(!is.na(tp_base_full$nb_enfants))) {

  tp_enfants <- tp_base_full |>
    filter(!is.na(nb_enfants), lpr == 1, annee %in% dates_cles) |>
    mutate(
      groupe       = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres"),
      nb_enf_cat   = case_when(
        nb_enfants == 0 ~ "Sans enfant",
        nb_enfants == 1 ~ "1 enfant",
        nb_enfants == 2 ~ "2 enfants",
        nb_enfants >= 3 ~ "3 enfants ou plus"
      ),
      nb_enf_cat   = factor(nb_enf_cat,
                             levels = c("Sans enfant", "1 enfant", "2 enfants", "3 enfants ou plus"))
    ) |>
    group_by(annee, pauvre, groupe, nb_enf_cat) |>
    summarise(poids = sum(wprm), .groups = "drop") |>
    group_by(annee, pauvre) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(nb_enf_cat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(nb_enf_cat, "_", pauvre, "_", annee)
    ) |>
    ungroup()

  g_tp2_enfants <- ggplot(tp_enfants,
                           aes(x = factor(annee), y = part, fill = nb_enf_cat)) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.6
    ) +
    facet_wrap(~groupe) +
    scale_fill_brewer(palette = "YlOrRd") +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(
      y       = "Répartition selon le nombre d'enfants (%)",
      fill    = NULL,
      caption = caption_base
    ) +
    theme_erfs()

  saveRDS(g_tp2_enfants, file.path(path_fig, "tp2_enfants.rds"))
  cat("tp2_enfants : ok\n")
} else {
  cat("AVERTISSEMENT : variable nb_enfants absente — tp2_enfants non créé\n")
}

# ==============================================================================
# 9. Taux de pauvreté par type de ménage (parmi les travailleurs) — longue période
# ==============================================================================

taux_tp_typmen <- tp_base_full |>
  filter(!is.na(typmen)) |>
  group_by(annee, typmen) |>
  summarise(
    taux = 100 * sum(wprm * pauvre, na.rm = TRUE) / sum(wprm),
    .groups = "drop"
  ) |>
  mutate(
    tooltip = paste0(typmen, " — ", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(typmen, "_", annee)
  )

g_tp2_taux_typmen <- ggplot(taux_tp_typmen,
                             aes(x = annee, y = taux, colour = typmen, group = typmen)) +
  geom_line_interactive(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    y       = "Taux de pauvreté des travailleurs (%)",
    caption = caption_base
  ) +
  theme_erfs()

saveRDS(g_tp2_taux_typmen, file.path(path_fig, "tp2_taux_typmen.rds"))
cat("tp2_taux_typmen : ok\n")

# ==============================================================================
# 10. Type de contrat — travailleurs pauvres vs non pauvres
#     Variables cdi/cdd/interim disponibles dans type_contrat à partir de 2015.
# ==============================================================================

annees_contrat <- unique(tp_base_full$annee[
  !is.na(tp_base_full$type_contrat) & tp_base_full$annee >= 2015
])

if (length(annees_contrat) >= 2) {
  # Garder 3 années clés (première, médiane, dernière)
  annees_contrat_sel <- sort(unique(c(
    min(annees_contrat),
    annees_contrat[ceiling(length(annees_contrat) / 2)],
    max(annees_contrat)
  )))

  tp_contrat_data <- tp_base_full |>
    filter(!is.na(type_contrat), annee %in% annees_contrat_sel) |>
    mutate(
      groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres"),
      type_contrat = factor(type_contrat,
        levels = c("CDI", "CDD", "Intérim", "Autre / Inconnu"))
    ) |>
    group_by(annee, groupe, type_contrat) |>
    summarise(poids = sum(wprm, na.rm = TRUE), .groups = "drop") |>
    group_by(annee, groupe) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(type_contrat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(type_contrat, "_", groupe, "_", annee)
    ) |>
    ungroup() |>
    filter(!is.na(type_contrat))

  pal_contrat <- c(
    "CDI"              = "#1f78b4",
    "CDD"              = "#ff7f00",
    "Intérim"          = "#e31a1c",
    "Autre / Inconnu"  = "#999999"
  )

  g_tp2_contrat <- ggplot(
    tp_contrat_data,
    aes(x = factor(annee), y = part, fill = type_contrat)
  ) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.65
    ) +
    facet_wrap(~groupe) +
    scale_fill_manual(values = pal_contrat, drop = FALSE, na.value = "grey80") +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(
      x       = NULL,
      y       = "Répartition par type de contrat (%)",
      fill    = NULL,
      caption = paste0(caption_base,
                       "\nVariables cdi/cdd/interim disponibles à partir de 2015.")
    ) +
    theme_erfs()

  saveRDS(g_tp2_contrat, file.path(path_fig, "tp2_contrat.rds"))
  cat("tp2_contrat : ok\n")
} else {
  cat("AVERTISSEMENT : type_contrat non disponible — tp2_contrat non créé\n")
}

cat("\n=== travailleurs_pauvres.R terminé ===\n")
