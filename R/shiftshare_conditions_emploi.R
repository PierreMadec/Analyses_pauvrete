# ==============================================================================
# shiftshare_conditions_emploi.R
#
# Extensions 1 + 2 de l'article "Pourquoi l'emploi ne protège plus..."
#
# EXTENSION 1 — Shift-share sur les conditions d'emploi
#   Pour chaque "type d'emploi" k défini par {pcs_cat × temps_partiel},
#   décompose la variation de la contribution à la pauvreté laborieuse en :
#     - effet_taux   : le taux de pauvreté de ce type a changé
#     - effet_struct : la part de ce type parmi les travailleurs a changé
#   Période : 2015-2023 (première année avec pcs_cat ET tpstrav).
#
# EXTENSION 2 — Profil des emplois nets créés
#   Pour chaque cellule (pcs × tps), calcule :
#     - la variation d'effectif (2015-2017 → 2021-2023)
#     - le taux de pauvreté en fin de période
#   Visualisation : scatter plot (taux pauvreté × variation effectifs),
#   bulle = emploi pauvrant ET en croissance.
#
# Figures produites :
#   shiftshare_cond_taux.rds     — shift-share par conditions (vue annuelle)
#   shiftshare_cond_total.rds    — bilan 2015-2023 par condition (bar chart)
#   emplois_crees_scatter.rds    — scatter profil emplois créés
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig     <- "figure"
caption_base <- paste0(
  "Source : INSEE, ERFS 2015-2023, calculs de l'auteur.\n",
  "Champ : personnes de référence du ménage (PR) en emploi, 18-64 ans.\n",
  "Types d'emploi : PCS × statut horaire (temps partiel / complet)."
)

# ==============================================================================
# 1. Base : PR en emploi, 18-64 ans, avec PCS et statut horaire
# ==============================================================================

base_cond <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi",
         !is.na(age_num), age_num >= 18, age_num <= 64,
         !is.na(wprm), wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std)) |>
  mutate(
    pauvre         = as.integer(nivviem < seuil_std),
    temps_partiel  = case_when(
      tpstrav == 2 ~ "Temps partiel",
      tpstrav == 1 ~ "Temps complet",
      TRUE         ~ NA_character_
    )
  ) |>
  # Garder uniquement les années avec pcs_cat ET temps_partiel disponibles
  filter(!is.na(pcs_cat), !is.na(temps_partiel))

# Vérifier la couverture temporelle
annees_cond <- sort(unique(base_cond$annee))
cat(sprintf("Années disponibles avec PCS + tpstrav : %s\n",
            paste(annees_cond, collapse = ", ")))

if (length(annees_cond) < 3) {
  cat("AVERTISSEMENT : pas assez d'années — scripts stoppé\n")
  stop("Données insuffisantes")
}

# Simplifier PCS en 3 niveaux pour la lisibilité
base_cond <- base_cond |>
  mutate(
    pcs_3 = case_when(
      pcs_cat %in% c("Cadres et prof. intellectuelles sup.",
                     "Professions intermediaires")          ~ "Qualifiés\n(cadres + prof. interm.)",
      pcs_cat %in% c("Employes", "Artisans, commercants, chefs d'entreprise",
                     "Agriculteurs exploitants")            ~ "Indépendants\n& employés non-ouvriers",
      pcs_cat == "Ouvriers"                                ~ "Ouvriers",
      TRUE                                                 ~ NA_character_
    )
  ) |>
  filter(!is.na(pcs_3))

# Label cellule
base_cond <- base_cond |>
  mutate(type_emploi = paste0(pcs_3, "\n(", temps_partiel, ")"))

# ==============================================================================
# 2. Statistiques par cellule et par année
# ==============================================================================

stats_cellule <- base_cond |>
  group_by(annee, pcs_3, temps_partiel, type_emploi) |>
  summarise(
    N_k  = sum(wprm),
    TP_k = weighted.mean(pauvre, wprm) * 100,
    .groups = "drop"
  ) |>
  group_by(annee) |>
  mutate(
    N_total = sum(N_k),
    part_k  = N_k / N_total * 100    # part en %
  ) |>
  ungroup()

# Année de base = première année disponible
annee_base <- min(annees_cond)

base_vals_cond <- stats_cellule |>
  filter(annee == annee_base) |>
  select(pcs_3, temps_partiel, type_emploi,
         TP0 = TP_k, part0 = part_k)

# ==============================================================================
# 3. Shift-share annuel vs année de base
# ==============================================================================

ss_cond <- stats_cellule |>
  filter(annee != annee_base) |>
  left_join(base_vals_cond, by = c("pcs_3", "temps_partiel", "type_emploi")) |>
  mutate(
    delta_TP   = TP_k   - TP0,
    delta_part = part_k - part0,
    # Contributions en pts de % (sur le taux de pauvreté laborieuse global)
    effet_taux   = delta_TP   * part0   / 100,
    effet_struct = TP0        * delta_part / 100,
    interaction  = delta_TP   * delta_part / 100,
    delta_contrib = effet_taux + effet_struct + interaction
  )

# ==============================================================================
# 4. FIGURE 1 — Bilan 2015 → 2023 : shift-share par condition
#    Bar chart horizontal : pour chaque type d'emploi, les deux effets
# ==============================================================================

annee_fin <- max(annees_cond)

ss_bilan <- ss_cond |>
  filter(annee == annee_fin) |>
  select(type_emploi, pcs_3, temps_partiel, TP0, TP_k, part0, part_k,
         effet_taux, effet_struct, interaction, delta_contrib) |>
  arrange(desc(delta_contrib))

ordre_type <- ss_bilan$type_emploi

ss_bilan_long <- ss_bilan |>
  pivot_longer(cols = c(effet_taux, effet_struct, interaction),
               names_to = "composante", values_to = "valeur") |>
  mutate(
    composante = case_when(
      composante == "effet_taux"   ~ "Effet taux\n(pauvreté conditionnelle)",
      composante == "effet_struct" ~ "Effet structure\n(part du type d'emploi)",
      composante == "interaction"  ~ "Interaction"
    ),
    composante = factor(composante, levels = c(
      "Effet structure\n(part du type d'emploi)",
      "Effet taux\n(pauvreté conditionnelle)",
      "Interaction"
    )),
    type_emploi = factor(type_emploi, levels = rev(ordre_type)),
    tooltip = paste0(
      type_emploi, "\n",
      sub("\n", " ", composante), " : ", sprintf("%+.3f", valeur), " pt\n",
      "Part ", annee_base, " : ", round(part0, 1), "% → ",
      annee_fin, " : ", round(part_k, 1), "%\n",
      "Tx pauvreté ", annee_base, " : ", round(TP0, 1), "% → ",
      annee_fin, " : ", round(TP_k, 1), "%"
    ),
    data_id = paste0(gsub("\n| ", "_", type_emploi), "_", gsub("\n| ", "_", composante))
  )

total_bilan <- ss_bilan |>
  mutate(type_emploi = factor(type_emploi, levels = rev(ordre_type)))

pal_comp_cond <- c(
  "Effet structure\n(part du type d'emploi)" = "#e31a1c",
  "Effet taux\n(pauvreté conditionnelle)"    = "#1f78b4",
  "Interaction"                              = "#b3b3b3"
)

g_ss_bilan <- ggplot(
  ss_bilan_long,
  aes(y = type_emploi, x = valeur, fill = composante)
) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.65
  ) +
  geom_point(
    data = total_bilan,
    aes(y = type_emploi, x = delta_contrib, fill = NULL),
    shape = 23, size = 3, fill = "black", colour = "white",
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_fill_manual(values = pal_comp_cond) +
  scale_x_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x       = sprintf("Variation de la contribution à la pauvreté laborieuse vs %d (pts de %%)",
                      annee_base),
    y       = NULL,
    fill    = NULL,
    caption = paste0(
      caption_base, "\n",
      "Le losange noir = variation totale de la contribution. ",
      "Effet taux : le type d'emploi est plus ou moins pauvrant. ",
      "Effet structure : ce type d'emploi représente une part plus ou moins grande des travailleurs."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_ss_bilan, file.path(path_fig, "shiftshare_cond_total.rds"))
cat("shiftshare_cond_total : ok\n")

# ==============================================================================
# 5. EXTENSION 2 — Scatter "profil des emplois créés"
#
# x = taux de pauvreté en t1 (2021-2023 mean)
# y = variation d'effectif t0→t1 (en % du total travailleurs à t0)
# taille = N_k(t1) (poids absolu)
# couleur = pcs_3
# forme = temps_partiel
#
# Quadrant haut-droit = emplois qui croissent ET sont pauvres → problème
# ==============================================================================

# Périodes pour la comparaison
annees_t0 <- annees_cond[annees_cond >= 2015 & annees_cond <= 2017]
annees_t1 <- annees_cond[annees_cond >= 2021 & annees_cond <= 2023]

if (length(annees_t0) == 0) annees_t0 <- annees_cond[1:min(2, length(annees_cond))]
if (length(annees_t1) == 0) annees_t1 <- annees_cond[(length(annees_cond)-1):length(annees_cond)]

cat(sprintf("Emplois créés : %d-%d vs %d-%d\n",
            min(annees_t0), max(annees_t0), min(annees_t1), max(annees_t1)))

scatter_t0 <- base_cond |>
  filter(annee %in% annees_t0) |>
  group_by(pcs_3, temps_partiel, type_emploi) |>
  summarise(N0 = sum(wprm), TP0 = weighted.mean(pauvre, wprm)*100, .groups = "drop") |>
  mutate(N_total0 = sum(N0), part0 = N0/N_total0*100) |>
  select(pcs_3, temps_partiel, type_emploi, N0, part0, TP0)

scatter_t1 <- base_cond |>
  filter(annee %in% annees_t1) |>
  group_by(pcs_3, temps_partiel, type_emploi) |>
  summarise(N1 = sum(wprm), TP1 = weighted.mean(pauvre, wprm)*100, .groups = "drop") |>
  mutate(N_total1 = sum(N1), part1 = N1/N_total1*100) |>
  select(pcs_3, temps_partiel, type_emploi, N1, part1, TP1)

scatter_data <- inner_join(scatter_t0, scatter_t1,
                           by = c("pcs_3", "temps_partiel", "type_emploi")) |>
  mutate(
    delta_part = part1 - part0,
    label      = gsub("\n", " — ", type_emploi),
    tooltip    = paste0(
      label, "\n",
      "Taux de pauvreté ", min(annees_t1), "-", max(annees_t1),
      " : ", round(TP1, 1), " %\n",
      "Variation de part : ", sprintf("%+.1f", delta_part), " pt\n",
      "(", round(part0, 1), " % → ", round(part1, 1), " %)"
    ),
    data_id    = gsub(" |/|\\(|\\)|\n", "_", label),
    quadrant   = case_when(
      delta_part > 0 & TP1 > 5  ~ "Croissance + pauvreté élevée",
      delta_part > 0 & TP1 <= 5 ~ "Croissance + faible pauvreté",
      delta_part < 0 & TP1 > 5  ~ "Recul + pauvreté élevée",
      TRUE                       ~ "Recul + faible pauvreté"
    )
  )

pal_quadrant <- c(
  "Croissance + pauvreté élevée"  = "#e31a1c",
  "Croissance + faible pauvreté"  = "#33a02c",
  "Recul + pauvreté élevée"       = "#ff7f00",
  "Recul + faible pauvreté"       = "#999999"
)

label_t0 <- sprintf("%d-%d", min(annees_t0), max(annees_t0))
label_t1 <- sprintf("%d-%d", min(annees_t1), max(annees_t1))

g_scatter <- ggplot(
  scatter_data,
  aes(x = TP1, y = delta_part, size = N1, colour = quadrant,
      shape = temps_partiel)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey50") +
  geom_vline(xintercept = 5, linetype = "dashed", linewidth = 0.4, colour = "grey50") +
  geom_point_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    alpha = 0.85, stroke = 0.5
  ) +
  ggrepel::geom_text_repel(
    aes(label = label),
    size = 3, colour = "grey30", max.overlaps = 20,
    segment.colour = "grey70", segment.size = 0.3
  ) +
  scale_colour_manual(values = pal_quadrant) +
  scale_size_continuous(range = c(3, 14), guide = "none") +
  scale_x_continuous(labels = label_number(suffix = " %"),
                     name   = sprintf("Taux de pauvreté du type d'emploi (%s, %%)",
                                      label_t1)) +
  scale_y_continuous(labels = label_number(suffix = " pt"),
                     name   = sprintf("Variation de la part dans les travailleurs\n%s → %s (pts de %%)",
                                      label_t0, label_t1)) +
  labs(
    colour  = NULL,
    shape   = NULL,
    caption = paste0(
      caption_base, "\n",
      "La taille des bulles est proportionnelle au nombre de travailleurs en ", label_t1, ".\n",
      "Quadrant haut-droit : types d'emploi en croissance dont les travailleurs ont un ",
      "taux de pauvreté élevé — contribution négative à l'équité."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_scatter, file.path(path_fig, "emplois_crees_scatter.rds"))
cat("emplois_crees_scatter : ok\n")

# ==============================================================================
# 6. FIGURE ANNEXE — Évolution annuelle du taux de pauvreté par condition
#    (ligne chart par type d'emploi)
# ==============================================================================

evol_cond <- stats_cellule |>
  mutate(
    type_emploi = factor(type_emploi),
    tooltip = paste0(type_emploi, " — ", annee, " : ", round(TP_k, 1), " %"),
    data_id = paste0(gsub("\n| ", "_", type_emploi), "_", annee)
  )

pal_pcs3 <- c(
  "Qualifiés\n(cadres + prof. interm.)"   = "#1f78b4",
  "Indépendants\n& employés non-ouvriers" = "#33a02c",
  "Ouvriers"                              = "#e31a1c"
)

g_evol_cond <- ggplot(
  evol_cond,
  aes(x = annee, y = TP_k, colour = pcs_3, linetype = temps_partiel)
) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = pal_pcs3) +
  scale_linetype_manual(values = c("Temps complet" = "solid", "Temps partiel" = "dashed")) +
  scale_x_continuous(breaks = annees_cond) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, NA)) +
  labs(
    y        = "Taux de pauvreté (%)",
    colour   = "Catégorie professionnelle",
    linetype = "Régime horaire",
    caption  = caption_base
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_evol_cond, file.path(path_fig, "shiftshare_cond_taux.rds"))
cat("shiftshare_cond_taux : ok\n")

cat("\n=== shiftshare_conditions_emploi.R terminé ===\n")
