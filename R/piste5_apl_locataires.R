# ==============================================================================
# piste5_apl_locataires.R
#
# Travailleurs pauvres, statut occupant et effet des APL (Piste 5)
#
# Figures produites :
#   apl_effet_statut.rds    — taux de pauvreté et effet APL par statut occupant
#   prop_pauvres_evol.rds   — part des propriétaires parmi les TP (évolution)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64,
         wprm > 0, !is.na(statut_occ)) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(
    pauvre     = nivviem           < seuil_std,
    pauvre_apl = nivviem_hors_apl < seuil_std,
    effet_apl  = pauvre_apl & !pauvre  # amortis par APL
  ) |>
  filter(statut_occ %in% c("Locataire", "Proprietaire"))

# ── Figure 1 : taux de pauvreté et effet APL par statut et par année ─────────

apl_statut <- base |>
  group_by(annee, statut_occ) |>
  summarise(
    tx_obs      = weighted.mean(pauvre,     wprm, na.rm = TRUE),
    tx_sans_apl = weighted.mean(pauvre_apl, wprm, na.rm = TRUE),
    effet_apl   = tx_sans_apl - tx_obs,
    .groups = "drop"
  ) |>
  mutate(statut_lab = if_else(statut_occ == "Locataire", "Locataire", "Propriétaire"))

pal_statut <- c("Locataire" = "#e31a1c", "Propriétaire" = "#1f78b4")

g_apl <- ggplot(apl_statut,
  aes(x = annee, colour = statut_lab)) +
  # Taux observé (ligne pleine)
  geom_line_interactive(aes(y = tx_obs,
    tooltip = sprintf("%s — %d : taux %.1f %%", statut_lab, annee, 100 * tx_obs),
    data_id = paste("obs", statut_lab, annee)), linewidth = 1.1) +
  # Taux sans APL (ligne pointillée)
  geom_line_interactive(aes(y = tx_sans_apl,
    tooltip = sprintf("%s — %d : sans APL %.1f %%", statut_lab, annee, 100 * tx_sans_apl),
    data_id = paste("sapl", statut_lab, annee)),
    linewidth = 0.7, linetype = "dashed") +
  scale_colour_manual(values = pal_statut) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  annotate("text", x = 2023, y = 0.14, label = "sans APL", hjust = 1.1,
           size = 3, colour = "grey40") +
  annotate("text", x = 2023, y = 0.12, label = "observé", hjust = 1.1,
           size = 3, colour = "grey40") +
  labs(
    x = NULL, y = "Taux de pauvreté laborieuse",
    colour = "Statut occupant",
    caption = paste0("Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans, locataires et propriétaires. ",
                     "Ligne pointillée = taux contrefactuel sans aides au logement.")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_apl, file.path(path_fig, "apl_effet_statut.rds"))

# ── Figure 2 : part des propriétaires parmi les TP ───────────────────────────

prop_tp <- base |>
  filter(pauvre) |>
  group_by(annee, statut_occ) |>
  summarise(n = sum(wprm, na.rm = TRUE), .groups = "drop") |>
  group_by(annee) |>
  mutate(pct = n / sum(n),
         statut_lab = if_else(statut_occ == "Locataire", "Locataire", "Propriétaire")) |>
  ungroup()

g_prop <- ggplot(prop_tp,
  aes(x = annee, y = pct, fill = statut_lab,
      tooltip = sprintf("%s — %d : %.0f %%", statut_lab, annee, 100 * pct),
      data_id = paste(statut_lab, annee))) +
  geom_area_interactive(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = pal_statut) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  labs(
    x = NULL, y = "Composition des travailleurs pauvres",
    fill = "Statut occupant",
    caption = paste0("Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
                     "Champ : PR en emploi pauvres 18-64 ans (locataires + propriétaires).")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_prop, file.path(path_fig, "prop_pauvres_evol.rds"))

message("Piste 5 : figures sauvegardées.")
