# ==============================================================================
# piste1_salaire_horaire.R
#
# Distribution du salaire horaire des travailleurs pauvres (2013-2023)
# Figures produites :
#   tp_salaire_horaire_distrib.rds  — densités ratio horaire/SMIC (pauvres vs non-pauvres)
#   tp_salaire_horaire_evol.rds     — évolution % au-dessus du SMIC horaire
# ==============================================================================

library(tidyverse)
library(ggiraph)

if (!exists("data_all") || !exists("seuils_annuels"))  {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
ti <- readRDS("figure/travailleurs_indiv.rds")

path_fig <- "figure"

# SMIC net annuel et horaire (net de cotisations salariales, 1820 h/an)
smic_net <- tibble(
  annee = 2013:2023,
  smic_net_annuel = c(13191, 13320, 13464, 13596, 13884, 14424,
                      14976, 15288, 15876, 17472, 18534)
) |> mutate(smic_net_horaire = smic_net_annuel / 1820)

# Conversion code txtppred → fraction de temps de travail
conv_txtp <- c("1" = 0.25, "2" = 0.50, "3" = 0.625, "4" = 0.75, "5" = 0.875)

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std)

ti_pr <- ti |>
  filter(lpr == 1) |>
  # En cas de doublon (ménage avec 2 PR), garder le noi le plus petit
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  mutate(
    quotite = case_when(
      temps_partiel == 0                               ~ 1.0,
      temps_partiel == 1 & !is.na(txtp) & txtp %in% 1:5 ~ conv_txtp[as.character(txtp)],
      TRUE ~ NA_real_
    )
  )

tp_indiv <- base |>
  left_join(ti_pr |> select(annee, ident, salaires_i, temps_partiel, quotite),
            by = c("annee", "ident")) |>
  filter(!is.na(quotite), quotite > 0, salaires_i > 0, annee >= 2013) |>
  left_join(smic_net, by = "annee") |>
  mutate(
    heures_ann   = 1820 * quotite,
    sal_horaire  = salaires_i / heures_ann,
    ratio_smic   = sal_horaire / smic_net_horaire,
    dessus_smic  = ratio_smic >= 1.0,
    pauvre_lab   = if_else(pauvre, "Travailleur pauvre", "Travailleur non pauvre")
  )

# ── Figure 1 : densités du ratio salaire horaire / SMIC ──────────────────────

pal2 <- c("Travailleur pauvre" = "#e31a1c", "Travailleur non pauvre" = "#1f78b4")

plot_data <- tp_indiv |>
  filter(ratio_smic <= 3) |>
  mutate(pauvre_lab = if_else(pauvre, "Travailleur pauvre", "Travailleur non pauvre"))

g_distrib <- ggplot(plot_data,
  aes(x = ratio_smic, weight = wprm, fill = pauvre_lab, colour = pauvre_lab)) +
  geom_density(alpha = 0.30, linewidth = 0.8, adjust = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  annotate("text", x = 1.04, y = Inf, label = "SMIC net horaire",
           hjust = 0, vjust = 1.5, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = pal2) +
  scale_colour_manual(values = pal2) +
  scale_x_continuous(labels = scales::label_number(suffix = "× SMIC"),
                     breaks = seq(0, 3, 0.5)) +
  labs(
    x = "Salaire horaire / SMIC net horaire",
    y = "Densité (pondérée)",
    fill = NULL, colour = NULL,
    caption = "Source : INSEE, ERFS 2013-2023, calculs de l'auteur.\nChamp : PR en emploi 18-64 ans avec quotité connue."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

saveRDS(g_distrib, file.path(path_fig, "tp_salaire_horaire_distrib.rds"))

# ── Figure 2 : évolution % TP pauvres au-dessus du SMIC horaire ──────────────

evol_smic <- tp_indiv |>
  filter(pauvre == TRUE) |>
  group_by(annee) |>
  summarise(
    pct_dessus = weighted.mean(dessus_smic, wprm, na.rm = TRUE),
    .groups = "drop"
  )

g_evol <- ggplot(evol_smic,
  aes(x = annee, y = pct_dessus,
      tooltip = sprintf("%d : %.0f %%", annee, 100 * pct_dessus),
      data_id = annee)) +
  geom_line_interactive(colour = "#e31a1c", linewidth = 1.1) +
  geom_point_interactive(colour = "#e31a1c", size = 2.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.6)) +
  scale_x_continuous(breaks = 2013:2023) +
  labs(
    x = NULL, y = "Part des travailleurs pauvres\nau-dessus du SMIC horaire",
    caption = "Source : INSEE, ERFS 2013-2023, calculs de l'auteur.\nChamp : PR en emploi pauvres 18-64 ans avec quotité connue."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_evol, file.path(path_fig, "tp_salaire_horaire_evol.rds"))

message("Piste 1 : figures sauvegardées.")
