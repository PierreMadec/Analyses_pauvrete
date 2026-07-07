# ==============================================================================
# piste4_quasi_pauvres.R
#
# Robustesse : quasi-pauvres et test de seuils alternatifs (Piste 4)
#
# Figures produites :
#   robustesse_seuils.rds   — taux de pauvreté laborieuse aux seuils 50/60/70 %
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
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(
    seuil_50 = seuil_std * (50 / 60),
    seuil_70 = seuil_std * (70 / 60),
    p50 = nivviem < seuil_50,
    p60 = nivviem < seuil_std,
    p70 = nivviem < seuil_70,
    quasi = nivviem >= seuil_std & nivviem < 1.20 * seuil_std
  )

robustesse <- base |>
  group_by(annee) |>
  summarise(
    tx_50   = weighted.mean(p50,    wprm, na.rm = TRUE),
    tx_60   = weighted.mean(p60,    wprm, na.rm = TRUE),
    tx_70   = weighted.mean(p70,    wprm, na.rm = TRUE),
    tx_quasi = weighted.mean(quasi, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-annee, names_to = "seuil", values_to = "taux") |>
  mutate(
    seuil_lab = case_when(
      seuil == "tx_50"   ~ "Seuil à 50 % de la médiane",
      seuil == "tx_60"   ~ "Seuil à 60 % (référence)",
      seuil == "tx_70"   ~ "Seuil à 70 % de la médiane",
      seuil == "tx_quasi" ~ "Zone 100-120 % (quasi-pauvres)"
    ),
    seuil_lab = factor(seuil_lab, levels = c(
      "Seuil à 50 % de la médiane",
      "Seuil à 60 % (référence)",
      "Seuil à 70 % de la médiane",
      "Zone 100-120 % (quasi-pauvres)"
    )),
    lty = if_else(seuil == "tx_60", "solid", "dashed")
  )

pal_seuils <- c(
  "Seuil à 50 % de la médiane"     = "#984ea3",
  "Seuil à 60 % (référence)"       = "#e31a1c",
  "Seuil à 70 % de la médiane"     = "#ff7f00",
  "Zone 100-120 % (quasi-pauvres)" = "#1f78b4"
)

g_rob <- ggplot(robustesse,
  aes(x = annee, y = taux, colour = seuil_lab, linetype = seuil_lab,
      tooltip = sprintf("%s — %d : %.1f %%", seuil_lab, annee, 100 * taux),
      data_id = paste(seuil_lab, annee))) +
  geom_line_interactive(linewidth = 1.0) +
  geom_point_interactive(size = 1.8) +
  scale_colour_manual(values = pal_seuils) +
  scale_linetype_manual(values = c(
    "Seuil à 50 % de la médiane"     = "dashed",
    "Seuil à 60 % (référence)"       = "solid",
    "Seuil à 70 % de la médiane"     = "dashed",
    "Zone 100-120 % (quasi-pauvres)" = "dotted"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  labs(
    x = NULL, y = "Taux", colour = NULL, linetype = NULL,
    caption = paste0("Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans.")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_rob, file.path(path_fig, "robustesse_seuils.rds"))

message("Piste 4 : figure sauvegardée.")
