# ==============================================================================
# piste10_urbanisation.R
#
# Le degré d'urbanisation (rural / urbain hors Paris / Paris), harmonisé sur
# 2005-2023 à partir de trois variables ERFS successives (tur5, tuu2010r,
# tuu2020 — cf. extract_urbanisation.R), est mobilisé pour affiner le
# diagnostic logement de la Partie 4 : l'effet amortisseur des APL est-il
# homogène selon la zone, ou la baisse déjà documentée depuis 2017 masque-t-
# elle des trajectoires différentes ?
#
# Champ : PR en emploi, 18-64 ans, locataires (l'APL est marginale pour les
# propriétaires).
#
# Figure produite (figure/) :
#   apl_effet_zone — taux de pauvreté observé vs sans APL, par zone
#                     d'urbanisation, 2021-2023
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
urbanisation <- readRDS("figure/urbanisation.rds")

path_fig <- "figure"

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0,
         statut_occ == "Locataire") |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  left_join(urbanisation, by = c("annee", "ident")) |>
  filter(!is.na(zone_urbaine)) |>
  mutate(
    pauvre          = nivviem < seuil_std,
    pauvre_sans_apl = nivviem_hors_apl < seuil_std
  )

apl_zone <- base |>
  filter(annee %in% 2021:2023) |>
  group_by(zone_urbaine) |>
  summarise(
    tx_obs  = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE),
    tx_sans = 100 * weighted.mean(pauvre_sans_apl, wprm, na.rm = TRUE),
    n_obs   = n(),
    .groups = "drop"
  ) |>
  mutate(
    effet_apl = tx_sans - tx_obs,
    zone_urbaine = factor(zone_urbaine, levels = c("Paris", "Urbain hors Paris", "Rural")),
    tooltip = paste0(zone_urbaine, "\nAvec APL : ", round(tx_obs, 1), " %",
                     "\nSans APL : ", round(tx_sans, 1), " %",
                     "\nEffet amortisseur : ", round(effet_apl, 1), " pts"),
    data_id = paste0("zone_", zone_urbaine)
  )

g_apl_zone <- ggplot(apl_zone, aes(x = zone_urbaine)) +
  geom_col_interactive(aes(y = effet_apl, tooltip = tooltip, data_id = data_id),
                       fill = "#1f78b4", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    x = NULL, y = "Effet amortisseur des APL (pts de taux de pauvreté laborieuse)",
    caption = paste0(
      "Source : INSEE, ERFS 2021-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, locataires. ",
      "Zone d'urbanisation harmonisée 2005-2023 (tur5/tuu2010r/tuu2020).")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g_apl_zone, file.path(path_fig, "apl_effet_zone.rds"))
cat("apl_effet_zone : ok\n")
print(as.data.frame(apl_zone |> select(zone_urbaine, tx_obs, tx_sans, effet_apl, n_obs)))

cat("\n=== piste10_urbanisation.R terminé ===\n")
