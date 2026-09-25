# ==============================================================================
# piste8_type_contrat.R
#
# Teste une dimension absente du diagnostic principal : la précarité
# contractuelle (CDD, intérim, vs CDI) est-elle associée à la pauvreté
# laborieuse, et cette association s'est-elle creusée dans le temps ?
#
# type_contrat (CDI/CDD/Interim) est construit dans decomposition_pauvrete.R à
# partir de cdd/cdi/interim (ERFS 2023+) ou contra (2015-2020, fichier IRF) ;
# il n'est renseigné que pour les salariés (hors fonction publique statutaire
# et indépendants, pour lesquels la notion de CDI/CDD ne s'applique pas).
#
# Champ : PR en emploi, 18-64 ans (identique au reste de l'article).
#
# Figure produite (figure/) :
#   tp_contrat_evol — part en contrat précaire (CDD + intérim) parmi les
#                      travailleurs pauvres vs non pauvres, 2005-2023
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
  mutate(pauvre = nivviem < seuil_std)

evol_contrat <- base |>
  filter(!is.na(type_contrat)) |>
  mutate(precaire = type_contrat %in% c("CDD", "Interim")) |>
  group_by(annee, pauvre) |>
  summarise(pct = 100 * weighted.mean(precaire, wprm, na.rm = TRUE), n_obs = n(), .groups = "drop") |>
  filter(n_obs >= 30) |>
  mutate(
    statut  = if_else(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres"),
    tooltip = paste0(statut, "\n", annee, " : ", round(pct), " % en CDD/intérim"),
    data_id = paste0(statut, "_", annee)
  )

pal_statut <- c("Travailleurs pauvres" = "#e31a1c", "Travailleurs non pauvres" = "#1f78b4")

g_contrat <- ggplot(evol_contrat, aes(x = annee, y = pct, colour = statut, group = statut)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = pal_statut) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x = NULL, y = "Part en contrat précaire (CDD + intérim)",
    colour = NULL,
    caption = paste0(
      "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, avec type de contrat connu ",
      "(salariés hors fonction publique statutaire et indépendants).")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position   = "top",
    plot.caption      = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_contrat, file.path(path_fig, "tp_contrat_evol.rds"))
cat("tp_contrat_evol : ok\n")

cat("\n=== piste8_type_contrat.R terminé ===\n")
