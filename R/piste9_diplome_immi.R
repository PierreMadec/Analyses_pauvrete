# ==============================================================================
# piste9_diplome_immi.R
#
# Taux de pauvreté laborieuse BRUT par diplôme et par origine migratoire —
# parallèle individuel à fig-tp3-taux-config (qui raisonne par configuration
# de ménage). Ces deux variables ne sont mobilisées, ailleurs dans l'article,
# que comme contribution à la décomposition Oaxaca (immi_cat) ; le diplôme en
# est même absent (couverture 2017-2023 insuffisante pour comparer à
# 2010-2012). Ici on regarde le niveau brut, pas la contribution à une
# variation.
#
# Champ : PR en emploi, 18-64 ans (identique au reste de l'article).
#
# Figures produites (figure/) :
#   tp_taux_diplome — taux de pauvreté laborieuse par diplôme, 2017-2023
#   tp_taux_immi    — taux de pauvreté laborieuse par origine migratoire, 2010-2023
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

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}

# ==============================================================================
# 1. Par diplôme (2017-2023, seules années où dip5 est renseigné)
# ==============================================================================

diplome_levels <- c("Sans diplome", "CEP/BEPC/Brevet", "CAP/BEP", "Bac", "Superieur au bac")

taux_diplome <- base |>
  filter(!is.na(diplome)) |>
  group_by(annee, diplome) |>
  summarise(taux = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE), n_obs = n(), .groups = "drop") |>
  filter(n_obs >= 30) |>
  mutate(
    diplome = factor(diplome, levels = diplome_levels),
    tooltip = paste0(diplome, "\n", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(gsub("[^a-z]", "", tolower(diplome)), "_", annee)
  )

pal_diplome <- setNames(
  c("#e31a1c", "#fb9a99", "#a6cee3", "#1f78b4", "#08306b"),
  diplome_levels
)

g_diplome <- ggplot(taux_diplome, aes(x = annee, y = taux, colour = diplome, group = diplome)) +
  geom_line_interactive(linewidth = 1.05) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  scale_colour_manual(values = pal_diplome) +
  scale_x_continuous(breaks = 2017:2023) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x = NULL, y = "Taux de pauvreté laborieuse",
    caption = paste0(
      "Source : INSEE, ERFS 2017-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, avec diplôme connu ",
      "(variable dip5 disponible à partir de 2017).")) +
  theme_erfs()

saveRDS(g_diplome, file.path(path_fig, "tp_taux_diplome.rds"))
cat("tp_taux_diplome : ok\n")

# ==============================================================================
# 2. Par origine migratoire (2010-2023)
# ==============================================================================

taux_immi <- base |>
  filter(!is.na(immi_cat)) |>
  group_by(annee, immi_cat) |>
  summarise(taux = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE), n_obs = n(), .groups = "drop") |>
  filter(n_obs >= 30) |>
  mutate(
    tooltip = paste0(immi_cat, "\n", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(gsub("[^a-z]", "", tolower(immi_cat)), "_", annee)
  )

pal_immi <- c("Immigre" = "#e31a1c", "Non immigre" = "#1f78b4")

g_immi <- ggplot(taux_immi, aes(x = annee, y = taux, colour = immi_cat, group = immi_cat)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = pal_immi) +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x = NULL, y = "Taux de pauvreté laborieuse",
    caption = paste0(
      "Source : INSEE, ERFS 2010-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, avec origine migratoire connue.")) +
  theme_erfs()

saveRDS(g_immi, file.path(path_fig, "tp_taux_immi.rds"))
cat("tp_taux_immi : ok\n")

cat("\n=== piste9_diplome_immi.R terminé ===\n")
