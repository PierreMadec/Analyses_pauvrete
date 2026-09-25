# ==============================================================================
# piste11_naf_secteur.R
#
# Taux de pauvreté laborieuse par secteur d'activité (nomenclature NA 10
# postes, cf. extract_naf.R), 2013-2023 — complément sectoriel au diagnostic
# par PCS (occupation) déjà mené en Partie 2/3 : quels secteurs, indépendamment
# de la profession exercée, concentrent la pauvreté laborieuse ?
#
# Champ : PR en emploi, 18-64 ans (identique au reste de l'article).
#
# Figure produite (figure/) :
#   tp_taux_naf — taux de pauvreté laborieuse par secteur d'activité,
#                 2013-2023 (pooled)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
naf  <- readRDS("figure/naf_secteur.rds")
trav <- readRDS("figure/travailleurs_indiv.rds")

path_fig <- "figure"

pr_noindiv <- trav |>
  filter(lpr == 1) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, noindiv)

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std) |>
  left_join(pr_noindiv, by = c("annee", "ident")) |>
  left_join(naf |> distinct(annee, noindiv, .keep_all = TRUE), by = c("annee", "noindiv")) |>
  filter(!is.na(secteur_naf))

taux_naf <- base |>
  group_by(secteur_naf) |>
  summarise(taux = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE), n_obs = n(), .groups = "drop") |>
  filter(n_obs >= 30) |>
  mutate(
    secteur_naf = fct_reorder(secteur_naf, taux),
    tooltip = paste0(secteur_naf, "\n", round(taux, 1), " %"),
    data_id = paste0("naf_", gsub("[^a-z]", "", tolower(secteur_naf)))
  )

g_naf <- ggplot(taux_naf, aes(x = secteur_naf, y = taux)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       fill = "#33a02c", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x = NULL, y = "Taux de pauvreté laborieuse",
    caption = paste0(
      "Source : INSEE, ERFS 2013-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, avec secteur d'activité connu (NA 10 postes).")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g_naf, file.path(path_fig, "tp_taux_naf.rds"))
cat("tp_taux_naf : ok\n")
print(as.data.frame(taux_naf |> select(secteur_naf, taux, n_obs) |> arrange(desc(taux))))

cat("\n=== piste11_naf_secteur.R terminé ===\n")
