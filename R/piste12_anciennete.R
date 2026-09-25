# ==============================================================================
# piste12_anciennete.R
#
# Taux de pauvreté laborieuse par ancienneté dans l'entreprise (ancentr, en
# mois — cf. extract_anciennete.R), 2005-2019. Variable absente des ERFS
# 2020-2023 (disparue du questionnaire EEC après sa refonte) : la série
# s'arrête avant la période récente, contrairement au reste de l'article.
# Complète le diagnostic de précarité contractuelle (CDD/CDI) par une mesure
# continue de la stabilité dans l'emploi.
#
# Champ : PR en emploi, 18-64 ans, 2005-2019.
#
# Figure produite (figure/) :
#   tp_taux_anciennete — taux de pauvreté laborieuse par tranche d'ancienneté
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
anc  <- readRDS("figure/anciennete.rds")
trav <- readRDS("figure/travailleurs_indiv.rds")

path_fig <- "figure"

pr_noindiv <- trav |>
  filter(lpr == 1) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, noindiv)

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0, annee <= 2019) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std) |>
  left_join(pr_noindiv, by = c("annee", "ident")) |>
  left_join(anc |> distinct(annee, noindiv, .keep_all = TRUE), by = c("annee", "noindiv")) |>
  filter(!is.na(ancentr_mois))

anc_levels <- c("Moins d'1 an", "1 à 3 ans", "3 à 10 ans", "10 ans ou plus")

taux_anc <- base |>
  mutate(anc_cat = case_when(
    ancentr_mois < 12  ~ "Moins d'1 an",
    ancentr_mois < 36  ~ "1 à 3 ans",
    ancentr_mois < 120 ~ "3 à 10 ans",
    TRUE               ~ "10 ans ou plus"
  ), anc_cat = factor(anc_cat, levels = anc_levels)) |>
  group_by(anc_cat) |>
  summarise(taux = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE), n_obs = n(), .groups = "drop") |>
  mutate(
    tooltip = paste0(anc_cat, "\n", round(taux, 1), " %"),
    data_id = paste0("anc_", anc_cat)
  )

g_anc <- ggplot(taux_anc, aes(x = anc_cat, y = taux)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       fill = "#ff7f00", width = 0.6) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    x = NULL, y = "Taux de pauvreté laborieuse",
    caption = paste0(
      "Source : INSEE, ERFS 2005-2019, calculs de l'auteur.\n",
      "Champ : PR en emploi, 18-64 ans, avec ancienneté connue. ",
      "Variable indisponible en ERFS 2020-2023.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g_anc, file.path(path_fig, "tp_taux_anciennete.rds"))
cat("tp_taux_anciennete : ok\n")
print(as.data.frame(taux_anc))

cat("\n=== piste12_anciennete.R terminé ===\n")
