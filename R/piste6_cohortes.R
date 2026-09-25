# ==============================================================================
# piste6_cohortes.R
#
# Pseudo-panel par cohorte d'âge × configuration (Piste 6)
# Chaque cohorte quinquennale est suivie sur toutes les années ERFS.
#
# Figures produites :
#   cohortes_jeunes.rds     — taux à 25-34 ans par cohorte (dégradation générationnelle)
#   cohortes_age_periode.rds — profil âge-pauvreté par période (2005-23)
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
    pauvre    = nivviem < seuil_std,
    naiss     = annee - age_num,
    cohorte5  = floor(naiss / 5) * 5,
    age5      = floor(age_num / 5) * 5,
    periode   = case_when(
      annee <= 2009 ~ "2005-2009",
      annee <= 2014 ~ "2010-2014",
      annee <= 2019 ~ "2015-2019",
      TRUE          ~ "2020-2024"
    )
  )

# ── Figure 1 : taux de pauvreté À ÂGE FIXE (27-31 ans) selon la cohorte ──────
# Tranche d'âge étroite + on ne garde que les cohortes quinquennales observées
# sur TOUS les âges 27..31 dans la fenêtre 2005-2024 (born 1975-79 -> 1990-94).
# Objectif : éliminer le biais de composition d'âge des cohortes extrêmes
# (les nées ~1970 ne sont vues qu'en fin de tranche 25-34, les nées ~1995 qu'en
# début), qui gonflait le dégradé apparent quand on prenait 25-34 ans en entier.

A1 <- 27L; A2 <- 31L

cohortes_jeunes <- base |>
  filter(age_num >= A1, age_num <= A2) |>
  group_by(cohorte5) |>
  summarise(
    tx           = weighted.mean(pauvre, wprm, na.rm = TRUE),
    ages_couv    = n_distinct(age_num),
    annee_centre = round(mean(annee)),
    n            = n(),
    .groups = "drop"
  ) |>
  filter(ages_couv == (A2 - A1 + 1), n >= 500) |>   # cohorte complète sur la tranche
  mutate(
    label_coh = sprintf("Nés\n%d-%d", cohorte5, cohorte5 + 4),
    cohorte5  = as.factor(cohorte5)
  )

g_jeunes <- ggplot(cohortes_jeunes,
  aes(x = cohorte5, y = tx,
      tooltip = sprintf("Cohorte %s — %.1f %%", cohorte5, 100 * tx),
      data_id = cohorte5)) +
  geom_col_interactive(fill = "#2674DD", alpha = 0.85, width = 0.65) +
  scale_x_discrete(labels = function(x) sprintf("Nés %s-%s", x, as.integer(x) + 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, 0.13), expand = c(0, 0.002)) +
  labs(
    x = "cohorte de naissance",
    y = sprintf("taux de pauvreté laborieuse à %d-%d ans", A1, A2),
    caption = paste0("Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
                     sprintf("Champ : PR en emploi %d-%d ans. Seules les cohortes observées sur\n", A1, A2),
                     "tous ces âges dans la période sont retenues (à âge comparable).")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

saveRDS(g_jeunes, file.path(path_fig, "cohortes_jeunes.rds"))
cat("cohortes_jeunes (27-31 ans, cohortes complètes) :\n"); print(as.data.frame(cohortes_jeunes[c("label_coh","tx","n")]))

# ── Figure 2 : profil âge-pauvreté par période ───────────────────────────────

age_periode <- base |>
  filter(age5 >= 20) |>
  group_by(periode, age5) |>
  summarise(
    tx = weighted.mean(pauvre, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    age_lab = sprintf("%d-%d", age5, age5 + 4),
    age_lab = factor(age_lab, levels = unique(age_lab[order(age5)])),
    periode = factor(periode, levels = c("2005-2009","2010-2014","2015-2019","2020-2024"))
  )

pal_per <- c("2005-2009" = "#a6cee3", "2010-2014" = "#1f78b4",
             "2015-2019" = "#ff7f00", "2020-2024" = "#e31a1c")

g_age <- ggplot(age_periode,
  aes(x = age_lab, y = tx, colour = periode, group = periode,
      tooltip = sprintf("%s — %s ans : %.1f %%", periode, age_lab, 100 * tx),
      data_id = paste(periode, age_lab))) +
  geom_line_interactive(linewidth = 1.0) +
  geom_point_interactive(size = 2.2) +
  scale_colour_manual(values = pal_per) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Tranche d'âge",
    y = "Taux de pauvreté laborieuse",
    colour = "Période",
    caption = paste0("Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans.")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_age, file.path(path_fig, "cohortes_age_periode.rds"))

message("Piste 6 : figures sauvegardées.")
