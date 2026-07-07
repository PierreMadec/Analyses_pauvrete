# ==============================================================================
# piste2_calibrage_second_revenu.R
#
# Calibrage empirique du contrefactuel « second revenu » (Piste 2)
# On observe le salaire des conjoints dans les couples bi-actifs pour
# tester si l'hypothèse +1 SMIC du contrefactuel est réaliste.
#
# Figure produite :
#   cf_calibrage_second_revenu.rds  — distribution salaire CJ bi-actifs / SMIC
# ==============================================================================

library(tidyverse)
library(ggiraph)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
ti <- readRDS("figure/travailleurs_indiv.rds")

path_fig <- "figure"

smic_net <- tibble(
  annee = 2005:2023,
  smic_net_annuel = c(10800, 11160, 11520, 11880, 12240, 12480, 12840, 13020,
                      13191, 13320, 13464, 13596, 13884, 14424, 14976, 15288,
                      15876, 17472, 18534)
)

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std) |>
  left_join(smic_net, by = "annee")

# Apparier PR (lpr=1) et CJ (lpr=2) dans le même ménage
ti_pr <- ti |>
  filter(lpr == 1) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, salaires_pr = salaires_i, emploi_pr = emploi, age_pr = age)

ti_cj <- ti |>
  filter(lpr == 2) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, salaires_cj = salaires_i, emploi_cj = emploi, age_cj = age, sexe_cj = sexe)

couples_full <- ti_pr |>
  inner_join(ti_cj, by = c("annee", "ident")) |>
  inner_join(base |> select(annee, ident, wprm, biactivite, typmen2, diplome,
                             smic_net_annuel, groupe),
             by = c("annee", "ident"))

# Salaires des conjoints dans les couples bi-actifs (2019-2023)
biactifs_ref <- couples_full |>
  filter(biactivite == "Bi-actif", salaires_cj > 0, emploi_cj == 1,
         annee >= 2019) |>
  mutate(ratio_cj = salaires_cj / smic_net_annuel)

# Distribution du ratio salaire CJ / SMIC
# Statistiques clés pour annotation
med_ratio <- weighted.mean(biactifs_ref$ratio_cj, biactifs_ref$wprm, na.rm = TRUE)
pct_sous  <- weighted.mean(biactifs_ref$ratio_cj < 1, biactifs_ref$wprm, na.rm = TRUE)

plot_data2 <- biactifs_ref |> filter(ratio_cj <= 4)

g_calib <- ggplot(plot_data2, aes(x = ratio_cj, weight = wprm)) +
  geom_density(fill = "#1f78b4", colour = "#1f78b4", alpha = 0.25, linewidth = 1, adjust = 1.2) +
  geom_vline(xintercept = 1,         linetype = "dashed", colour = "#e31a1c", linewidth = 0.8) +
  geom_vline(xintercept = med_ratio, linetype = "solid",  colour = "#ff7f00", linewidth = 0.8) +
  annotate("text", x = 1.04, y = Inf,
           label = "Hypothèse\ncontrefactuel\n(1 SMIC)", hjust = 0, vjust = 1.3,
           size = 3.0, colour = "#e31a1c") +
  annotate("text", x = med_ratio + 0.05, y = Inf,
           label = sprintf("Médiane observée\n(%.1f SMIC)", med_ratio),
           hjust = 0, vjust = 2.8, size = 3.0, colour = "#ff7f00") +
  scale_x_continuous(labels = scales::label_number(suffix = "× SMIC"),
                     breaks = 0:4) +
  labs(
    x = "Salaire du conjoint / SMIC net annuel",
    y = "Densité (pondérée)",
    caption = sprintf(
      "Source : INSEE, ERFS 2019-2023, calculs de l'auteur.\n%d %% des conjoints bi-actifs gagnent moins d'1 SMIC net.",
      round(100 * pct_sous)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

saveRDS(g_calib, file.path(path_fig, "cf_calibrage_second_revenu.rds"))

message("Piste 2 : figure sauvegardée.")
