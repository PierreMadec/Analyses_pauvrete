# ==============================================================================
# temps_partiel_config.R
#
# Part de temps partiel parmi les personnes de référence en emploi, selon la
# configuration du ménage, 2023-2024 : travailleurs pauvres vs ensemble.
#
# Mesure robuste : indicateur de temps partiel (TPPRED) uniquement, aucune
# reconstitution de salaire horaire.
#
# Figure produite : figure/tp_temps_partiel_config.rds
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"

trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, wprm, nivviem, typmen, biactivite)

lab_levels <- c("Ensemble", "Personne seule", "Famille monoparentale",
                "Couple monoactif sans enfant", "Couple monoactif avec enfant(s)",
                "Couple biactif avec enfant(s)")

base <- trav |>
  filter(lpr == 1) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  inner_join(hh, by = c("annee", "ident")) |>
  left_join(s |> select(annee, seuil_std), by = "annee") |>
  filter(emploi == 1, age >= 18, age <= 64, annee %in% 2023:2024, !is.na(temps_partiel)) |>
  mutate(
    pauvre = nivviem < seuil_std,
    config = case_when(
      typmen == "Personne seule"                                   ~ "Personne seule",
      typmen == "Famille monoparentale"                            ~ "Famille monoparentale",
      typmen == "Couple sans enfant"    & biactivite != "Bi-actif" ~ "Couple monoactif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif" ~ "Couple monoactif avec enfant(s)",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif" ~ "Couple biactif avec enfant(s)",
      TRUE ~ "Autre")
  )

part_tp <- function(df, grp) {
  bind_rows(
    df |> summarise(config = "Ensemble",
                    pauvres  = 100 * weighted.mean(temps_partiel[pauvre] == 1, wprm[pauvre], na.rm = TRUE),
                    ensemble = 100 * weighted.mean(temps_partiel == 1, wprm, na.rm = TRUE)),
    df |> filter(config != "Autre") |> group_by(config) |>
      summarise(pauvres  = 100 * weighted.mean(temps_partiel[pauvre] == 1, wprm[pauvre], na.rm = TRUE),
                ensemble = 100 * weighted.mean(temps_partiel == 1, wprm, na.rm = TRUE),
                .groups = "drop")
  )
}

tab <- part_tp(base) |>
  pivot_longer(c(pauvres, ensemble), names_to = "champ", values_to = "pct") |>
  mutate(
    champ  = factor(recode(champ, pauvres = "Travailleurs pauvres",
                                   ensemble = "Ensemble des personnes en emploi"),
                    levels = c("Travailleurs pauvres", "Ensemble des personnes en emploi")),
    config = factor(config, levels = rev(lab_levels)),
    tooltip = sprintf("%s — %s : %.0f %%", config, champ, pct)
  )

pal <- c("Travailleurs pauvres" = "#174695",
         "Ensemble des personnes en emploi" = "#86C6FF")

g <- ggplot(tab, aes(y = config, x = pct, fill = champ,
                     tooltip = tooltip, data_id = paste(config, champ))) +
  geom_col_interactive(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f", pct)),
            position = position_dodge(width = 0.7), hjust = -0.2, size = 3, colour = "grey30") +
  scale_fill_manual(values = pal, name = NULL) +
  scale_x_continuous(labels = label_number(suffix = " %"),
                     expand = expansion(mult = c(0, 0.12)), limits = c(0, 60)) +
  labs(x = "part travaillant à temps partiel", y = NULL,
       caption = paste0("Source : INSEE, ERFS 2023-2024, calculs de l'auteur.\n",
                        "Champ : personnes de référence en emploi, 18-64 ans.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g, file.path(path_fig, "tp_temps_partiel_config.rds"))
cat("tp_temps_partiel_config : ok\n")
print(as.data.frame(part_tp(base) |> mutate(across(where(is.numeric), round))), row.names = FALSE)
