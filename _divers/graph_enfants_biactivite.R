# ==============================================================================
# Graphiques : biactivité, enfants, PA
# ==============================================================================

library(tidyverse)
library(ggiraph)

cat("Chargement des données...\n")
source("charger_erfs.R")
source("decomposition_pauvrete.R")

# Graphique 1 : Biactivité par groupe d'enfants
g_biactivite <- data_all |>
  filter(annee >= 2015, grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    groupe_enfants = case_when(
      nb_enfants == "0 enfant" ~ "Sans enfant",
      nb_enfants %in% c("1 enfant", "2 enfants") ~ "1-2 enfants",
      TRUE ~ "3+ enfants"
    )
  ) |>
  group_by(annee, groupe_enfants) |>
  summarise(
    pct_biactif = 100 * sum(biactivite == "Bi-actif", na.rm = TRUE) / n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = annee, y = pct_biactif, color = groupe_enfants, group = groupe_enfants)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Taux de biactivité chez les couples (2015-2023)",
    x = "Année", y = "% de couples bi-actifs",
    color = "Groupe"
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Graphique 2 : Niveau de vie par groupe d'enfants
g_niveauvie <- data_all |>
  filter(annee >= 2015, grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    groupe_enfants = case_when(
      nb_enfants == "0 enfant" ~ "Sans enfant",
      nb_enfants %in% c("1 enfant", "2 enfants") ~ "1-2 enfants",
      TRUE ~ "3+ enfants"
    )
  ) |>
  group_by(annee, groupe_enfants) |>
  summarise(
    nv_moyen = weighted.mean(nivviem, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = annee, y = nv_moyen, color = groupe_enfants, group = groupe_enfants)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Niveau de vie moyen des couples (2015-2023)",
    x = "Année", y = "Niveau de vie mensuel (€)",
    color = "Groupe"
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Graphique 3 : Taux de pauvreté par biactivité et enfants
g_pauvrete <- data_all |>
  filter(annee >= 2015, grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    groupe_enfants = if_else(nb_enfants == "0 enfant", "Sans enfant", "Avec enfant(s)"),
    biactivite_label = factor(biactivite, levels = c("Bi-actif", "Mono-actif", "Sans emploi"))
  ) |>
  group_by(annee, groupe_enfants, biactivite_label) |>
  summarise(
    taux_pauvre = 100 * sum(pauvre, na.rm = TRUE) / n(),
    n = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(biactivite_label), n > 30) |>
  ggplot(aes(x = annee, y = taux_pauvre, color = biactivite_label, linetype = groupe_enfants)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Taux de pauvreté : couples bi-actifs vs autres (enfants)",
    x = "Année", y = "Taux de pauvreté (%)",
    color = "Statut emploi", linetype = "Groupe"
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Sauvegarde
ggsave("g_biactivite_enfants.png", g_biactivite, width = 11, height = 7, dpi = 300)
ggsave("g_niveauvie_enfants.png", g_niveauvie, width = 11, height = 7, dpi = 300)
ggsave("g_pauvrete_biactivite_enfants.png", g_pauvrete, width = 12, height = 7, dpi = 300)

cat("✓ Graphiques sauvegardés.\n")
