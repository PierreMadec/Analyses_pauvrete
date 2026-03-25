# ==============================================================================
# Analyse de la biactivité chez les couples avec enfants (2017-2023)
# ==============================================================================

# Charger les données ERFS et le script de décomposition
source("charger_erfs.R")
source("decomposition_pauvrete.R")

# Filtrer sur les couples uniquement (excluant les personnes seules, familles mono)
# Les typmen couples sont ceux commençant par "Couple"
taux_biactivite <- data_all |>
  filter(
    annee %in% c(2017, 2023),
    grepl("^Couple", typmen)  # uniquement les couples
  ) |>
  # Dédupliquer à 1 obs par ménage (biactivite est une variable ménage)
  distinct(ident, .keep_all = TRUE) |>
  group_by(annee, nb_enfants) |>
  summarise(
    n_couples = n(),
    n_biactifs = sum(biactivite == "Bi-actif", na.rm = TRUE),
    taux_biactivite_pct = 100 * n_biactifs / n_couples,
    .groups = "drop"
  ) |>
  arrange(nb_enfants, annee)

cat("=== Taux de biactivité des couples selon le nombre d'enfants ===\n\n")
print(taux_biactivite)

# Aussi : évolution année par année pour les couples avec 1-2 enfants
taux_evolution <- data_all |>
  filter(
    annee >= 2017,
    grepl("^Couple", typmen),
    nb_enfants %in% c(1, 2)
  ) |>
  distinct(ident, .keep_all = TRUE) |>
  group_by(annee, nb_enfants) |>
  summarise(
    taux_biactivite_pct = 100 * sum(biactivite == "Bi-actif", na.rm = TRUE) / n(),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = nb_enfants, values_from = taux_biactivite_pct,
              names_prefix = "enfants_")

cat("\n\n=== Évolution par année (couples avec 1 ou 2 enfants) ===\n\n")
print(taux_evolution)

# Graphique simple
library(ggplot2)

ggplot(
  data_all |>
    filter(
      annee >= 2017,
      grepl("^Couple", typmen),
      nb_enfants %in% c(1, 2)
    ) |>
    distinct(ident, .keep_all = TRUE) |>
    group_by(annee, nb_enfants) |>
    summarise(
      taux = 100 * sum(biactivite == "Bi-actif", na.rm = TRUE) / n(),
      .groups = "drop"
    ),
  aes(x = annee, y = taux, color = factor(nb_enfants), group = nb_enfants)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Taux de biactivité chez les couples avec enfants (2017-2023)",
    x = "Année", y = "Taux de biactivité (%)",
    color = "Nombre\nd'enfants"
  ) +
  scale_x_continuous(breaks = 2017:2023) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("biactivite_couples_enfants.png", width = 10, height = 6, dpi = 300)
cat("\n✓ Graphique sauvegardé : biactivite_couples_enfants.png\n")
