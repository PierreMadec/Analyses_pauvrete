# ==============================================================================
# typologie_travailleurs_pauvres.R
#
# Diagnostic des causes proximales de la pauvreté laborieuse, par configuration
# de ménage. Pour chaque travailleur pauvre (PR en emploi, 18-64 ans, ménage
# sous le seuil), on caractérise :
#   - durée insuffisante : temps partiel et/ou sous-emploi
#   - salaire (taux horaire) insuffisant : salaire reconstitué en équivalent
#     temps plein (ETP) inférieur au SMIC net annuel temps plein
#   - absence de second revenu : ménage mono-actif (définition de la config)
#   - charges familiales : présence d'enfants (définition de la config)
#
# Référence salaire : SMIC net annuel temps plein (repère absolu) ; le salaire
# ETP (salaires_i / quotité) isole le déficit de taux horaire du déficit de durée.
#
# Champ : niveau de vie (pas de reste-à-vivre). Travailleurs pauvres uniquement.
#
# Figures produites (figure/) :
#   tp_diag_config   — causes proximales par configuration (2021-2023)
#   tp_diag_evol     — évolution des causes proximales dans le temps (2005-2023)
#   tp_compo_evol    — évolution de la composition des travailleurs pauvres
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"

# ── Construction de la base travailleurs pauvres ──────────────────────────────
trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))
pm   <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
smic_an <- setNames(pm$smic_net * 12, as.character(pm$annee))   # SMIC net annuel

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, wprm, nivviem, nb_uci, typmen, biactivite, nb_enfants)

lab_levels <- c("Personne seule", "Famille monoparentale",
                "Couple mono-actif sans enfant", "Couple mono-actif avec enfant(s)",
                "Couple bi-actif sans enfant", "Couple bi-actif avec enfant(s)")

base <- trav |>
  inner_join(hh, by = c("annee", "ident")) |>
  left_join(s |> select(annee, seuil_std), by = "annee") |>
  mutate(
    pauvre  = nivviem < seuil_std,
    smic    = smic_an[as.character(annee)],
    sal_etp = ifelse(!is.na(txtp) & txtp > 0 & temps_partiel == 1,
                     salaires_i / (txtp / 100), salaires_i),
    config = case_when(
      typmen == "Personne seule"                                  ~ "Personne seule",
      typmen == "Famille monoparentale"                           ~ "Famille monoparentale",
      typmen == "Couple sans enfant"    & biactivite == "Bi-actif" ~ "Couple bi-actif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif" ~ "Couple bi-actif avec enfant(s)",
      typmen == "Couple sans enfant"                              ~ "Couple mono-actif sans enfant",
      typmen == "Couple avec enfant(s)"                           ~ "Couple mono-actif avec enfant(s)",
      TRUE ~ NA_character_),
    config = factor(config, levels = lab_levels)
  )

trav_pauvres <- base |>
  filter(lpr == 1, emploi == 1, age >= 18, age <= 64, pauvre, !is.na(config))

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}
pal_causes <- c("Temps partiel"            = "#1f78b4",
                "Sous-emploi"              = "#a6cee3",
                "Salaire ETP < SMIC"       = "#e31a1c",
                "Un seul revenu d'activité" = "#6a3d9a",
                "Enfants à charge"         = "#33a02c")

# ==============================================================================
# Figure 1 : causes proximales par configuration (période récente 2021-2023)
# ==============================================================================
diag <- trav_pauvres |>
  filter(annee %in% 2021:2023) |>
  mutate(mono = as.integer(biactivite != "Bi-actif" | typmen %in%
                             c("Personne seule", "Famille monoparentale")),
         enfants = as.integer(grepl("enfant", config) | config == "Famille monoparentale")) |>
  group_by(config) |>
  summarise(
    `Temps partiel`             = 100 * weighted.mean(temps_partiel, wprm, na.rm = TRUE),
    `Sous-emploi`               = 100 * weighted.mean(sousemploi, wprm, na.rm = TRUE),
    `Salaire ETP < SMIC`        = 100 * weighted.mean(sal_etp < smic, wprm, na.rm = TRUE),
    `Un seul revenu d'activité` = 100 * weighted.mean(mono, wprm, na.rm = TRUE),
    `Enfants à charge`          = 100 * weighted.mean(enfants, wprm, na.rm = TRUE),
    part = sum(wprm),
    .groups = "drop"
  ) |>
  mutate(part = round(100 * part / sum(part)))

diag_long <- diag |>
  select(-part) |>
  pivot_longer(-config, names_to = "cause", values_to = "pct") |>
  mutate(cause = factor(cause, levels = names(pal_causes)),
         tooltip = paste0(config, "\n", cause, " : ", round(pct), " % des travailleurs pauvres"),
         data_id = paste0(gsub("[^a-z]", "", tolower(config)), "_",
                          gsub("[^a-z]", "", tolower(cause))))

g_diag <- ggplot(diag_long, aes(x = cause, y = pct, fill = cause)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.75) +
  facet_wrap(~ config, ncol = 2) +
  scale_fill_manual(values = pal_causes, guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 100)) +
  coord_flip() +
  labs(y = "Part des travailleurs pauvres de la configuration concernés",
       x = NULL,
       caption = paste0(
         "Source : INSEE, ERFS 2021-2023, calculs de l'auteur.\n",
         "Champ : personnes de référence en emploi, 18-64 ans, ménage pauvre. ",
         "Salaire ETP = salaire annuel rapporté à un temps plein. SMIC net annuel temps plein.")) +
  theme_erfs() +
  theme(strip.text = element_text(face = "bold", size = 9.5))
saveRDS(g_diag, file.path(path_fig, "tp_diag_config.rds"))
cat("tp_diag_config : ok\n")

# ==============================================================================
# Figure 2 : évolution des causes proximales dans le temps (le "de moins en moins")
# ==============================================================================
evol <- trav_pauvres |>
  group_by(annee) |>
  summarise(
    `Temps partiel`      = 100 * weighted.mean(temps_partiel, wprm, na.rm = TRUE),
    # Salaire ETP : la quotité (txtppred) n'existe qu'à partir de 2013 ; avant,
    # l'équivalent temps plein n'est pas reconstituable -> série non comparable,
    # restreinte à 2013-2023 pour éviter un faux décrochage.
    `Salaire ETP < SMIC` = ifelse(first(annee) >= 2013,
        100 * weighted.mean(sal_etp < smic, wprm, na.rm = TRUE), NA_real_),
    `Un seul revenu d'activité` = 100 * weighted.mean(
        as.integer(biactivite != "Bi-actif" |
                   typmen %in% c("Personne seule", "Famille monoparentale")), wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-annee, names_to = "cause", values_to = "pct") |>
  filter(!is.na(pct)) |>
  mutate(cause = factor(cause, levels = names(pal_causes)),
         tooltip = paste0(cause, "\n", annee, " : ", round(pct), " %"),
         data_id = paste0(gsub("[^a-z]", "", tolower(cause)), "_", annee))

g_evol <- ggplot(evol, aes(x = annee, y = pct, colour = cause, group = cause)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = pal_causes) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(y = "Part des travailleurs pauvres concernés", x = NULL,
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Le salaire ETP n'est disponible qu'à partir de 2013 (quotité de temps partiel). ",
         "Champ : PR en emploi, 18-64 ans, ménage pauvre.")) +
  theme_erfs()
saveRDS(g_evol, file.path(path_fig, "tp_diag_evol.rds"))
cat("tp_diag_evol : ok\n")

# ==============================================================================
# Figure 3 : évolution de la composition des travailleurs pauvres par config
# ==============================================================================
compo <- trav_pauvres |>
  group_by(annee, config) |>
  summarise(w = sum(wprm), .groups = "drop") |>
  group_by(annee) |>
  mutate(part = 100 * w / sum(w)) |>
  ungroup() |>
  mutate(tooltip = paste0(config, "\n", annee, " : ", round(part), " % des travailleurs pauvres"),
         data_id = paste0(gsub("[^a-z]", "", tolower(config)), "_", annee))

g_compo <- ggplot(compo, aes(x = annee, y = part, fill = config)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = "stack", width = 0.9,
                       colour = "white", linewidth = 0.15) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %"), expand = expansion(mult = c(0, 0.02))) +
  labs(y = "Composition des travailleurs pauvres (%)", x = NULL,
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Champ : PR en emploi, 18-64 ans, ménage pauvre.")) +
  theme_erfs() +
  theme(legend.text = element_text(size = 8))
saveRDS(g_compo, file.path(path_fig, "tp_compo_evol.rds"))
cat("tp_compo_evol : ok\n")

cat("\n=== typologie_travailleurs_pauvres.R terminé ===\n")
