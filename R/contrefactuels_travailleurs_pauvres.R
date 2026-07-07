# ==============================================================================
# contrefactuels_travailleurs_pauvres.R
#
# Quantifie, par configuration de ménage, quel levier sortirait effectivement
# le travailleur pauvre de la pauvreté. Pour chaque travailleur pauvre (PR en
# emploi, ménage sous le seuil), on simule trois leviers et on teste si le
# niveau de vie repasse au-dessus du seuil :
#
#   A. Temps plein au même taux horaire : salaire -> salaire ETP
#   B. Temps plein au SMIC              : salaire -> max(SMIC, salaire ETP)
#   C. Second revenu d'activité         : +1 SMIC temps plein (ménages
#      mono-actifs en couple uniquement — un second adulte existe mais est inactif)
#
# Simulation statique et BRUTE : on ajoute le gain salarial au revenu disponible
# sans tenir compte de la reprise de prestations (PPA/RSA), ce qui en fait une
# borne haute de l'effet. Les leviers A/B s'appliquent à tous ; le levier C ne
# s'applique qu'aux couples mono-actifs (présence d'un second adulte inactif).
#
# Figure produite (figure/) :
#   tp_contrefactuel — part des travailleurs pauvres sortis du seuil, par levier
#                      et par configuration (2021-2023)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"

trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))
pm   <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
smic_an <- setNames(pm$smic_net * 12, as.character(pm$annee))

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, wprm, nivviem, nb_uci, revdispm, typmen, biactivite, ppa)

# Taux marginal de reprise de la prime d'activité sur un surcroît de revenu
# d'activité (la PPA bonifie 61 % des revenus d'activité du foyer puis les
# reprend à 100 % comme ressources -> reprise marginale nette ≈ 38 %).
TAUX_REPRISE_PPA <- 0.38

lab_levels <- c("Personne seule", "Famille monoparentale",
                "Couple mono-actif sans enfant", "Couple mono-actif avec enfant(s)",
                "Couple bi-actif avec enfant(s)")

base <- trav |>
  inner_join(hh, by = c("annee", "ident")) |>
  left_join(s |> select(annee, seuil_std), by = "annee") |>
  mutate(
    pauvre  = nivviem < seuil_std,
    smic    = smic_an[as.character(annee)],
    sal_etp = ifelse(!is.na(txtp) & txtp > 0 & temps_partiel == 1,
                     salaires_i / (txtp / 100), salaires_i),
    ppa         = coalesce(as.numeric(ppa), 0),
    couple_mono = biactivite != "Bi-actif" &
                  typmen %in% c("Couple sans enfant", "Couple avec enfant(s)"),
    config = case_when(
      typmen == "Personne seule"                                  ~ "Personne seule",
      typmen == "Famille monoparentale"                           ~ "Famille monoparentale",
      typmen == "Couple sans enfant"    & biactivite != "Bi-actif" ~ "Couple mono-actif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif" ~ "Couple mono-actif avec enfant(s)",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif" ~ "Couple bi-actif avec enfant(s)",
      TRUE ~ NA_character_),
    config = factor(config, levels = lab_levels)
  )

wpp <- base |>
  filter(lpr == 1, emploi == 1, age >= 18, age <= 64, pauvre,
         !is.na(config), annee %in% 2021:2023, !is.na(salaires_i))

# ── Simulation des leviers ────────────────────────────────────────────────────
# Gain BRUT de revenu d'activité, puis gain NET après reprise de la prime
# d'activité : net = brut - min(taux_reprise * brut, ppa_perçue) (la PPA ne peut
# pas devenir négative). Le niveau de vie augmente du gain net rapporté aux UC.
net_gain <- function(brut, ppa) brut - pmin(TAUX_REPRISE_PPA * brut, ppa)

simu <- wpp |>
  mutate(
    brut_A = pmax(0, sal_etp - salaires_i),
    brut_B = pmax(0, pmax(smic, sal_etp) - salaires_i),
    brut_C = ifelse(couple_mono, smic, NA_real_),
    nv_A = nivviem + net_gain(brut_A, ppa) / nb_uci,
    nv_B = nivviem + net_gain(brut_B, ppa) / nb_uci,
    nv_C = ifelse(couple_mono, nivviem + net_gain(brut_C, ppa) / nb_uci, NA_real_),
    sort_A = as.integer(nv_A >= seuil_std),
    sort_B = as.integer(nv_B >= seuil_std),
    sort_C = ifelse(couple_mono, as.integer(nv_C >= seuil_std), NA_integer_),
    # version brute (sans reprise) pour comparaison
    sort_B_brut = as.integer(nivviem + brut_B / nb_uci >= seuil_std)
  )

res <- simu |>
  group_by(config) |>
  summarise(
    `Passer à temps plein\n(même taux horaire)` = 100 * weighted.mean(sort_A, wprm, na.rm = TRUE),
    `Temps plein au SMIC`                        = 100 * weighted.mean(sort_B, wprm, na.rm = TRUE),
    `Ajouter un second revenu\n(1 SMIC)`         = 100 * weighted.mean(sort_C, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-config, names_to = "levier", values_to = "pct") |>
  filter(!is.nan(pct)) |>
  mutate(
    levier = factor(levier, levels = c("Passer à temps plein\n(même taux horaire)",
                                       "Temps plein au SMIC",
                                       "Ajouter un second revenu\n(1 SMIC)")),
    tooltip = paste0(config, "\n", gsub("\n", " ", levier), " : ",
                     round(pct), " % sortis du seuil"),
    data_id = paste0(gsub("[^a-z]", "", tolower(config)), "_",
                     gsub("[^a-z]", "", tolower(levier)))
  )

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}

pal_lev <- c("Passer à temps plein\n(même taux horaire)" = "#1f78b4",
             "Temps plein au SMIC"                        = "#e31a1c",
             "Ajouter un second revenu\n(1 SMIC)"         = "#33a02c")

g_cf <- ggplot(res, aes(x = config, y = pct, fill = levier)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = pal_lev) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 100)) +
  coord_flip() +
  labs(y = "Part des travailleurs pauvres de la configuration sortis du seuil",
       x = NULL,
       caption = paste0(
         "Source : INSEE, ERFS 2021-2023, calculs de l'auteur.\n",
         "Simulation statique, nette de la reprise de prime d'activité (≈ 38 % du gain). ",
         "Le second revenu n'est simulé que pour les couples mono-actifs.\n",
         "Champ : PR en emploi, 18-64 ans, ménage pauvre.")) +
  theme_erfs() +
  theme(legend.text = element_text(size = 8))
saveRDS(g_cf, file.path(path_fig, "tp_contrefactuel.rds"))
cat("tp_contrefactuel : ok\n")

# Table de contrôle : comparaison brut vs net pour le levier "temps plein au SMIC"
comp <- simu |> group_by(config) |>
  summarise(
    `TP SMIC (brut)` = round(100 * weighted.mean(sort_B_brut, wprm, na.rm = TRUE)),
    `TP SMIC (net)`  = round(100 * weighted.mean(sort_B, wprm, na.rm = TRUE)),
    .groups = "drop")
cat("Effet de la reprise de prime d'activité (levier temps plein au SMIC) :\n")
print(as.data.frame(comp), row.names = FALSE)
cat("\n=== contrefactuels_travailleurs_pauvres.R terminé ===\n")
