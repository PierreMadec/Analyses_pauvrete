# ==============================================================================
# intensite_genre_tp.R
#
# Deux approfondissements de l'analyse de la pauvreté laborieuse :
#
# 1. INTENSITÉ (poverty gap) : au-delà du taux (incidence), la profondeur de la
#    pauvreté. Écart relatif médian au seuil = (seuil - niveau de vie) / seuil,
#    par configuration et dans le temps. Champ : PR en emploi (cohérent avec le
#    reste de l'article).
#
# 2. GENRE : la pauvreté laborieuse au niveau INDIVIDUEL (tous les actifs occupés,
#    pas seulement les PR), pour ne pas masquer la surexposition des femmes que la
#    mesure au niveau du ménage tend à invisibiliser.
#
# Figures produites (figure/) :
#   tp_intensite_config — écart médian au seuil par configuration (2021-2023)
#   tp_intensite_evol   — écart médian au seuil des travailleurs pauvres (2005-2023)
#   tp_genre_taux       — taux de pauvreté laborieuse individuel par sexe (2005-2023)
#   tp_genre_routes     — routes de la pauvreté laborieuse par sexe (2021-2023)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"
trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, wprm, nivviem, nb_uci, typmen, biactivite)

lab_levels <- c("Personne seule", "Famille monoparentale",
                "Couple mono-actif sans enfant", "Couple mono-actif avec enfant(s)",
                "Couple bi-actif avec enfant(s)")

base <- trav |>
  inner_join(hh, by = c("annee", "ident")) |>
  left_join(s |> select(annee, seuil_std), by = "annee") |>
  mutate(
    pauvre = nivviem < seuil_std,
    gap    = ifelse(pauvre, (seuil_std - nivviem) / seuil_std * 100, NA_real_),
    sexe_lab = recode(as.character(sexe), "1" = "Homme", "2" = "Femme"),
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

# Médiane / quantile pondérés
wmed <- function(x, w) { i <- !is.na(x) & !is.na(w); x <- x[i]; w <- w[i]
  o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) >= sum(w) / 2)[1]] }

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}

# ==============================================================================
# 1. INTENSITÉ — écart médian au seuil par configuration (PR en emploi pauvres)
# ==============================================================================
wpp <- base |> filter(lpr == 1, emploi == 1, age >= 18, age <= 64, pauvre, !is.na(config))

int_cfg <- wpp |> filter(annee %in% 2021:2023) |>
  group_by(config) |>
  summarise(gap_med = wmed(gap, wprm), .groups = "drop") |>
  mutate(config = fct_reorder(config, gap_med),
         tooltip = paste0(config, "\nÉcart médian au seuil : ", round(gap_med), " %"),
         data_id = paste0("int_", gsub("[^a-z]", "", tolower(config))))

g_int_cfg <- ggplot(int_cfg, aes(x = config, y = gap_med)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       fill = "#6a3d9a", width = 0.7) +
  geom_text(aes(label = paste0(round(gap_med), " %")), hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = " %"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Écart médian au seuil de pauvreté (intensité)",
       caption = paste0(
         "Source : INSEE, ERFS 2021-2023, calculs de l'auteur.\n",
         "Écart relatif au seuil = (seuil − niveau de vie) / seuil. ",
         "Champ : PR en emploi pauvres, 18-64 ans.")) +
  theme_erfs()
saveRDS(g_int_cfg, file.path(path_fig, "tp_intensite_config.rds"))
cat("tp_intensite_config : ok\n")

# Intensité dans le temps (ensemble des travailleurs pauvres)
int_evol <- wpp |> group_by(annee) |>
  summarise(gap_med = wmed(gap, wprm), .groups = "drop") |>
  mutate(tooltip = paste0(annee, " : écart médian ", round(gap_med, 1), " %"),
         data_id = paste0("intev_", annee))

g_int_evol <- ggplot(int_evol, aes(x = annee, y = gap_med)) +
  geom_line_interactive(colour = "#6a3d9a", linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id),
                         colour = "#6a3d9a", size = 2.3) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Écart médian au seuil (intensité, %)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Champ : PR en emploi pauvres, 18-64 ans.")) +
  theme_erfs()
saveRDS(g_int_evol, file.path(path_fig, "tp_intensite_evol.rds"))
cat("tp_intensite_evol : ok\n")

# ==============================================================================
# 2. GENRE — pauvreté laborieuse au niveau individuel (tous actifs occupés)
# ==============================================================================
indiv <- base |> filter(emploi == 1, age >= 18, age <= 64, !is.na(sexe_lab))

genre_taux <- indiv |> group_by(annee, sexe_lab) |>
  summarise(taux = 100 * weighted.mean(pauvre, wprm, na.rm = TRUE), .groups = "drop") |>
  mutate(tooltip = paste0(sexe_lab, " — ", annee, " : ", round(taux, 1), " %"),
         data_id = paste0(sexe_lab, "_", annee))

g_genre_taux <- ggplot(genre_taux, aes(x = annee, y = taux, colour = sexe_lab, group = sexe_lab)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.3) +
  scale_colour_manual(values = c("Femme" = "#e31a1c", "Homme" = "#1f78b4")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Taux de pauvreté laborieuse individuel (%)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Champ : actifs occupés (niveau individuel), 18-64 ans.")) +
  theme_erfs()
saveRDS(g_genre_taux, file.path(path_fig, "tp_genre_taux.rds"))
cat("tp_genre_taux : ok\n")

# Temps partiel par sexe parmi TOUS les actifs occupés (2005-2023)
# Fait robuste (attribut individuel) : la précarité de durée est très genrée,
# alors même que le taux de pauvreté laborieuse individuel ne l'est pas — la
# mesure au niveau du ménage absorbe les bas revenus féminins des couples.
tp_sexe <- indiv |> group_by(annee, sexe_lab) |>
  summarise(part_tp = 100 * weighted.mean(temps_partiel, wprm, na.rm = TRUE), .groups = "drop") |>
  mutate(tooltip = paste0(sexe_lab, " — ", annee, " : ", round(part_tp), " % à temps partiel"),
         data_id = paste0("tp_", sexe_lab, "_", annee))

g_genre_tp <- ggplot(tp_sexe, aes(x = annee, y = part_tp, colour = sexe_lab, group = sexe_lab)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.3) +
  scale_colour_manual(values = c("Femme" = "#e31a1c", "Homme" = "#1f78b4")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Part à temps partiel parmi les actifs occupés (%)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Champ : actifs occupés (niveau individuel), 18-64 ans.")) +
  theme_erfs()
saveRDS(g_genre_tp, file.path(path_fig, "tp_genre_tempspartiel.rds"))
cat("tp_genre_tempspartiel : ok\n")

# Contrôles
cat("\n--- Intensité par config (2021-2023) ---\n")
print(as.data.frame(int_cfg |> select(config, gap_med) |> mutate(gap_med = round(gap_med, 1))), row.names = FALSE)
cat("\n--- Taux pauvreté laborieuse individuel par sexe ---\n")
print(as.data.frame(genre_taux |> filter(annee %in% c(2005, 2023)) |>
        mutate(taux = round(taux, 1))), row.names = FALSE)
cat("\n--- Temps partiel par sexe (actifs occupés, 2021-2023) ---\n")
print(as.data.frame(tp_sexe |> filter(annee %in% c(2005, 2023)) |>
        mutate(part_tp = round(part_tp))), row.names = FALSE)
cat("\n=== intensite_genre_tp.R terminé ===\n")
