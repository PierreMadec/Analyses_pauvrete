# ==============================================================================
# pouvoir_achat_menages.R — Reconstruction du pouvoir d'achat des ménages
#
# Objet : reconstituer, à partir des comptes nationaux trimestriels de l'INSEE
# (compte de revenu des ménages, base 2020), une série annuelle et trimestrielle
# de l'évolution du pouvoir d'achat des ménages, avec :
#   - le niveau (Mds €) et la contribution de chaque composante du revenu
#     disponible brut (RDB) : salaires bruts, EBE/revenu mixte, revenus de la
#     propriété, prestations sociales, cotisations sociales, impôts ;
#   - le pouvoir d'achat total, par personne, par ménage et par unité de
#     consommation (UC), en indice et en taux de croissance (séries officielles
#     INSEE) ;
#   - une estimation du niveau en euros courants et constants du RDB par
#     ménage et par UC.
#
# Sources INSEE (téléchargées le 2026-09-25, base 2020) :
#   - data/insee_pouvoir_achat/t_men_val.xls
#       Compte trimestriel des ménages (CVS-CJO), 1949T1 à aujourd'hui.
#       Insee, "Revenu, pouvoir d'achat et comptes des ménages", Comptes
#       nationaux trimestriels : https://www.insee.fr/fr/statistiques/8666489
#   - data/insee_pouvoir_achat/t_pouvachat_val.xls
#       Pouvoir d'achat et ratios des comptes des ménages (mêmes réserves).
#   - data/insee_pouvoir_achat/reve_pouv_achat_annuel.xlsx
#       Séries annuelles 1960-2025 (évolutions en %) :
#       https://www.insee.fr/fr/statistiques/2830244
#   - data/insee_pouvoir_achat/menages_structure_familiale.xlsx
#       Structure des ménages (recensements 1999-2016), utilisée pour ancrer
#       le nombre de ménages et estimer le nombre d'UC :
#       https://www.insee.fr/fr/statistiques/4277630
#   - data/insee_pouvoir_achat/uc_par_type_menage_2016.csv
#       Table intermédiaire, construite à la main à partir du fichier
#       précédent, documentant l'estimation du nombre moyen d'UC par type de
#       ménage en 2016 (échelle d'équivalence OCDE modifiée : 1 / 0.5 / 0.3).
#
# ATTENTION — Ce que cette reconstruction peut affirmer et ce qu'elle ne peut pas :
#   - Le RDB, sa décomposition en composantes et les taux de croissance du
#     pouvoir d'achat (total, par personne, par ménage, par UC) sont des
#     séries officielles INSEE, exactes.
#   - Le nombre de ménages en niveau est ancré sur les recensements (1999,
#     2006, 2011, 2016) puis chaîné avec le taux de croissance officiel INSEE
#     de "Nombre de ménages" : l'écart entre le chaînage et les benchmarks de
#     recensement est < 0,05 % sur 1999-2016 (voir contrôle en fin de script).
#   - Le nombre d'UC en niveau n'est PAS publié par l'INSEE : il est estimé
#     ici à partir de la structure des ménages par type (recensement 2016) et
#     de l'échelle d'équivalence OCDE modifiée (cf. uc_par_type_menage_2016.csv),
#     puis chaîné avec le taux de croissance officiel INSEE de "Nombre
#     d'unités de consommation". Le "euros par UC" qui en résulte est donc une
#     ESTIMATION (ordre de grandeur ± 5-10 %), à la différence du "euros par
#     ménage" qui repose sur un ancrage solide. Le taux de croissance du
#     pouvoir d'achat par UC, lui, reste la série officielle exacte.
# ==============================================================================

library(readxl)
library(tidyverse)
library(ggiraph)

dir_data        <- "data/insee_pouvoir_achat"
dir_graphiques  <- "figure"
if (!dir.exists(dir_graphiques)) dir.create(dir_graphiques, recursive = TRUE)

# ==============================================================================
# 1. Compte trimestriel des ménages : niveaux et contributions au RDB
# ==============================================================================

noms_postes <- c(
  "trimestre", "ebe_ei", "ebe_hors_ei", "ebe_total", "salaires_bruts",
  "interets_dividendes_nets", "prestations_sociales", "autres_ressources_nettes",
  "total_ressources", "impots_revenu_patrimoine", "cotisations_sociales",
  "total_charges", "rdb", "transferts_nature", "rdb_ajuste"
)

lire_feuille_men <- function(feuille) {
  read_excel(file.path(dir_data, "t_men_val.xls"), sheet = feuille, col_names = FALSE) |>
    set_names(noms_postes) |>
    filter(str_detect(trimestre, "^[0-9]{4}T[1-4]$")) |>
    mutate(
      across(-trimestre, as.numeric),
      annee = as.integer(str_sub(trimestre, 1, 4)),
      trim  = as.integer(str_sub(trimestre, 6, 6))
    ) |>
    arrange(annee, trim)
}

niveaux_trim      <- lire_feuille_men("Niveaux")       # Mds € courants, CVS-CJO
contributions_trim <- lire_feuille_men("Contributions") # points de croissance du RDB

# Contribution "prix" trimestrielle (déflateur de la consommation), pour
# reconstituer la contribution des postes à l'évolution du pouvoir d'achat
# (et non seulement du RDB nominal) : cf. Insee, "Informations rapides",
# décomposition usuelle = contributions nominales + effet prix.
noms_pa <- c(
  "trimestre", "g_rdb", "g_rdb_uc", "g_rdb_ajuste", "g_rdb_ajuste_uc",
  "g_prix", "g_pa_rdb", "g_pa_rdb_uc", "g_pa_rdb_ajuste", "g_pa_rdb_ajuste_uc",
  "taux_epargne", "epargne_financiere", "taux_epargne_financiere"
)

pouvachat_trim <- read_excel(file.path(dir_data, "t_pouvachat_val.xls"),
                              sheet = "Evolutions", col_names = FALSE) |>
  set_names(noms_pa) |>
  filter(str_detect(trimestre, "^[0-9]{4}T[1-4]$")) |>
  mutate(
    across(-trimestre, as.numeric),
    annee = as.integer(str_sub(trimestre, 1, 4)),
    trim  = as.integer(str_sub(trimestre, 6, 6))
  ) |>
  arrange(annee, trim)

# Table trimestrielle complète : contributions nominales + effet prix
pa_trimestriel <- contributions_trim |>
  select(trimestre, annee, trim,
         contrib_ebe = ebe_total, contrib_salaires = salaires_bruts,
         contrib_propriete = interets_dividendes_nets,
         contrib_prestations = prestations_sociales,
         contrib_autres = autres_ressources_nettes,
         contrib_impots = impots_revenu_patrimoine,
         contrib_cotisations = cotisations_sociales,
         contrib_rdb_nominal = rdb) |>
  left_join(pouvachat_trim |> select(trimestre, g_prix, g_pa_rdb, g_pa_rdb_uc),
            by = "trimestre") |>
  mutate(contrib_prix = -g_prix,
         pouvoir_achat_reconstitue = contrib_rdb_nominal + contrib_prix)

# ==============================================================================
# 2. Séries annuelles officielles de pouvoir d'achat (1960-2025)
# ==============================================================================

lire_figure1_annuel <- function() {
  raw <- read_excel(file.path(dir_data, "reve_pouv_achat_annuel.xlsx"),
                     sheet = "Figure 1", col_names = TRUE, skip = 3)
  names(raw)[1] <- "indicateur"
  raw |>
    filter(!is.na(indicateur)) |>
    mutate(cle = case_when(
      str_starts(indicateur, "Revenu disponible brut")            ~ "g_rdb_nominal",
      str_starts(indicateur, "Indice du prix")                     ~ "g_deflateur",
      str_detect(indicateur, "^Pouvoir d.achat du revenu")         ~ "g_pa_total",
      str_detect(indicateur, "^Pouvoir d.achat par personne")      ~ "g_pa_personne",
      str_detect(indicateur, "^Pouvoir d.achat par m.nage")        ~ "g_pa_menage",
      str_detect(indicateur, "^Pouvoir d.achat par unit")          ~ "g_pa_uc",
      str_detect(indicateur, "^Taux d.épargne")                ~ "taux_epargne",
      str_starts(indicateur, "Population moyenne")                 ~ "g_population",
      str_starts(indicateur, "Nombre de m")                        ~ "g_nb_menages",
      str_detect(indicateur, "^Nombre d.unit")                     ~ "g_nb_uc",
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(cle)) |>
    select(-indicateur) |>
    pivot_longer(-cle, names_to = "annee_brut", values_to = "valeur") |>
    mutate(annee = as.integer(str_extract(annee_brut, "[0-9]{4}")),
           valeur = as.numeric(valeur)) |>
    filter(!is.na(annee)) |>
    select(cle, annee, valeur) |>
    pivot_wider(names_from = cle, values_from = valeur) |>
    arrange(annee)
}

pa_annuel_officiel <- lire_figure1_annuel()

# ==============================================================================
# 3. Chaînage des niveaux : nombre de ménages et nombre d'UC
# ==============================================================================

# Chaîne un taux de croissance annuel (%) à partir d'une année-ancre connue,
# en avant et en arrière. `croissance` est le taux de croissance de l'année
# n par rapport à n-1 (aligné sur `annees`, même longueur, NA la 1re année).
chainer_niveau <- function(annees, croissance_pct, annee_ancre, valeur_ancre) {
  stopifnot(!is.unsorted(annees))
  n <- length(annees)
  valeurs <- rep(NA_real_, n)
  i_ancre <- which(annees == annee_ancre)
  valeurs[i_ancre] <- valeur_ancre
  if (i_ancre < n) {
    for (i in (i_ancre + 1):n) {
      valeurs[i] <- valeurs[i - 1] * (1 + croissance_pct[i] / 100)
    }
  }
  if (i_ancre > 1) {
    for (i in (i_ancre - 1):1) {
      valeurs[i] <- valeurs[i + 1] / (1 + croissance_pct[i + 1] / 100)
    }
  }
  valeurs
}

# --- Ancrage "nombre de ménages" sur le recensement 2016 -------------------
menages_structure <- read_excel(
  file.path(dir_data, "menages_structure_familiale.xlsx"),
  sheet = "Figure 1", col_names = FALSE
)
# La feuille contient, en colonne A, les libellés de type de ménage et, dans
# les colonnes suivantes, les couples (effectif en milliers ; % ) pour les
# recensements 1999 / 2006 / 2011 / 2016. On récupère uniquement le total
# 2016 (dernière paire de colonnes) pour ancrer le nombre de ménages.
# NB : la feuille source a une plage utilisée anormalement large (colonnes
# vides jusqu'à ~AMJ), donc on repère la colonne par position fixe (8 = total
# 2016 en milliers : label, 1999 milliers/%, 2006 milliers/%, 2011 milliers/%,
# 2016 milliers/%) plutôt que par ncol().
col1_structure   <- as.character(menages_structure[[1]])
idx_ligne_total  <- which(str_starts(coalesce(col1_structure, ""), "Total"))[1]
nb_menages_2016_milliers <- if (!is.na(idx_ligne_total)) {
  as.numeric(menages_structure[[idx_ligne_total, 8]])
} else {
  NA_real_
}
if (is.na(nb_menages_2016_milliers)) nb_menages_2016_milliers <- 29235.939  # repli documenté (cf. commentaire ci-dessus)

# --- Estimation du nombre d'UC en 2016 (cf. uc_par_type_menage_2016.csv) ---
uc_par_type <- read_csv(file.path(dir_data, "uc_par_type_menage_2016.csv"),
                         show_col_types = FALSE) |>
  mutate(
    uc_moyen = part_avec_enfants_mineurs * uc_estime_avec_mineurs +
      (1 - part_avec_enfants_mineurs) * uc_estime_sans_mineurs,
    uc_total_milliers = nb_menages_milliers * uc_moyen
  )
nb_uc_2016_milliers      <- sum(uc_par_type$uc_total_milliers)
ratio_uc_par_menage_2016 <- nb_uc_2016_milliers / sum(uc_par_type$nb_menages_milliers)

# --- Chaînage sur la série annuelle -----------------------------------------
pa_annuel <- pa_annuel_officiel |>
  mutate(
    nb_menages_milliers = chainer_niveau(annee, g_nb_menages, 2016, nb_menages_2016_milliers),
    nb_uc_milliers       = chainer_niveau(annee, g_nb_uc, 2016, nb_uc_2016_milliers),
    deflateur_indice      = chainer_niveau(annee, g_deflateur, max(annee, na.rm = TRUE), 100)
  )

# --- Contrôle de cohérence : chaînage vs. benchmarks de recensement --------
benchmarks_recensement <- tibble(
  annee = c(1999, 2006, 2011, 2016),
  nb_menages_recensement_milliers = c(24332.3, 26695.505, 28041.405, 29235.939)
)
controle_chainage <- benchmarks_recensement |>
  left_join(pa_annuel |> select(annee, nb_menages_milliers), by = "annee") |>
  mutate(ecart_pct = 100 * (nb_menages_milliers - nb_menages_recensement_milliers) /
           nb_menages_recensement_milliers)
message("Contrôle du chaînage du nombre de ménages (recensement vs. chaînage Insee) :")
print(controle_chainage)

# ==============================================================================
# 4. Décomposition annuelle du RDB par composante (niveaux et contributions)
# ==============================================================================

niveaux_annuel <- niveaux_trim |>
  group_by(annee) |>
  summarise(across(ebe_ei:rdb_ajuste, sum), .groups = "drop") |>
  arrange(annee)

compo_annuel <- niveaux_annuel |>
  mutate(
    contrib_salaires    = 100 * (salaires_bruts - lag(salaires_bruts)) / lag(rdb),
    contrib_ebe         = 100 * (ebe_total - lag(ebe_total)) / lag(rdb),
    contrib_propriete   = 100 * (interets_dividendes_nets - lag(interets_dividendes_nets)) / lag(rdb),
    contrib_prestations = 100 * (prestations_sociales - lag(prestations_sociales)) / lag(rdb),
    contrib_autres      = 100 * (autres_ressources_nettes - lag(autres_ressources_nettes)) / lag(rdb),
    contrib_impots      = -100 * (impots_revenu_patrimoine - lag(impots_revenu_patrimoine)) / lag(rdb),
    contrib_cotisations = -100 * (cotisations_sociales - lag(cotisations_sociales)) / lag(rdb),
    contrib_rdb_nominal = 100 * (rdb - lag(rdb)) / lag(rdb)
  ) |>
  left_join(pa_annuel |> select(annee, g_deflateur, g_pa_total, g_pa_menage, g_pa_uc,
                                 nb_menages_milliers, nb_uc_milliers, deflateur_indice),
            by = "annee") |>
  mutate(
    contrib_prix = -g_deflateur,
    pouvoir_achat_reconstitue = contrib_rdb_nominal + contrib_prix,
    euros_par_menage = rdb * 1e9 / (nb_menages_milliers * 1e3),
    euros_par_uc      = rdb * 1e9 / (nb_uc_milliers * 1e3),
    euros_par_menage_reel = euros_par_menage * 100 / deflateur_indice,
    euros_par_uc_reel      = euros_par_uc * 100 / deflateur_indice
  )

annee_reference <- max(compo_annuel$annee, na.rm = TRUE)

# ==============================================================================
# 5. Exports (CSV + RDS)
# ==============================================================================

write_csv(pa_trimestriel, file.path(dir_data, "pouvoir_achat_trimestriel.csv"))
write_csv(compo_annuel, file.path(dir_data, "pouvoir_achat_annuel.csv"))
saveRDS(pa_trimestriel, file.path(dir_graphiques, "pa_trimestriel.rds"))
saveRDS(compo_annuel, file.path(dir_graphiques, "pa_annuel.rds"))

# ==============================================================================
# 6. Graphiques
# ==============================================================================

labels_postes <- c(
  contrib_salaires    = "Salaires bruts",
  contrib_ebe         = "EBE et revenu mixte (indépendants)",
  contrib_propriete   = "Revenus de la propriété",
  contrib_prestations = "Prestations sociales",
  contrib_autres      = "Autres ressources nettes",
  contrib_impots      = "Impôts sur le revenu et le patrimoine",
  contrib_cotisations = "Cotisations sociales",
  contrib_prix        = "Effet prix (déflateur)"
)

# --- Graphique 1 : décomposition annuelle de l'évolution du pouvoir d'achat --
annee_debut_g1 <- max(min(compo_annuel$annee, na.rm = TRUE), annee_reference - 24)

g1_data <- compo_annuel |>
  filter(annee >= annee_debut_g1, annee <= annee_reference) |>
  select(annee, all_of(names(labels_postes)), g_pa_total) |>
  pivot_longer(all_of(names(labels_postes)), names_to = "poste", values_to = "contribution") |>
  mutate(
    poste = factor(labels_postes[poste], levels = unname(labels_postes)),
    tooltip = paste0(poste, "\n", annee, " : ", sprintf("%+.2f", contribution), " pt"),
    data_id = paste0(poste, "_", annee)
  )

g1 <- g1_data |>
  ggplot(aes(x = annee, y = contribution, fill = poste)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                        position = "stack", width = 0.7) +
  geom_line(
    data = compo_annuel |> filter(annee >= annee_debut_g1, annee <= annee_reference),
    aes(x = annee, y = g_pa_total), inherit.aes = FALSE, linewidth = 1.1, color = "black"
  ) +
  geom_point_interactive(
    data = compo_annuel |> filter(annee >= annee_debut_g1, annee <= annee_reference) |>
      mutate(tooltip = paste0("Pouvoir d'achat du RDB\n", annee, " : ",
                               sprintf("%+.2f", g_pa_total), " %"),
             data_id = paste0("pa_total_", annee)),
    aes(x = annee, y = g_pa_total, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2.2, color = "black"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(
    x = NULL, y = "Points de %", fill = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux trimestriels, base 2020, calculs OFCE. ",
      "La ligne noire est le taux de croissance officiel du pouvoir d'achat du RDB ; ",
      "l'écart avec la somme des contributions est le résidu de linéarisation (< 0,1 pt)."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank())

# --- Graphique 2 : RDB en euros par ménage et par UC (courants et constants) -
g2_data <- compo_annuel |>
  filter(annee >= 1960, annee <= annee_reference) |>
  select(annee, euros_par_menage, euros_par_uc, euros_par_menage_reel, euros_par_uc_reel) |>
  pivot_longer(-annee, names_to = "serie", values_to = "euros") |>
  mutate(
    unite = if_else(str_detect(serie, "menage"), "Par ménage", "Par unité de consommation"),
    prix  = if_else(str_detect(serie, "_reel"), paste0("Euros constants ", annee_reference), "Euros courants"),
    tooltip = paste0(unite, " — ", prix, "\n", annee, " : ", scales::comma(round(euros), big.mark = " "), " €"),
    data_id = paste0(serie, "_", annee)
  )

g2 <- g2_data |>
  ggplot(aes(x = annee, y = euros, color = prix, linetype = prix)) +
  geom_line(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 1.2, alpha = 0.6) +
  facet_wrap(~unite, scales = "free_y") +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €")) +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux, base 2020, calculs OFCE. ",
      "“Par unité de consommation” repose sur une ESTIMATION du nombre d'UC ",
      "(structure des ménages, recensement 2016 ; cf. méthodologie) ; ",
      "“Par ménage” repose sur le nombre de ménages ancré sur les recensements."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank())

# --- Graphique 3 : décomposition trimestrielle récente (8 derniers trimestres)
derniers_trim <- pa_trimestriel |> slice_max(order_by = annee * 10 + trim, n = 8) |> pull(trimestre)

g3_data <- pa_trimestriel |>
  filter(trimestre %in% derniers_trim) |>
  select(trimestre, contrib_salaires, contrib_ebe, contrib_propriete,
         contrib_prestations, contrib_autres, contrib_impots, contrib_cotisations,
         contrib_prix, pouvoir_achat_reconstitue) |>
  pivot_longer(all_of(names(labels_postes)), names_to = "poste", values_to = "contribution") |>
  mutate(
    poste = factor(labels_postes[poste], levels = unname(labels_postes)),
    trimestre = factor(trimestre, levels = derniers_trim),
    tooltip = paste0(poste, "\n", trimestre, " : ", sprintf("%+.2f", contribution), " pt"),
    data_id = paste0(poste, "_", trimestre)
  )

g3 <- g3_data |>
  ggplot(aes(x = trimestre, y = contribution, fill = poste)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                        position = "stack", width = 0.7) +
  geom_point_interactive(
    data = pa_trimestriel |> filter(trimestre %in% derniers_trim) |>
      mutate(trimestre = factor(trimestre, levels = derniers_trim),
             tooltip = paste0("Pouvoir d'achat du RDB\n", trimestre, " : ",
                               sprintf("%+.2f", pouvoir_achat_reconstitue), " %"),
             data_id = paste0("pa_total_", trimestre)),
    aes(x = trimestre, y = pouvoir_achat_reconstitue, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2.2, color = "black"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(
    x = NULL, y = "Points de %, variation t/t-1", fill = NULL,
    caption = "Source : Insee, comptes nationaux trimestriels, base 2020, calculs OFCE."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# --- Sauvegarde --------------------------------------------------------------
graphiques_pa <- list(
  pa1_decomposition_annuelle = g1,
  pa2_euros_menage_uc        = g2,
  pa3_decomposition_trim     = g3
)

walk2(names(graphiques_pa), graphiques_pa, function(nom, g) {
  ggsave(
    filename = file.path(dir_graphiques, paste0(nom, ".png")),
    plot = g, width = 12, height = 7, dpi = 300, bg = "white"
  )
  saveRDS(g, file.path(dir_graphiques, paste0(nom, ".rds")))
})

message("Terminé. Tables : ", file.path(dir_data, "pouvoir_achat_annuel.csv"),
        " et ", file.path(dir_data, "pouvoir_achat_trimestriel.csv"))
