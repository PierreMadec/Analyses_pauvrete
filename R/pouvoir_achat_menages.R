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

# NB : on ne garde que les années complètes (4 trimestres). L'année en cours
# (ex. 2025 avec seulement T1-T3 publiés) serait sinon sommée sur 3 trimestres
# et comparée à une année pleine précédente, ce qui produit des contributions
# et des euros/ménage-UC artificiellement énormes pour ce dernier point.
niveaux_annuel <- niveaux_trim |>
  group_by(annee) |>
  summarise(across(ebe_ei:rdb_ajuste, sum), n_trimestres = n(), .groups = "drop") |>
  filter(n_trimestres == 4) |>
  select(-n_trimestres) |>
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

# Postes de la décomposition (nominaux + effet prix), dans l'ordre d'affichage.
noms_contrib <- c("contrib_salaires", "contrib_ebe", "contrib_propriete",
                   "contrib_prestations", "contrib_autres", "contrib_impots",
                   "contrib_cotisations", "contrib_prix")

# Conversion des contributions (points de %) en euros par UC : on applique
# chaque contribution au niveau de RDB/UC de l'année précédente
# (contribution_euros(t) = contribution_pp(t)/100 * euros_par_uc(t-1)). La
# somme des postes nominaux + effet prix approxime ainsi la variation réelle
# du RDB par UC en euros (même résidu de linéarisation que la version en
# points de %, cf. graphique 1).
compo_annuel <- compo_annuel |>
  mutate(
    across(all_of(noms_contrib), ~ .x / 100 * lag(euros_par_uc), .names = "eurUC_{.col}"),
    variation_reelle_euros_uc = pouvoir_achat_reconstitue / 100 * lag(euros_par_uc)
  )

annee_reference  <- max(compo_annuel$annee, na.rm = TRUE)   # dernière année complète (4 trimestres)
annee_base_prix  <- max(pa_annuel$annee, na.rm = TRUE)       # année de base du déflateur (indice = 100), peut être postérieure

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
    prix  = if_else(str_detect(serie, "_reel"), paste0("Euros constants ", annee_base_prix), "Euros courants"),
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

# --- Graphique 4 : décomposition annuelle en euros par UC -------------------
# Même décomposition que le graphique 1, mais exprimée en euros par UC plutôt
# qu'en points de croissance (cf. section 4 pour le détail du calcul).
g4_data <- compo_annuel |>
  filter(annee >= annee_debut_g1, annee <= annee_reference) |>
  select(annee, starts_with("eurUC_")) |>
  pivot_longer(-annee, names_to = "poste", values_to = "contribution_euros") |>
  mutate(
    poste = str_remove(poste, "^eurUC_"),
    poste = factor(labels_postes[poste], levels = unname(labels_postes)),
    tooltip = paste0(poste, "\n", annee, " : ", sprintf("%+.0f", contribution_euros), " € par UC"),
    data_id = paste0(poste, "_", annee)
  )

g4 <- g4_data |>
  ggplot(aes(x = annee, y = contribution_euros, fill = poste)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                        position = "stack", width = 0.7) +
  geom_line(
    data = compo_annuel |> filter(annee >= annee_debut_g1, annee <= annee_reference),
    aes(x = annee, y = variation_reelle_euros_uc), inherit.aes = FALSE,
    linewidth = 1.1, color = "black"
  ) +
  geom_point_interactive(
    data = compo_annuel |> filter(annee >= annee_debut_g1, annee <= annee_reference) |>
      mutate(tooltip = paste0("Variation réelle du RDB par UC\n", annee, " : ",
                               sprintf("%+.0f", variation_reelle_euros_uc), " €"),
             data_id = paste0("variation_reelle_", annee)),
    aes(x = annee, y = variation_reelle_euros_uc, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2.2, color = "black"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €")) +
  labs(
    x = NULL, y = "€ par UC, variation par rapport à l'année précédente", fill = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux trimestriels, base 2020, calculs OFCE. ",
      "Contribution de chaque poste = contribution en points de % (graphique 1) × RDB par UC de ",
      "l'année précédente. La ligne noire (somme des postes, effet prix inclus) approxime la ",
      "variation réelle du RDB par UC en euros ; nombre d'UC ESTIMé (cf. méthodologie, graphique 2)."
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

# ==============================================================================
# 7. Décomposition trimestrielle en euros par UC, effet prix détaillé par
#    grande fonction de consommation, depuis 2017
# ==============================================================================
#
# Sources supplémentaires :
#   - data/insee_pouvoir_achat/ipc_5divisions_mensuel.csv
#       Indices mensuels 1990-2025 de l'IPC (base 2015, ensemble des ménages,
#       France) pour les 5 grandes fonctions de consommation (Alimentation,
#       Tabac, Produits manufacturés, Énergie, Services), extraits des séries
#       longues Insee (idbanks 001759963/966/967/968/969).
#   - data/insee_pouvoir_achat/ipc_ponderations_2026.csv
#       Pondérations 2026 de ces 5 fonctions dans le panier de l'IPC (Insee,
#       Informations rapides IPC août 2026, n° 218).
#
# ATTENTION : l'IPC (indice des prix à la consommation) n'est pas exactement
# le déflateur de la consommation des comptes nationaux utilisé pour
# "contrib_prix" (concepts proches mais pas identiques : champ, traitement du
# logement, pondérations), et les pondérations utilisées ici sont celles de
# 2026 seules, appliquées à tout l'historique (elles évoluent en réalité
# chaque année). Les 5 contributions IPC sont affichées TELLES QUELLES (poids
# x évolution trimestrielle, sans recalage) : un essai initial de recalage
# proportionnel pour forcer leur somme à égaler "contrib_prix" a été
# abandonné, car il amplifiait démesurément les postes les trimestres où
# l'IPC et le déflateur divergent fortement (ex. 2019T4, 2023T4 : facteur
# > 20). L'écart entre les deux mesures est donc isolé dans un poste dédié
# "Écart IPC / déflateur" plutôt que d'être réparti sur les 5 fonctions :
# seule la répartition *relative* entre fonctions provient de l'IPC, jamais
# amplifiée au-delà de ce que l'IPC lui-même indique.
ipc_mensuel      <- read_csv(file.path(dir_data, "ipc_5divisions_mensuel.csv"), show_col_types = FALSE)
ipc_ponderations <- read_csv(file.path(dir_data, "ipc_ponderations_2026.csv"), show_col_types = FALSE)

ipc_trim <- ipc_mensuel |>
  mutate(trim = ceiling(mois / 3)) |>
  group_by(division, annee, trim) |>
  summarise(indice = mean(indice), .groups = "drop") |>
  arrange(division, annee, trim) |>
  group_by(division) |>
  mutate(g_ipc = 100 * (indice / lag(indice) - 1)) |>
  ungroup() |>
  left_join(ipc_ponderations, by = "division") |>
  # Signe négatif : contrib_prix (déflateur, cf. section 4) mesure l'EFFET SUR LE
  # POUVOIR D'ACHAT (négatif quand les prix montent), alors que g_ipc est le taux
  # d'inflation brut (positif quand les prix montent) — il faut donc l'inverser
  # pour rester sur la même convention de signe que le reste de la décomposition.
  mutate(contrib_brute = -poids_pour_10000 / 10000 * g_ipc)

ipc_trim_total <- ipc_trim |>
  group_by(annee, trim) |>
  summarise(contrib_brute_totale = sum(contrib_brute, na.rm = TRUE), .groups = "drop")

# --- Interpolation trimestrielle du nombre d'UC (seule la série annuelle existe) --
uc_annuel_pour_interp <- pa_annuel |>
  filter(!is.na(nb_uc_milliers)) |>
  transmute(t = annee + 0.5, nb_uc_milliers)  # ancrage mi-année
interp_uc <- approx(uc_annuel_pour_interp$t, uc_annuel_pour_interp$nb_uc_milliers,
                     xout = niveaux_trim$annee + (niveaux_trim$trim - 0.5) / 4, rule = 2)

noms_contrib_nominal <- c("contrib_salaires", "contrib_ebe", "contrib_propriete",
                           "contrib_prestations", "contrib_autres", "contrib_impots",
                           "contrib_cotisations")

pa_trimestriel_uc <- niveaux_trim |>
  select(trimestre, annee, trim, rdb) |>
  mutate(
    nb_uc_milliers_interp = interp_uc$y,
    euros_par_uc = rdb * 1e9 / (nb_uc_milliers_interp * 1e3)
  ) |>
  left_join(pa_trimestriel |>
              select(trimestre, all_of(noms_contrib_nominal), contrib_prix,
                     pouvoir_achat_reconstitue),
            by = "trimestre") |>
  left_join(ipc_trim_total, by = c("annee", "trim")) |>
  mutate(
    # Pas de recalage forcé : les 5 contributions IPC gardent leur valeur brute
    # (déjà au bon signe). L'écart entre l'IPC (somme des 5) et le déflateur des
    # comptes nationaux — deux mesures de prix différentes, qui peuvent diverger
    # nettement un trimestre donné — est isolé dans un poste dédié plutôt que
    # d'amplifier artificiellement les 5 postes IPC pour forcer une somme exacte.
    contrib_residu_prix = contrib_prix - contrib_brute_totale,
    across(all_of(noms_contrib_nominal), ~ .x / 100 * lag(euros_par_uc), .names = "eurUC_{.col}"),
    eurUC_contrib_residu_prix = contrib_residu_prix / 100 * lag(euros_par_uc),
    variation_reelle_euros_uc = pouvoir_achat_reconstitue / 100 * lag(euros_par_uc)
  )

ipc_trim_recale <- ipc_trim |>
  left_join(pa_trimestriel_uc |> select(annee, trim, euros_par_uc),
            by = c("annee", "trim")) |>
  group_by(division) |>
  arrange(annee, trim, .by_group = TRUE) |>
  mutate(
    contrib_euros_uc = contrib_brute / 100 * lag(euros_par_uc)
  ) |>
  ungroup() |>
  mutate(trimestre = paste0(annee, "T", trim))

# --- Étiquettes et couleurs de la décomposition détaillée -------------------
labels_postes_detail <- c(
  contrib_salaires        = "Salaires bruts",
  contrib_ebe             = "EBE et revenu mixte (indépendants)",
  contrib_propriete       = "Revenus de la propriété",
  contrib_prestations     = "Prestations sociales",
  contrib_autres          = "Autres ressources nettes",
  contrib_impots          = "Impôts sur le revenu et le patrimoine",
  contrib_cotisations     = "Cotisations sociales",
  contrib_residu_prix     = "Écart IPC / déflateur",
  alimentation            = "Effet prix : alimentation",
  tabac                   = "Effet prix : tabac",
  produits_manufactures   = "Effet prix : produits manufacturés",
  energie                 = "Effet prix : énergie",
  services                = "Effet prix : services"
)
couleurs_postes_detail <- c(
  "Salaires bruts"                        = "#4E79A7",
  "EBE et revenu mixte (indépendants)"    = "#59A14F",
  "Revenus de la propriété"               = "#B07AA1",
  "Prestations sociales"                  = "#76B7B2",
  "Autres ressources nettes"              = "#9C755F",
  "Impôts sur le revenu et le patrimoine" = "#BAB0AC",
  "Cotisations sociales"                  = "#499894",
  "Écart IPC / déflateur"                 = "#B3B3B3",
  "Effet prix : alimentation"             = "#E15759",
  "Effet prix : tabac"                    = "#EDC948",
  "Effet prix : produits manufacturés"    = "#F28E2B",
  "Effet prix : énergie"                  = "#8B0000",
  "Effet prix : services"                 = "#FF9D9A"
)

annee_debut_g5 <- 2017

g5_nominal <- pa_trimestriel_uc |>
  filter(annee >= annee_debut_g5) |>
  select(trimestre, annee, trim, starts_with("eurUC_")) |>
  pivot_longer(starts_with("eurUC_"), names_to = "poste", values_to = "contribution_euros") |>
  mutate(poste = str_remove(poste, "^eurUC_"))

g5_prix <- ipc_trim_recale |>
  filter(annee >= annee_debut_g5) |>
  select(trimestre, annee, trim, poste = division, contribution_euros = contrib_euros_uc)

g5_data <- bind_rows(g5_nominal, g5_prix) |>
  mutate(
    trimestre = factor(trimestre, levels = unique(trimestre[order(annee, trim)])),
    poste = factor(labels_postes_detail[poste], levels = unname(labels_postes_detail)),
    tooltip = paste0(poste, "\n", trimestre, " : ", sprintf("%+.0f", contribution_euros), " € par UC"),
    data_id = paste0(poste, "_", trimestre)
  )

g5_ligne <- pa_trimestriel_uc |>
  filter(annee >= annee_debut_g5) |>
  mutate(trimestre = factor(trimestre, levels = levels(g5_data$trimestre)))

g5 <- g5_data |>
  ggplot(aes(x = trimestre, y = contribution_euros, fill = poste)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                        position = "stack", width = 0.7) +
  geom_line(
    data = g5_ligne, aes(x = trimestre, y = variation_reelle_euros_uc), inherit.aes = FALSE,
    group = 1, linewidth = 1.1, color = "black"
  ) +
  geom_point_interactive(
    data = g5_ligne |>
      mutate(tooltip = paste0("Variation réelle du RDB par UC\n", trimestre, " : ",
                               sprintf("%+.0f", variation_reelle_euros_uc), " €"),
             data_id = paste0("variation_reelle_", trimestre)),
    aes(x = trimestre, y = variation_reelle_euros_uc, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2, color = "black"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = couleurs_postes_detail) +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €")) +
  labs(
    x = NULL, y = "€ par UC, variation par rapport au trimestre précédent", fill = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux trimestriels et IPC (base 2015), calculs OFCE. ",
      "L'effet prix est décomposé par grande fonction de consommation à partir de l'IPC (pondérations ",
      "2026, non recalées) ; l'écart avec le déflateur des comptes nationaux (mesure de prix différente) ",
      "est isolé dans un poste dédié. Nombre d'UC trimestriel interpolé à partir de l'estimation annuelle."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(nrow = 3))

write_csv(pa_trimestriel_uc, file.path(dir_data, "pouvoir_achat_trimestriel_euros_uc.csv"))
write_csv(ipc_trim_recale, file.path(dir_data, "ipc_contributions_trimestrielles.csv"))

# ==============================================================================
# 8. Niveaux trimestriels depuis 2017 : RDB par ménage/UC, et prix (déflateur
#    vs IPC)
# ==============================================================================

# --- Nombre de ménages trimestriel (même interpolation que le nombre d'UC) --
menages_annuel_pour_interp <- pa_annuel |>
  filter(!is.na(nb_menages_milliers)) |>
  transmute(t = annee + 0.5, nb_menages_milliers)
interp_menages <- approx(menages_annuel_pour_interp$t, menages_annuel_pour_interp$nb_menages_milliers,
                          xout = niveaux_trim$annee + (niveaux_trim$trim - 0.5) / 4, rule = 2)

# --- Déflateur trimestriel chaîné (ancre = dernier trimestre = 100) ---------
pouvachat_trim_ord <- pouvachat_trim |> arrange(annee, trim)
deflateur_trim_indice <- chainer_niveau(
  seq_along(pouvachat_trim_ord$trimestre),  # index séquentiel (chainer_niveau attend une clé triée)
  pouvachat_trim_ord$g_prix, nrow(pouvachat_trim_ord), 100
)
pouvachat_trim_ord$deflateur_trim_indice <- deflateur_trim_indice

niveau_trim <- niveaux_trim |>
  select(trimestre, annee, trim, rdb) |>
  mutate(
    nb_menages_milliers_interp = interp_menages$y,
    nb_uc_milliers_interp      = interp_uc$y,
    euros_par_menage = rdb * 1e9 / (nb_menages_milliers_interp * 1e3),
    euros_par_uc      = rdb * 1e9 / (nb_uc_milliers_interp * 1e3)
  ) |>
  left_join(pouvachat_trim_ord |> select(trimestre, deflateur_trim_indice), by = "trimestre") |>
  mutate(
    euros_par_menage_reel = euros_par_menage * 100 / deflateur_trim_indice,
    euros_par_uc_reel      = euros_par_uc * 100 / deflateur_trim_indice
  )

write_csv(niveau_trim, file.path(dir_data, "pouvoir_achat_niveau_trimestriel.csv"))

annee_debut_niveau <- 2017
niveau_trim_recent <- niveau_trim |> filter(annee >= annee_debut_niveau)
annee_base_prix_trim <- pouvachat_trim_ord$trimestre[nrow(pouvachat_trim_ord)]

g6_data <- niveau_trim_recent |>
  select(trimestre, annee, trim, euros_par_menage, euros_par_uc,
         euros_par_menage_reel, euros_par_uc_reel) |>
  pivot_longer(-c(trimestre, annee, trim), names_to = "serie", values_to = "euros") |>
  mutate(
    unite = if_else(str_detect(serie, "menage"), "Par ménage", "Par unité de consommation"),
    prix  = if_else(str_detect(serie, "_reel"), paste0("Euros constants ", annee_base_prix_trim), "Euros courants"),
    trimestre = factor(trimestre, levels = unique(trimestre[order(annee, trim)])),
    tooltip = paste0(unite, " — ", prix, "\n", trimestre, " : ",
                      scales::comma(round(euros), big.mark = " "), " €"),
    data_id = paste0(serie, "_", trimestre)
  )

g6 <- g6_data |>
  ggplot(aes(x = trimestre, y = euros, color = prix, group = prix)) +
  geom_line(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 1.4, alpha = 0.7) +
  facet_wrap(~unite, scales = "free_y") +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €")) +
  labs(
    x = NULL, y = NULL, color = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux trimestriels, base 2020, calculs OFCE. ",
      "“Par unité de consommation” repose sur une ESTIMATION du nombre d'UC (cf. méthodologie) ; ",
      "“Par ménage” repose sur le nombre de ménages ancré sur les recensements."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

# --- Graphique 7 : déflateur vs IPC, niveau trimestriel depuis 2017 ---------
ipc_ensemble_mensuel <- read_csv(file.path(dir_data, "ipc_ensemble_mensuel.csv"), show_col_types = FALSE)
ipc_ensemble_trim <- ipc_ensemble_mensuel |>
  mutate(trim = ceiling(mois / 3)) |>
  group_by(annee, trim) |>
  summarise(ipc_indice = mean(indice), .groups = "drop") |>
  arrange(annee, trim) |>
  mutate(trimestre = paste0(annee, "T", trim))

g7_data <- niveau_trim_recent |>
  select(trimestre, annee, trim, deflateur_trim_indice) |>
  inner_join(ipc_ensemble_trim |> select(trimestre, ipc_indice), by = "trimestre") |>
  arrange(annee, trim) |>
  mutate(
    deflateur_base100 = 100 * deflateur_trim_indice / first(deflateur_trim_indice),
    ipc_base100        = 100 * ipc_indice / first(ipc_indice),
    trimestre = factor(trimestre, levels = trimestre)
  ) |>
  select(trimestre, "Déflateur de la consommation" = deflateur_base100,
         "IPC ensemble" = ipc_base100) |>
  pivot_longer(-trimestre, names_to = "serie", values_to = "indice") |>
  mutate(
    tooltip = paste0(serie, "\n", trimestre, " : ", sprintf("%.1f", indice)),
    data_id = paste0(serie, "_", trimestre)
  )

g7 <- g7_data |>
  ggplot(aes(x = trimestre, y = indice, color = serie, group = serie)) +
  geom_line(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 1.6) +
  labs(
    x = NULL, y = paste0("Indice, base 100 en ", first(levels(g7_data$trimestre))), color = NULL,
    caption = paste0(
      "Source : Insee, comptes nationaux trimestriels et IPC (base 2015), calculs OFCE. ",
      "Les deux indices sont recalés à 100 sur le premier trimestre affiché pour comparer leurs ",
      "évolutions cumulées ; en niveau absolu ils ne sont pas directement comparables."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

# ==============================================================================
# 9. Niveau trimestriel du RDB par UC décomposé par composante, et des prix
#    par grande fonction, depuis 2017
# ==============================================================================

noms_niveau_composantes <- c(
  "salaires_bruts", "ebe_total", "interets_dividendes_nets",
  "prestations_sociales", "autres_ressources_nettes",
  "impots_revenu_patrimoine", "cotisations_sociales"
)

niveau_composantes_trim <- niveaux_trim |>
  select(trimestre, annee, trim, rdb, all_of(noms_niveau_composantes)) |>
  mutate(nb_uc_milliers_interp = interp_uc$y) |>
  mutate(across(all_of(noms_niveau_composantes),
                ~ .x * 1e9 / (nb_uc_milliers_interp * 1e3), .names = "euc_{.col}")) |>
  mutate(
    # Impôts et cotisations sont des charges : affichées en montants négatifs
    euc_impots_revenu_patrimoine = -euc_impots_revenu_patrimoine,
    euc_cotisations_sociales      = -euc_cotisations_sociales,
    euc_rdb_total = rdb * 1e9 / (nb_uc_milliers_interp * 1e3)
  )

write_csv(niveau_composantes_trim, file.path(dir_data, "pouvoir_achat_niveau_composantes_trimestriel.csv"))

labels_niveau <- c(
  salaires_bruts             = "Salaires bruts",
  ebe_total                  = "EBE et revenu mixte (indépendants)",
  interets_dividendes_nets   = "Revenus de la propriété",
  prestations_sociales       = "Prestations sociales",
  autres_ressources_nettes   = "Autres ressources nettes",
  impots_revenu_patrimoine   = "Impôts sur le revenu et le patrimoine",
  cotisations_sociales       = "Cotisations sociales"
)

g8_data <- niveau_composantes_trim |>
  filter(annee >= annee_debut_niveau) |>
  select(trimestre, annee, trim, starts_with("euc_"), -euc_rdb_total) |>
  pivot_longer(starts_with("euc_"), names_to = "poste", values_to = "euros") |>
  mutate(
    poste = str_remove(poste, "^euc_"),
    poste = factor(labels_niveau[poste], levels = unname(labels_niveau)),
    trimestre = factor(trimestre, levels = unique(trimestre[order(annee, trim)])),
    tooltip = paste0(poste, "\n", trimestre, " : ", scales::comma(round(euros), big.mark = " "), " € par UC"),
    data_id = paste0(poste, "_", trimestre)
  )

g8_total <- niveau_composantes_trim |>
  filter(annee >= annee_debut_niveau) |>
  mutate(trimestre = factor(trimestre, levels = levels(g8_data$trimestre)))

g8 <- g8_data |>
  ggplot(aes(x = trimestre, y = euros, fill = poste)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), position = "stack", width = 0.8) +
  geom_line(data = g8_total, aes(x = trimestre, y = euc_rdb_total), inherit.aes = FALSE,
            group = 1, linewidth = 1.1, color = "black") +
  geom_point_interactive(
    data = g8_total |> mutate(
      tooltip = paste0("RDB total par UC\n", trimestre, " : ",
                        scales::comma(round(euc_rdb_total), big.mark = " "), " €"),
      data_id = paste0("total_", trimestre)),
    aes(x = trimestre, y = euc_rdb_total, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2, color = "black"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = couleurs_postes_detail) +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €")) +
  labs(
    x = NULL, y = "€ par UC (niveau trimestriel)", fill = NULL,
    caption = "Source : Insee, comptes nationaux trimestriels, base 2020, calculs OFCE. Nombre d'UC estimé (cf. méthodologie)."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

# --- Graphique 9 : prix par grande fonction, niveau depuis 2017 ------------
g9_data <- ipc_trim |>
  filter(annee >= annee_debut_niveau) |>
  arrange(division, annee, trim) |>
  group_by(division) |>
  mutate(indice_base100 = 100 * indice / first(indice)) |>
  ungroup() |>
  mutate(
    trimestre = paste0(annee, "T", trim),
    trimestre = factor(trimestre, levels = unique(trimestre[order(annee, trim)])),
    poste = factor(labels_postes_detail[division], levels = unname(labels_postes_detail)),
    tooltip = paste0(poste, "\n", trimestre, " : ", sprintf("%.1f", indice_base100)),
    data_id = paste0(division, "_", trimestre)
  )

g9 <- g9_data |>
  ggplot(aes(x = trimestre, y = indice_base100, color = poste, group = poste)) +
  geom_line(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 1.6) +
  geom_hline(yintercept = 100, linewidth = 0.5, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = couleurs_postes_detail) +
  labs(
    x = NULL, y = paste0("Indice, base 100 en ", first(levels(g9_data$trimestre))), color = NULL,
    caption = "Source : Insee, IPC (base 2015), calculs OFCE."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

# --- Sauvegarde --------------------------------------------------------------
graphiques_pa <- list(
  pa1_decomposition_annuelle       = g1,
  pa2_euros_menage_uc              = g2,
  pa3_decomposition_trim           = g3,
  pa4_decomposition_annuelle_euros = g4,
  pa5_decomposition_trim_euros_uc  = g5,
  pa6_niveau_trim_menage_uc        = g6,
  pa7_niveau_trim_prix             = g7,
  pa8_niveau_trim_composantes_rdb  = g8,
  pa9_niveau_trim_composantes_prix = g9
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
