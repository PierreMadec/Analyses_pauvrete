# ==============================================================================
# pouvoir_achat_menages.R — RDB par UC : ressources et emplois, depuis 2017
#
# Un graphique annuel : pour chaque année depuis 2017, deux colonnes miroir
# qui totalisent le même montant (le RDB par UC) :
#   - Ressources : comment le revenu se forme (salaires bruts, EBE/revenu
#     mixte des indépendants, revenus de la propriété, prestations sociales,
#     moins cotisations sociales et impôts sur le revenu et le patrimoine) ;
#   - Emplois : à quoi il sert (consommation, répartie par grande fonction —
#     alimentation, tabac, produits manufacturés, énergie, services — et
#     épargne, le résidu).
# Empilées année après année, les deux colonnes donnent une idée de
# l'évolution du pouvoir d'achat et de la marge de manœuvre (épargne) des
# ménages depuis 2017.
#
# Sources INSEE (téléchargées le 2026-09-25, base 2020) :
#   - data/insee_pouvoir_achat/t_men_val.xls
#       Compte trimestriel des ménages (CVS-CJO), 1949T1 à aujourd'hui —
#       Insee, "Revenu, pouvoir d'achat et comptes des ménages", Comptes
#       nationaux trimestriels : https://www.insee.fr/fr/statistiques/8666489
#   - data/insee_pouvoir_achat/reve_pouv_achat_annuel.xlsx
#       Séries annuelles 1960-2025 (évolutions en %), dont le taux d'épargne
#       et la croissance du nombre d'UC : https://www.insee.fr/fr/statistiques/2830244
#   - data/insee_pouvoir_achat/uc_par_type_menage_2016.csv
#       Table intermédiaire documentant l'estimation du nombre moyen d'UC par
#       ménage en 2016 à partir de la structure des ménages (recensement) et
#       de l'échelle d'équivalence OCDE modifiée (1 / 0.5 / 0.3), utilisée
#       pour ancrer le nombre d'UC (non publié en niveau par l'Insee).
#   - data/insee_pouvoir_achat/ipc_ponderations_2026.csv
#       Pondérations 2026 de l'IPC par grande fonction de consommation
#       (Insee, Informations rapides IPC août 2026, n° 218), utilisées comme
#       proxy de la structure de consommation des ménages pour répartir la
#       consommation totale entre les 5 fonctions.
#
# ATTENTION — ce que cette reconstruction peut affirmer et ce qu'elle ne peut pas :
#   - Le RDB, sa décomposition en composantes (ressources) et le taux
#     d'épargne officiel sont des séries INSEE exactes, sans retraitement.
#   - Le nombre d'UC en niveau n'est PAS publié par l'Insee (seul son taux de
#     croissance annuel l'est) : il est ESTIMÉ ici (cf. uc_par_type_menage_2016.csv),
#     donc tous les montants "par UC" sont des ordres de grandeur (± 5-10 %),
#     pas des niveaux officiels.
#   - La répartition de la consommation entre les 5 fonctions utilise les
#     pondérations de l'IPC 2026 (fixes sur toute la période 2017-2024, alors
#     qu'elles évoluent en réalité chaque année) comme proxy de la structure
#     de consommation réelle des ménages. Le total consommation + épargne =
#     RDB est en revanche exact (le taux d'épargne officiel fixe le total
#     consommation, seule sa répartition entre les 5 fonctions est approchée).
# ==============================================================================

library(readxl)
library(tidyverse)
library(ggiraph)

dir_data       <- "data/insee_pouvoir_achat"
dir_graphiques <- "figure"
if (!dir.exists(dir_graphiques)) dir.create(dir_graphiques, recursive = TRUE)

# ==============================================================================
# 1. Compte trimestriel des ménages : niveaux, agrégés en années complètes
# ==============================================================================

noms_postes <- c(
  "trimestre", "ebe_ei", "ebe_hors_ei", "ebe_total", "salaires_bruts",
  "interets_dividendes_nets", "prestations_sociales", "autres_ressources_nettes",
  "total_ressources", "impots_revenu_patrimoine", "cotisations_sociales",
  "total_charges", "rdb", "transferts_nature", "rdb_ajuste"
)

niveaux_trim <- read_excel(file.path(dir_data, "t_men_val.xls"), sheet = "Niveaux", col_names = FALSE) |>
  set_names(noms_postes) |>
  filter(str_detect(trimestre, "^[0-9]{4}T[1-4]$")) |>
  mutate(
    across(-trimestre, as.numeric),
    annee = as.integer(str_sub(trimestre, 1, 4)),
    trim  = as.integer(str_sub(trimestre, 6, 6))
  ) |>
  arrange(annee, trim)

postes_ressources <- c("salaires_bruts", "ebe_total", "interets_dividendes_nets",
                        "prestations_sociales", "autres_ressources_nettes")
postes_charges     <- c("impots_revenu_patrimoine", "cotisations_sociales")

# NB : on ne garde que les années complètes (4 trimestres). L'année en cours
# (ex. 2025 avec seulement T1-T3 publiés) sommée sur 3 trimestres serait sous-
# estimée par rapport aux années pleines.
niveaux_annuel <- niveaux_trim |>
  group_by(annee) |>
  summarise(across(all_of(c(postes_ressources, postes_charges, "rdb")), sum),
            n_trimestres = n(), .groups = "drop") |>
  filter(n_trimestres == 4) |>
  select(-n_trimestres) |>
  arrange(annee)

# ==============================================================================
# 2. Série annuelle officielle : taux d'épargne et croissance du nombre d'UC
# ==============================================================================

raw_annuel <- read_excel(file.path(dir_data, "reve_pouv_achat_annuel.xlsx"),
                          sheet = "Figure 1", col_names = TRUE, skip = 3)
names(raw_annuel)[1] <- "indicateur"

pa_annuel_officiel <- raw_annuel |>
  filter(!is.na(indicateur)) |>
  mutate(cle = case_when(
    str_detect(indicateur, "^Taux d.épargne")    ~ "taux_epargne",
    str_detect(indicateur, "^Nombre d.unit")           ~ "g_nb_uc",
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

# ==============================================================================
# 3. Nombre d'UC en niveau : ancré en 2016 (structure des ménages, cf. csv),
#    chaîné avec le taux de croissance officiel INSEE
# ==============================================================================

chainer_niveau <- function(annees, croissance_pct, annee_ancre, valeur_ancre) {
  stopifnot(!is.unsorted(annees))
  n <- length(annees)
  valeurs <- rep(NA_real_, n)
  i_ancre <- which(annees == annee_ancre)
  valeurs[i_ancre] <- valeur_ancre
  if (i_ancre < n) {
    for (i in (i_ancre + 1):n) valeurs[i] <- valeurs[i - 1] * (1 + croissance_pct[i] / 100)
  }
  if (i_ancre > 1) {
    for (i in (i_ancre - 1):1) valeurs[i] <- valeurs[i + 1] / (1 + croissance_pct[i + 1] / 100)
  }
  valeurs
}

uc_par_type <- read_csv(file.path(dir_data, "uc_par_type_menage_2016.csv"), show_col_types = FALSE) |>
  mutate(
    uc_moyen = part_avec_enfants_mineurs * uc_estime_avec_mineurs +
      (1 - part_avec_enfants_mineurs) * uc_estime_sans_mineurs,
    uc_total_milliers = nb_menages_milliers * uc_moyen
  )
nb_uc_2016_milliers <- sum(uc_par_type$uc_total_milliers)

pa_annuel <- pa_annuel_officiel |>
  mutate(nb_uc_milliers = chainer_niveau(annee, g_nb_uc, 2016, nb_uc_2016_milliers))

# ==============================================================================
# 4. Ressources et emplois du RDB par UC, depuis 2017
# ==============================================================================

ipc_ponderations <- read_csv(file.path(dir_data, "ipc_ponderations_2026.csv"), show_col_types = FALSE)

annee_debut <- 2017
annee_fin   <- max(niveaux_annuel$annee, na.rm = TRUE)

ressources_emplois <- niveaux_annuel |>
  left_join(pa_annuel |> select(annee, taux_epargne, nb_uc_milliers), by = "annee") |>
  filter(annee >= annee_debut, annee <= annee_fin) |>
  mutate(
    across(all_of(c(postes_ressources, postes_charges, "rdb")),
           ~ .x * 1e9 / (nb_uc_milliers * 1e3), .names = "euc_{.col}"),
    conso_totale_euc = euc_rdb * (1 - taux_epargne / 100),
    epargne_euc      = euc_rdb * taux_epargne / 100
  )

for (i in seq_len(nrow(ipc_ponderations))) {
  div <- ipc_ponderations$division[i]
  poids <- ipc_ponderations$poids_pour_10000[i]
  ressources_emplois[[paste0("conso_", div)]] <- ressources_emplois$conso_totale_euc * poids / 10000
}

write_csv(ressources_emplois, file.path(dir_data, "ressources_emplois_annuel_par_uc.csv"))

# ==============================================================================
# 5. Graphique
# ==============================================================================

labels_ressources <- c(
  salaires_bruts           = "Salaires bruts",
  ebe_total                = "EBE et revenu mixte (indépendants)",
  interets_dividendes_nets = "Revenus de la propriété",
  prestations_sociales     = "Prestations sociales",
  autres_ressources_nettes = "Autres ressources nettes",
  impots_revenu_patrimoine = "Impôts sur le revenu et le patrimoine",
  cotisations_sociales     = "Cotisations sociales"
)
labels_emplois <- c(
  conso_alimentation          = "Consommation : alimentation",
  conso_tabac                 = "Consommation : tabac",
  conso_produits_manufactures = "Consommation : produits manufacturés",
  conso_energie                = "Consommation : énergie",
  conso_services                = "Consommation : services",
  epargne_euc                   = "Épargne"
)
couleurs <- c(
  "Salaires bruts"                        = "#4E79A7",
  "EBE et revenu mixte (indépendants)"    = "#59A14F",
  "Revenus de la propriété"               = "#B07AA1",
  "Prestations sociales"                  = "#76B7B2",
  "Autres ressources nettes"              = "#9C755F",
  "Impôts sur le revenu et le patrimoine" = "#BAB0AC",
  "Cotisations sociales"                  = "#499894",
  "Consommation : alimentation"           = "#E15759",
  "Consommation : tabac"                  = "#EDC948",
  "Consommation : produits manufacturés"  = "#F28E2B",
  "Consommation : énergie"                 = "#8B0000",
  "Consommation : services"                = "#FF9D9A",
  "Épargne"                                = "#54507A"
)

# Ressources en valeur absolue (empilement "brut", avant charges) ; côté
# emplois, directement en valeur signée (tout est positif par construction).
g_ressources <- ressources_emplois |>
  select(annee, all_of(paste0("euc_", names(labels_ressources)))) |>
  rename_with(~ str_remove(., "^euc_")) |>
  mutate(across(everything(), ~ if (is.numeric(.x)) abs(.x) else .x)) |>
  pivot_longer(-annee, names_to = "poste", values_to = "euros") |>
  mutate(cote = "Ressources", poste = labels_ressources[poste])

g_emplois <- ressources_emplois |>
  select(annee, all_of(names(labels_emplois))) |>
  pivot_longer(-annee, names_to = "poste", values_to = "euros") |>
  mutate(cote = "Emplois", poste = labels_emplois[poste])

g_data <- bind_rows(g_ressources, g_emplois) |>
  mutate(
    poste = factor(poste, levels = c(unname(labels_ressources), unname(labels_emplois))),
    cote  = factor(cote, levels = c("Ressources", "Emplois")),
    tooltip = paste0(poste, "\n", annee, " (", cote, ") : ",
                      scales::comma(round(euros), big.mark = " "), " € par UC"),
    data_id = paste0(poste, "_", annee, "_", cote)
  )

totaux <- ressources_emplois |>
  transmute(annee, rdb_net = euc_rdb,
            brut = euc_salaires_bruts + euc_ebe_total + euc_interets_dividendes_nets +
              euc_prestations_sociales + euc_autres_ressources_nettes)

n_annees   <- nrow(totaux)
x_annee    <- seq_len(n_annees)
decalage   <- 0.19
totaux <- totaux |> mutate(x_ress = x_annee - decalage, x_empl = x_annee + decalage)
g_data <- g_data |>
  left_join(tibble(annee = totaux$annee, x_annee = x_annee), by = "annee") |>
  mutate(x = x_annee + if_else(cote == "Ressources", -decalage, decalage))

g1 <- ggplot() +
  geom_col_interactive(
    data = g_data, aes(x = x, y = euros, fill = poste, tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.34
  ) +
  geom_segment(
    data = totaux, aes(x = x_ress - 0.17, xend = x_empl + 0.17, y = rdb_net, yend = rdb_net),
    linewidth = 0.7, linetype = "22"
  ) +
  geom_text(
    data = totaux, aes(x = x_empl + 0.19, y = rdb_net, label = paste0("RDB net : ", scales::comma(round(rdb_net), big.mark = " "), " €")),
    hjust = 0, size = 3, fontface = "bold", nudge_y = max(totaux$brut) * 0.01
  ) +
  geom_text(
    data = totaux, aes(x = x_ress, y = brut, label = paste0("brut : ", scales::comma(round(brut), big.mark = " "), " €")),
    vjust = -0.6, size = 2.7, color = "grey40"
  ) +
  scale_x_continuous(breaks = x_annee, labels = totaux$annee,
                      expand = expansion(add = c(0.6, 1.6))) +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " €"),
                      expand = expansion(mult = c(0.02, 0.12))) +
  scale_fill_manual(values = couleurs) +
  labs(
    x = NULL, y = "€ par UC (niveau annuel)", fill = NULL,
    title = "Revenu disponible brut par UC : ressources et emplois, depuis 2017",
    caption = paste0(
      "Source : Insee, comptes nationaux, base 2020, calculs OFCE — euros courants. ",
      "Ressources : 7 postes empilés jusqu'au total brut (avant charges) ; la ligne pointillée marque le RDB net ",
      "(brut moins impôts et cotisations), identique au sommet de la colonne Emplois. Emplois = consommation ",
      "(répartie selon les pondérations IPC 2026, proxy de la structure de consommation) + épargne (résidu, taux ",
      "d'épargne officiel Insee). Nombre d'UC ESTIMé (cf. méthodologie en tête de script)."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, size = 7.5, color = "grey40")
  ) +
  guides(fill = guide_legend(nrow = 4))

ggsave(file.path(dir_graphiques, "pa_ressources_emplois_annuel.png"),
       plot = g1, width = 13, height = 9, dpi = 300, bg = "white")
saveRDS(g1, file.path(dir_graphiques, "pa_ressources_emplois_annuel.rds"))

message("Terminé. Table : ", file.path(dir_data, "ressources_emplois_annuel_par_uc.csv"))
