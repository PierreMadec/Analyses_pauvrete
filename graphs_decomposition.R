# ==============================================================================
# graphs_decomposition.R — Génération des graphiques de décomposition de la pauvreté
#
# Prérequis : decomposition_pauvrete.R doit avoir été exécuté au préalable.
# Les objets suivants doivent être en mémoire :
#   resultats, tableau, g3_data, cout_pauvrete, cout_intensite,
#   detail_par_variable, detail_modalite, taux_occ_apl,
#   taux_activite (colonne acteu_ind), part_actifs_pauvres (colonne acteu_ind),
#   ob_annee_debut, ob_annee_fin,
#   mono_data, mono_taux_activite (colonne acteu_ind),
#   tp_data, tp_typmen, tp_contrat, tp_pcs
# ==============================================================================

if (!exists("resultats")) stop("Lancer decomposition_pauvrete.R avant ce script.")

# Dossier de sortie
dir_graphiques <- "graphiques"
if (!dir.exists(dir_graphiques)) dir.create(dir_graphiques, recursive = TRUE)

# ==============================================================================
# Graphique 1 : Evolution des taux de pauvrete
# ==============================================================================
g1 <- resultats |>
  select(annee, taux_obs, taux_seuil_gele, taux_avant_redist) |>
  pivot_longer(-annee, names_to = "type", values_to = "taux") |>
  mutate(type = factor(type,
    levels = c("taux_avant_redist", "taux_obs", "taux_seuil_gele"),
    labels = c("Avant redistribution", "Apres redistribution (observe)",
               "Apres redistribution (seuil gele IPC)")),
    tooltip = paste0(type, "\n", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(type, "_", annee)) |>
  ggplot(aes(x = annee, y = taux, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  labs(
    x = NULL, y = "Taux de pauvrete (%)", color = NULL,
    caption = "Source : INSEE, ERFS 2005-2023, calculs OFCE."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 2 : Decomposition en barres empilees
# ==============================================================================
g2 <- resultats |>
  filter(annee > 2005) |>
  select(annee, effet_seuil, effet_structure_typmen, effet_structure_acteu,
         effet_redistribution, effet_rev_primaires) |>
  pivot_longer(-annee, names_to = "effet", values_to = "contribution") |>
  mutate(effet = factor(effet,
    levels = c("effet_seuil", "effet_structure_typmen", "effet_structure_acteu",
               "effet_redistribution", "effet_rev_primaires"),
    labels = c("Effet seuil (median)", "Structure : type de menage",
               "Structure : activite (mono/bi-actif)",
               "Effet redistribution", "Effet revenus primaires")),
    tooltip = paste0(effet, "\n", annee, " : ", sprintf("%+.2f", contribution), " pts"),
    data_id = paste0(effet, "_", annee)) |>
  ggplot(aes(x = annee, y = contribution, fill = effet)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = "stack", width = 0.7) +
  geom_line(data = resultats |> filter(annee > 2005),
            aes(x = annee, y = delta_total), inherit.aes = FALSE,
            linewidth = 1.2, color = "black") +
  geom_point_interactive(
    data = resultats |> filter(annee > 2005) |>
      mutate(tooltip = paste0("Variation totale\n", annee, " : ", sprintf("%+.2f", delta_total), " pts"),
             data_id = paste0("total_", annee)),
    aes(x = annee, y = delta_total, tooltip = tooltip, data_id = data_id),
    inherit.aes = FALSE, size = 2.5, color = "black") +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(
    x = NULL, y = "Points de %", fill = NULL,
    caption = "Source : INSEE, ERFS 2005-2023, calculs OFCE.\nLa ligne noire represente la variation totale du taux de pauvrete."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 3 : Evolution RSA / SMIC / Revenu median (base 100)
# ==============================================================================
g3 <- g3_data |>
  mutate(
    tooltip = paste0(serie, "\n", annee, " : ", round(base100, 1),
                     " (", round(euros, 0), " EUR/mois)"),
    data_id = paste0(serie, "_b100_", annee)
  ) |>
  ggplot(aes(x = annee, y = base100, color = serie)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  labs(
    x = NULL, y = "Base 100 = 2005", color = NULL,
    caption = "Source : INSEE, legislation, ERFS 2005-2023, calculs OFCE.\nRSA = RSA socle personne seule (RMI avant 2009). SMIC = SMIC net mensuel 35h."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

g3_euros <- g3_data |>
  mutate(
    tooltip = paste0(serie, "\n", annee, " : ", round(euros, 0), " EUR/mois"),
    data_id = paste0(serie, "_eur_", annee)
  ) |>
  ggplot(aes(x = annee, y = euros, color = serie)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  labs(
    x = NULL, y = "Euros courants par mois", color = NULL,
    caption = "Source : INSEE, legislation, ERFS 2005-2023, calculs OFCE.\nRSA = RSA socle personne seule (RMI avant 2009). SMIC = SMIC net mensuel 35h."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 3bis : Écarts RSA-SMIC et SMIC-Revenu médian (euros constants 2005)
# ==============================================================================
# Recréer le vecteur IPC global (écrasé par reste_a_vivre.R qui redéfinit ipc
# comme un tribble COICOP)
ipc_global <- setNames(params_macro$ipc, as.character(params_macro$annee))

# Préparer les données en dehors du pipeline dplyr
g3_wide <- g3_data |>
  pivot_wider(names_from = "serie", values_from = c("euros", "base100"))

# Extraire les colonnes en vecteurs simples
annees_g3   <- g3_wide$annee
rsa_eur     <- g3_wide$euros_RSA
smic_eur    <- g3_wide$euros_SMIC
median_eur  <- g3_wide[["euros_Revenu median"]]
ipc_vec     <- as.numeric(ipc_global[as.character(annees_g3)])
ipc_base    <- as.numeric(ipc_global["2005"])

# Calculer les écarts en euros constants 2005
g3_ecarts_data <- tibble(
  annee = rep(annees_g3, 2),
  ecart = factor(rep(c("SMIC - RSA", "Revenu médian - SMIC"), each = length(annees_g3)),
                 levels = c("SMIC - RSA", "Revenu médian - SMIC")),
  montant = c(
    (smic_eur - rsa_eur) * ipc_base / ipc_vec,
    (median_eur - smic_eur) * ipc_base / ipc_vec
  )
) |>
  mutate(
    tooltip = paste0(ecart, "\n", annee, " : ", round(montant, 0), " EUR"),
    data_id = paste0(ecart, "_", annee)
  )

g3_ecarts <- ggplot(g3_ecarts_data, aes(x = annee, y = montant, color = ecart)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = NULL, y = "Écart mensuel (euros constants 2005)", color = NULL,
    caption = "Source : INSEE, legislation, ERFS 2005-2023, calculs OFCE.\nÉcarts mensuels en euros constants 2005. IPC base 100=2015."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 4 : Cout budgetaire de l'eradication de la pauvrete
# ==============================================================================
g_cout <- cout_pauvrete |>
  select(annee, `50% median (euros courants)` = cout_50_courant,
         `60% median (euros courants)` = cout_60_courant,
         `50% median (euros 2023)` = cout_50_2023,
         `60% median (euros 2023)` = cout_60_2023) |>
  pivot_longer(-annee, names_to = "serie", values_to = "cout") |>
  mutate(
    seuil = if_else(str_detect(serie, "50%"), "50%", "60%"),
    euros = if_else(str_detect(serie, "courants"), "Euros courants", "Euros 2023"),
    tooltip = paste0("Seuil ", seuil, " (", euros, ")\n",
                     annee, " : ", round(cout, 1), " Md EUR"),
    data_id = paste0(serie, "_", annee)
  ) |>
  ggplot(aes(x = annee, y = cout, color = seuil, linetype = euros)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  labs(
    x = NULL, y = "Milliards d'euros", color = "Seuil", linetype = NULL,
    caption = "Source : INSEE, ERFS 2005-2023, calculs OFCE.\nLe cout correspond a la somme des ecarts entre le seuil de pauvrete et le niveau de vie\ndes menages situes sous ce seuil (poverty gap)."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 5 : Cout de la reduction de l'intensite de la pauvrete d'un point
# ==============================================================================
g_cout_intensite <- cout_intensite |>
  select(annee, `50% median (euros courants)` = cout_1pt_50_courant,
         `60% median (euros courants)` = cout_1pt_60_courant,
         `50% median (euros 2023)` = cout_1pt_50_2023,
         `60% median (euros 2023)` = cout_1pt_60_2023) |>
  pivot_longer(-annee, names_to = "serie", values_to = "cout") |>
  mutate(
    seuil = if_else(str_detect(serie, "50%"), "50%", "60%"),
    euros = if_else(str_detect(serie, "courants"), "Euros courants", "Euros 2023"),
    tooltip = paste0("Seuil ", seuil, " (", euros, ")\n",
                     annee, " : ", round(cout, 2), " Md EUR"),
    data_id = paste0(serie, "_", annee)
  ) |>
  ggplot(aes(x = annee, y = cout, color = seuil, linetype = euros)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  labs(
    x = NULL, y = "Milliards d'euros", color = "Seuil", linetype = NULL,
    caption = "Source : INSEE, ERFS 2005-2023, calculs OFCE.\nL'intensite de la pauvrete mesure l'ecart relatif moyen entre le niveau de vie des pauvres et le seuil.\nReduire l'intensite d'1 pt = transferer 1% du seuil (en niv. de vie par UC) a chaque menage pauvre."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphiques 6-8 : Decomposition Oaxaca-Blinder
# ==============================================================================

# --- Graphique 6 : Oaxaca par variable ---
g_oaxaca_var <- detail_par_variable |>
  pivot_longer(c(composition, coefficients), names_to = "effet", values_to = "contribution") |>
  mutate(
    contribution = contribution * 100,
    variable = factor(variable,
      levels = c("Age", "Sexe", "Immigration", "Diplome",
                 "Type de menage", "Activite individuelle", "Activite CJ")),
    effet = factor(effet,
      levels = c("composition", "coefficients"),
      labels = c("Composition (structure)", "Coefficients (rendement)"))
  ) |>
  mutate(
    tooltip = paste0(variable, "\n", effet, " : ", sprintf("%+.2f", contribution), " pts"),
    data_id = paste0(variable, "_", effet)
  ) |>
  ggplot(aes(x = variable, y = contribution, fill = effet)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(
    x = NULL, y = "Points de %", fill = NULL,
    caption = sprintf("Source : INSEE, ERFS %d et %d, calculs OFCE.\nNiveau individuel.",
                      ob_annee_debut, ob_annee_fin)
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 20, hjust = 1))

# Echelle commune pour les graphiques par modalite
echelle_max <- max(
  abs(detail_modalite$composition * 100),
  abs(detail_modalite$coefficients * 100),
  na.rm = TRUE
) * 1.1
limites_ob <- c(-echelle_max, echelle_max)

# --- Graphique 7 : Detail par modalite (effet composition) ---
g_oaxaca_detail <- detail_modalite |>
  mutate(
    label = paste0(variable, ": ", modalite),
    composition_pct = composition * 100
  ) |>
  slice_max(abs(composition_pct), n = 20) |>
  mutate(
    label = fct_reorder(label, composition_pct),
    tooltip = paste0(label, "\nComposition : ", sprintf("%+.2f", composition_pct), " pts"),
    data_id = paste0("comp_", label)
  ) |>
  ggplot(aes(x = label, y = composition_pct, fill = variable)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(limits = limites_ob) +
  labs(
    x = NULL, y = "Points de %", fill = "Variable",
    caption = sprintf("Source : INSEE, ERFS %d et %d, calculs OFCE.\nContribution = delta proportion x effet marginal moyen.",
                      ob_annee_debut, ob_annee_fin)
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# --- Graphique 8 : Detail par modalite (effet coefficients) ---
g_oaxaca_coef <- detail_modalite |>
  mutate(
    label = paste0(variable, ": ", modalite),
    coefficients_pct = coefficients * 100
  ) |>
  slice_max(abs(coefficients_pct), n = 20) |>
  mutate(
    label = fct_reorder(label, coefficients_pct),
    tooltip = paste0(label, "\nCoefficients : ", sprintf("%+.2f", coefficients_pct), " pts"),
    data_id = paste0("coef_", label)
  ) |>
  ggplot(aes(x = label, y = coefficients_pct, fill = variable)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(limits = limites_ob) +
  labs(
    x = NULL, y = "Points de %", fill = "Variable",
    caption = sprintf("Source : INSEE, ERFS %d et %d, calculs OFCE.\nContribution = proportion x delta coefficient x effet marginal moyen.",
                      ob_annee_debut, ob_annee_fin)
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 9 : Taux de pauvrete proprietaires / locataires avec et hors APL
# ==============================================================================
g_apl <- ggplot(taux_occ_apl, aes(x = annee, y = taux, color = label,
                                    linetype = mesure_apl)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  scale_color_manual(values = c(
    "Proprietaire - avec APL" = "#2c7bb6",
    "Proprietaire - hors APL" = "#89c4e8",
    "Locataire - avec APL"    = "#d7191c",
    "Locataire - hors APL"    = "#f5967a"
  )) +
  scale_linetype_manual(values = c("taux_avec_apl" = "solid", "taux_hors_apl" = "dashed"),
                        guide = "none") +
  labs(
    x = NULL, y = "Taux de pauvrete (%)", color = NULL,
    caption = "Source : INSEE, ERFS 2005-2023, calculs OFCE.\nSeuils a 60 % de la mediane de l'ensemble de la population.\nHors APL : revenu disponible diminue des prestations logement."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 11 : Pauvrete par statut d'activite
# ==============================================================================
g_activite <- ggplot(taux_activite, aes(x = annee, y = taux, color = acteu_ind)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  labs(
    x = NULL, y = "Taux de pauvrete (%)", color = "Statut d'activite (individuel)",
    caption = "Source : INSEE, ERFS 2010-2023, calculs OFCE.\nSeuil commun a 60 % de la mediane de l'ensemble de la population.\nStatut d'activite individuel (acteu)."
  ) +
  scale_color_manual(values = c("Emploi" = "#4daf4a", "Chomage" = "#d7191c", "Inactif" = "#ff7f00")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 12 : Composition des menages pauvres par statut d'activite
# ==============================================================================
g_compo_pauvres <- ggplot(part_actifs_pauvres,
                           aes(x = annee, y = part, color = acteu_ind)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  labs(
    x = NULL, y = "Part parmi les individus pauvres (%)", color = NULL,
    caption = "Source : INSEE, ERFS 2010-2023, calculs OFCE.\nStatut d'activite individuel (acteu).\nSeuil commun a 60 % de la mediane de l'ensemble de la population."
  ) +
  scale_color_manual(values = c("Emploi" = "#4daf4a", "Chomage" = "#d7191c", "Inactif" = "#ff7f00")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Graphique 13 : Decomposition du niveau de vie des familles monoparentales
# Design : barres empilees (revenu primaire gris + 4 prestations colorees = niv. vie total)
#          + ligne pointillee seuil de pauvrete
# Meme design que g14 pour coherence visuelle
# ==============================================================================

couleurs_g13 <- c(
  "Revenu primaire"  = "grey75",
  "Prestations"      = "#4dac26",
  "Prelevements"     = "#525252"
)

g13_stacked <- mono_data |>
  select(annee, seuil_mensuel,
         revenu_primaire = nivvie_avant_moy,
         prestations = total_prest_moy,
         prelevements = total_impots_moy) |>
  pivot_longer(cols = -c(annee, seuil_mensuel),
               names_to = "poste", values_to = "montant") |>
  mutate(
    # Prélèvements en négatif pour créer l'effet "barre trou"
    montant = if_else(poste == "prelevements", -montant, montant),
    poste = factor(poste,
      levels = c("prestations", "prelevements", "revenu_primaire"),
      labels = c("Prestations", "Prelevements", "Revenu primaire")),
    tooltip = paste0(poste, "\n", annee, " : ", round(abs(montant)), " \u20ac/UC/mois"),
    data_id = paste0(poste, "_mono_", annee)
  )

g13_seuil <- mono_data |>
  mutate(
    ecart = seuil_mensuel - nivvie_apres,
    ecart_pct = 100 * ecart / seuil_mensuel,
    tooltip = paste0("Seuil de pauvrete (60 % mediane)\n",
                     annee, " : ", round(seuil_mensuel), " \u20ac/UC/mois"),
    data_id = paste0("seuil_mono_", annee)
  )

g13 <- ggplot() +
  geom_col_interactive(
    data = g13_stacked,
    aes(x = annee, y = montant, fill = poste, tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.6, alpha = 0.9
  ) +
  # Ligne du niveau de vie réel (après redistribution)
  geom_line(
    data = g13_seuil,
    aes(x = annee, y = nivvie_apres,
        group = 1, linetype = "Niveau de vie moyen (apres prelevements)"),
    color = "#d7191c", linewidth = 1
  ) +
  geom_point_interactive(data = g13_seuil,
    aes(x = annee, y = nivvie_apres,
        tooltip = paste0("Niveau de vie moyen\n", annee, " : ", round(nivvie_apres), " €/UC/mois"),
        data_id = paste0("niv_mono_", annee)),
    color = "#d7191c", size = 1.5) +
  # Segments montrant l'écart au seuil (flèche bidirectionnelle)
  geom_segment(
    data = g13_seuil,
    aes(x = annee + 0.31, xend = annee + 0.31,
        y = nivvie_apres, yend = seuil_mensuel),
    color = "#d7191c", linewidth = 0.7,
    arrow = arrow(length = unit(0.12, "cm"), ends = "both", type = "closed")
  ) +
  # Étiquettes montrant la valeur de l'écart en %
  geom_text(
    data = g13_seuil,
    aes(x = annee + 0.31, y = (nivvie_apres + seuil_mensuel) / 2,
        label = paste0("-", round(ecart_pct, 0), " %")),
    size = 2.3, color = "#d7191c", fontface = "bold",
    hjust = -0.15
  ) +
  geom_line(
    data = g13_seuil,
    aes(x = annee, y = seuil_mensuel,
        group = 1, linetype = "Seuil de pauvrete (60 % mediane)"),
    color = "black", linewidth = 1.3
  ) +
  geom_point_interactive(
    data = g13_seuil,
    aes(x = annee, y = seuil_mensuel, tooltip = tooltip, data_id = data_id),
    color = "black", size = 2.5
  ) +
  scale_fill_manual(values = couleurs_g13, name = NULL) +
  scale_linetype_manual(values = c(
    "Seuil de pauvrete (60 % mediane)" = "dashed",
    "Niveau de vie moyen (apres prelevements)" = "solid"
  ), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  labs(
    x = NULL,
    y = "Euros / UC / mois",
    caption = paste0(
      "Source : INSEE, ERFS 2005-2023, calculs OFCE. Familles monoparentales pauvres\n",
      "(niveau de vie < 60 % mediane). Les barres decomposent le niveau de vie\n",
      "(revenu primaire + prestations - prelevements = niveau de vie).\n",
      "Les fleches indiquent l'écart au seuil en % du seuil. Moyennes ponderees par wprm."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(nrow = 1, order = 1),
    linetype = guide_legend(order = 2)
  )

# ==============================================================================
# Graphique 14 : Decomposition du niveau de vie des travailleurs pauvres
# Design : barres empilees (revenu primaire + prestations = niveau de vie total)
#          + ligne pointillee seuil de pauvrete
# Les barres atteignent le niveau de vie total => tout sur la meme echelle
# ==============================================================================

couleurs_g14 <- c(
  "Revenu primaire"  = "grey75",
  "Prestations"      = "#4dac26",
  "Prelevements"     = "#525252"
)

# Barres empilees : revenu primaire + prestations - prélèvements = niveau de vie
g14_stacked <- tp_data |>
  select(annee, seuil_mensuel,
         revenu_primaire = nivvie_avant_moy,
         prestations = total_prest_moy,
         prelevements = total_impots_moy) |>
  pivot_longer(cols = -c(annee, seuil_mensuel),
               names_to = "poste", values_to = "montant") |>
  mutate(
    # Prélèvements en négatif pour créer l'effet "barre trou"
    montant = if_else(poste == "prelevements", -montant, montant),
    poste = factor(poste,
      levels = c("prestations", "prelevements", "revenu_primaire"),
      labels = c("Prestations", "Prelevements", "Revenu primaire")),
    tooltip = paste0(poste, "\n", annee, " : ", round(abs(montant)), " \u20ac/UC/mois"),
    data_id = paste0(poste, "_tp_", annee)
  )

# Ligne seuil + niveau de vie réel (après prélèvements)
g14_seuil <- tp_data |>
  mutate(
    ecart = seuil_mensuel - nivvie_apres,
    ecart_pct = 100 * ecart / seuil_mensuel,
    tooltip = paste0("Seuil de pauvrete (60 % mediane)\n",
                     annee, " : ", round(seuil_mensuel), " \u20ac/UC/mois"),
    tooltip_nv = paste0("Niveau de vie moyen\n",
                        annee, " : ", round(nivvie_apres), " \u20ac/UC/mois"),
    data_id = paste0("seuil_tp_", annee)
  )

g14 <- ggplot() +
  geom_col_interactive(
    data = g14_stacked,
    aes(x = annee, y = montant, fill = poste, tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.6, alpha = 0.9
  ) +
  # Ligne du niveau de vie réel (après redistribution)
  geom_line(
    data = g14_seuil,
    aes(x = annee, y = nivvie_apres,
        group = 1, linetype = "Niveau de vie moyen (apres prelevements)"),
    color = "#d7191c", linewidth = 1
  ) +
  geom_point_interactive(data = g14_seuil,
    aes(x = annee, y = nivvie_apres,
        tooltip = paste0("Niveau de vie moyen\n", annee, " : ", round(nivvie_apres), " €/UC/mois"),
        data_id = paste0("niv_tp_", annee)),
    color = "#d7191c", size = 1.5) +
  # Segments montrant l'écart au seuil (flèche bidirectionnelle)
  geom_segment(
    data = g14_seuil,
    aes(x = annee + 0.31, xend = annee + 0.31,
        y = nivvie_apres, yend = seuil_mensuel),
    color = "#d7191c", linewidth = 0.7,
    arrow = arrow(length = unit(0.12, "cm"), ends = "both", type = "closed")
  ) +
  # Étiquettes montrant la valeur de l'écart en %
  geom_text(
    data = g14_seuil,
    aes(x = annee + 0.31, y = (nivvie_apres + seuil_mensuel) / 2,
        label = paste0("-", round(ecart_pct, 0), " %")),
    size = 2.3, color = "#d7191c", fontface = "bold",
    hjust = -0.15
  ) +
  geom_line(
    data = g14_seuil,
    aes(x = annee, y = seuil_mensuel,
        group = 1, linetype = "Seuil de pauvrete (60 % mediane)"),
    color = "black", linewidth = 1.3
  ) +
  geom_point_interactive(
    data = g14_seuil,
    aes(x = annee, y = seuil_mensuel, tooltip = tooltip, data_id = data_id),
    color = "black", size = 2.5
  ) +
  scale_fill_manual(values = couleurs_g14, name = NULL) +
  scale_linetype_manual(values = c(
    "Seuil de pauvrete (60 % mediane)" = "dashed",
    "Niveau de vie moyen (apres prelevements)" = "solid"
  ), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) +
  labs(
    x = NULL,
    y = "Euros / UC / mois",
    caption = paste0(
      "Source : INSEE, ERFS 2010-2023, calculs OFCE. Travailleurs pauvres : PR en emploi,\n",
      "niveau de vie < 60 % mediane. Les barres decomposent le niveau de vie\n",
      "(revenu primaire + prestations - prelevements = niveau de vie).\n",
      "Les fleches indiquent l'écart au seuil en % du seuil. Moyennes ponderees par wprm."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(nrow = 1, order = 1),
    linetype = guide_legend(order = 2)
  )

# ==============================================================================
# Graphique 15 : Composition des travailleurs pauvres par type de menage
# ==============================================================================

g15 <- ggplot(
  tp_typmen |>
    mutate(
      groupe = factor(groupe,
        levels = c("Travailleurs non pauvres", "Travailleurs pauvres")),
      annee_f = factor(annee)
    ),
  aes(x = annee_f, y = part, fill = typmen)
) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.7
  ) +
  facet_wrap(~groupe, ncol = 2) +
  labs(
    x = NULL, y = "Part (%)", fill = NULL,
    caption = paste0(
      "Source : INSEE, ERFS (base individus) 2015-2023, calculs OFCE.\n",
      "Menages dont la PR est en emploi. Seuil de pauvrete : 60 % de la mediane."
    )
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12))

# ==============================================================================
# Graphique 16 : Type de contrat — travailleurs pauvres vs ensemble actifs
# ==============================================================================

if (!is.null(tp_contrat)) {
  g16_data <- tp_contrat |>
    mutate(
      groupe = factor(groupe,
        levels = c("Travailleurs non pauvres", "Travailleurs pauvres")),
      annee_f = factor(annee)
    )

  g16 <- ggplot(g16_data,
                aes(x = annee_f, y = part, fill = type_contrat)) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.7
    ) +
    facet_wrap(~groupe, ncol = 2) +
    labs(
      x = NULL, y = "Part (%)", fill = "Type de contrat",
      caption = paste0(
        "Source : INSEE, ERFS (base individus) 2015-2023, calculs OFCE.\n",
        "Personne de reference du menage en emploi. Hors contrats non renseignes.\n",
        "Seuil de pauvrete : 60 % de la mediane."
      )
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    scale_fill_manual(
      values = c("CDI" = "#2c7bb6", "CDD" = "#fdae61", "Interim" = "#d7191c")
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 12))
} else {
  g16 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 5,
             label = "Base individus non disponible pour cette periode") +
    theme_void()
}

# ==============================================================================
# Graphique 17 : PCS — travailleurs pauvres vs ensemble actifs
# ==============================================================================

if (!is.null(tp_pcs)) {
  g17 <- ggplot(
    tp_pcs |>
      mutate(
        groupe = factor(groupe,
          levels = c("Travailleurs non pauvres", "Travailleurs pauvres")),
        pcs_cat = factor(pcs_cat, levels = c(
          "Agriculteurs exploitants",
          "Artisans, commercants, chefs d'entreprise",
          "Ouvriers",
          "Employes",
          "Professions intermediaires",
          "Cadres et prof. intellectuelles sup."
        )),
        annee_f = factor(annee)
      ),
    aes(x = annee_f, y = part, fill = pcs_cat)
  ) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = "stack", width = 0.7
    ) +
    facet_wrap(~groupe, ncol = 2) +
    labs(
      x = NULL, y = "Part (%)", fill = "Categorie socioprofessionnelle",
      caption = paste0(
        "Source : INSEE, ERFS (base individus) 2015-2023, calculs OFCE.\n",
        "Personne de reference du menage en emploi (PCS renseignee).\n",
        "Seuil de pauvrete : 60 % de la mediane."
      )
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 12))
} else {
  g17 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 5,
             label = "Variable PCS non disponible dans les donnees") +
    theme_void()
}

# ==============================================================================
# Graphique 18 : Taux de pauvrete des familles monoparentales par statut d'activite
# ==============================================================================

g18 <- ggplot(mono_taux_activite,
              aes(x = annee, y = taux, color = acteu_ind, group = acteu_ind)) +
  geom_line(
    linewidth = 1.3
  ) +
  geom_point_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    size = 2.5
  ) +
  scale_color_manual(
    values = c("Emploi"  = "#2166ac",
               "Chomage" = "#f4a582",
               "Inactif" = "#d6604d"),
    name = NULL
  ) +
  scale_y_continuous(labels = \(x) paste0(x, " %")) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  labs(
    x = NULL, y = "Taux de pauvrete (%)",
    caption = paste0(
      "Source : INSEE, ERFS 2005-2023, calculs OFCE.\n",
      "Familles monoparentales uniquement. Seuil a 60 % de la mediane."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# Sauvegarde de tous les graphiques (PNG + RDS)
# ==============================================================================
graphiques <- list(
  g1_taux_pauvrete           = g1,
  g2_decomposition           = g2,
  g3_rsa_smic_median         = g3,
  g3_rsa_smic_median_euros   = g3_euros,
  g3_rsa_smic_ecarts         = g3_ecarts,
  g4_cout_eradication        = g_cout,
  g5_cout_intensite          = g_cout_intensite,
  g6_oaxaca_variables        = g_oaxaca_var,
  g7_oaxaca_composition      = g_oaxaca_detail,
  g8_oaxaca_coefficients     = g_oaxaca_coef,
  g9_taux_occ_apl            = g_apl,
  g11_taux_par_activite      = g_activite,
  g12_compo_pauvres_activite = g_compo_pauvres,
  g13_mono_decomp            = g13,
  g14_tp_decomp              = g14,
  g15_tp_typmen              = g15,
  g16_tp_contrat             = g16,
  g17_tp_pcs                 = g17,
  g18_mono_activite          = g18
)

walk2(names(graphiques), graphiques, function(nom, g) {
  ggsave(
    filename = file.path(dir_graphiques, paste0(nom, ".png")),
    plot = g, width = 12, height = 7, dpi = 300, bg = "white"
  )
  saveRDS(g, file.path(dir_graphiques, paste0(nom, ".rds")))
})

cat("Graphiques decomposition sauvegardes dans", normalizePath(dir_graphiques), "\n")
