# ==============================================================================
# graphs_reste_a_vivre.R — Génération des graphiques du reste à vivre
#
# Prérequis : reste_a_vivre.R doit avoir été exécuté au préalable.
# Les objets suivants doivent être en mémoire :
#   synthese, resultats_annuels, coicop_divisions
# ==============================================================================

if (!exists("synthese")) stop("Lancer reste_a_vivre.R avant ce script.")

# Dossier de sortie (unifié, lowercase)
graph_path <- "../figure"
if (!dir.exists(graph_path)) dir.create(graph_path, recursive = TRUE)

# Labels COICOP
coicop_labels <- c(
  "C01" = "Alimentation",
  "C02" = "Alcool, tabac",
  "C03" = "Habillement",
  "C04" = "Logement, énergie",
  "C05" = "Équipement",
  "C06" = "Santé",
  "C07" = "Transports",
  "C08" = "Communications",
  "C09" = "Loisirs, culture",
  "C10" = "Enseignement",
  "C11" = "Restaurants, hôtels",
  "C12" = "Autres biens & services"
)

# ==============================================================================
# p1 : Évolution du reste à vivre moyen
# ==============================================================================
if (nrow(synthese) > 1) {
  p1 <- synthese |>
    mutate(
      tooltip = paste0(annee, "\nRAV moyen : ", format(round(rav_moy), big.mark = " "), " EUR/an"),
      data_id = paste0("rav_", annee)
    ) |>
    ggplot(aes(x = annee, y = rav_moy)) +
    geom_line_interactive(linewidth = 1.2, color = "#2c7bb6") +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id),
                           size = 3, color = "#2c7bb6") +
    labs(x = "Année", y = "Euros par an") +
    scale_x_continuous(breaks = synthese$annee) +
    theme_minimal(base_size = 14)

  ggsave(file.path(graph_path, "rav_evolution.png"), p1, width = 10, height = 6, dpi = 150)
  saveRDS(p1, file.path(graph_path, "rav_evolution.rds"))
  message("  rav_evolution.png sauvegardé")
}

# ==============================================================================
# p2 : Part de la consommation contrainte dans le niveau de vie
# ==============================================================================
if (nrow(synthese) > 1) {
  synthese_pct <- synthese |>
    mutate(
      pct_contrainte = 100 * conso_contr_moy / nivvie_moy,
      pct_reste      = 100 - pct_contrainte,
      tooltip_c = paste0(annee, "\nContrainte : ", round(pct_contrainte, 1), " %"),
      tooltip_r = paste0(annee, "\nReste à vivre : ", round(pct_reste, 1), " %")
    )

  p2 <- ggplot(synthese_pct, aes(x = annee)) +
    geom_area_interactive(aes(y = 100, tooltip = tooltip_c, data_id = paste0("contr_", annee)),
                          fill = "#d7191c", alpha = 0.3) +
    geom_area_interactive(aes(y = pct_reste, tooltip = tooltip_r, data_id = paste0("rav_", annee)),
                          fill = "#2c7bb6", alpha = 0.5) +
    labs(x = "Année", y = "% du niveau de vie") +
    scale_x_continuous(breaks = synthese$annee) +
    theme_minimal(base_size = 14)

  ggsave(file.path(graph_path, "part_contrainte_evolution.png"), p2, width = 10, height = 6, dpi = 150)
  saveRDS(p2, file.path(graph_path, "part_contrainte_evolution.rds"))
  message("  part_contrainte_evolution.png sauvegardé")
}

# ==============================================================================
# p3 : Décomposition par COICOP
# ==============================================================================
if (nrow(synthese) > 1) {
  decomp <- synthese |>
    select(annee, ends_with("_moy")) |>
    select(annee, starts_with("conso_C")) |>
    pivot_longer(-annee, names_to = "poste", values_to = "montant") |>
    mutate(
      coicop = str_extract(poste, "C[0-9]{2}"),
      label  = coicop_labels[coicop],
      tooltip = paste0(label, "\n", annee, " : ", format(round(montant), big.mark = " "), " EUR/an"),
      data_id = paste0(coicop, "_", annee)
    )

  p3 <- ggplot(decomp, aes(x = annee, y = montant, fill = label)) +
    geom_area_interactive(aes(tooltip = tooltip, data_id = data_id), position = "stack") +
    labs(x = "Année", y = "Euros par an (par UC)", fill = "Poste") +
    scale_x_continuous(breaks = synthese$annee) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right")

  ggsave(file.path(graph_path, "decomp_coicop_evolution.png"), p3, width = 12, height = 7, dpi = 150)
  saveRDS(p3, file.path(graph_path, "decomp_coicop_evolution.rds"))
  message("  decomp_coicop_evolution.png sauvegardé")
}

# ==============================================================================
# p4 : Part de la consommation contrainte par quintile
# ==============================================================================
if (length(resultats_annuels) > 0) {
  rav_quintile <- map_dfr(names(resultats_annuels), function(a) {
    resultats_annuels[[a]] |>
      filter(!is.na(reste_a_vivre), !is.na(quintile)) |>
      group_by(quintile) |>
      summarise(
        annee = as.integer(a),
        rav_moy = weighted.mean(reste_a_vivre, wprm),
        nivvie_moy = weighted.mean(nivviem, wprm),
        part_contrainte = 100 * weighted.mean(conso_contrainte, wprm) / weighted.mean(nivviem, wprm),
        .groups = "drop"
      )
  })

  q_labels <- c("Q1 (20% les + pauvres)", "Q2", "Q3", "Q4", "Q5 (20% les + riches)")
  rav_quintile <- rav_quintile |>
    mutate(
      q_label = q_labels[quintile],
      tooltip = paste0(q_label, "\n", annee, " : ", round(part_contrainte, 1), " %"),
      data_id = paste0("Q", quintile, "_", annee)
    )

  p4 <- ggplot(rav_quintile, aes(x = annee, y = part_contrainte,
                                  color = factor(quintile), group = quintile)) +
    geom_line_interactive(linewidth = 1) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
    labs(x = "Année", y = "% du niveau de vie", color = "Quintile") +
    scale_x_continuous(breaks = unique(rav_quintile$annee)) +
    scale_color_brewer(palette = "RdYlBu", direction = -1, labels = q_labels) +
    theme_minimal(base_size = 13)

  ggsave(file.path(graph_path, "part_contrainte_quintile.png"), p4, width = 11, height = 7, dpi = 150)
  saveRDS(p4, file.path(graph_path, "part_contrainte_quintile.rds"))
  message("  part_contrainte_quintile.png sauvegardé")
}

# ==============================================================================
# p5-p8 : Analyses pauvres vs non-pauvres
# ==============================================================================
if (length(resultats_annuels) > 0) {

  grands_postes <- list(
    "Alimentation"      = c("conso_C01", "conso_C02"),
    "Logement, énergie" = c("conso_C04"),
    "Transports"        = c("conso_C07"),
    "Autres"            = c("conso_C03", "conso_C05", "conso_C06", "conso_C08",
                            "conso_C09", "conso_C10", "conso_C11", "conso_C12")
  )

  # Données pauvres vs non-pauvres par année et par grand poste
  rav_pauvrete <- map_dfr(names(resultats_annuels), function(a) {
    df <- resultats_annuels[[a]]
    if (is.null(df)) return(NULL)
    df <- df |> filter(!is.na(reste_a_vivre))

    mediane_nv <- Hmisc::wtd.quantile(df$nivviem, weights = df$wprm, probs = 0.5)
    seuil60 <- 0.6 * mediane_nv
    df$pauvre <- ifelse(df$nivviem < seuil60, "Pauvres", "Non pauvres")

    map_dfr(names(grands_postes), function(poste) {
      cols <- grands_postes[[poste]]
      df$montant_poste <- rowSums(df[, cols, drop = FALSE], na.rm = TRUE)
      df |>
        group_by(pauvre) |>
        summarise(
          annee = as.integer(a),
          poste = poste,
          conso_moy = weighted.mean(montant_poste, wprm, na.rm = TRUE),
          nivvie_moy = weighted.mean(nivviem, wprm),
          part_nivvie = 100 * weighted.mean(montant_poste, wprm, na.rm = TRUE) /
                        weighted.mean(nivviem, wprm),
          .groups = "drop"
        )
    })
  })

  rav_pauvrete$poste <- factor(rav_pauvrete$poste,
    levels = c("Alimentation", "Logement, énergie", "Transports", "Autres"))
  rav_pauvrete <- rav_pauvrete |>
    mutate(
      tooltip = paste0(pauvre, " — ", poste, "\n", annee, " : ", round(part_nivvie, 1), " %"),
      data_id = paste0(pauvre, "_", poste, "_", annee)
    )

  # p5 : Part de chaque poste dans le niveau de vie, pauvres vs non-pauvres
  p5 <- ggplot(rav_pauvrete, aes(x = annee, y = part_nivvie,
                                  color = pauvre, linetype = pauvre)) +
    geom_line_interactive(linewidth = 1) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 1.5) +
    facet_wrap(~ poste, scales = "free_y") +
    labs(x = "Année", y = "% du niveau de vie", color = "", linetype = "") +
    scale_x_continuous(breaks = unique(rav_pauvrete$annee)) +
    scale_color_manual(values = c("Pauvres" = "#d7191c", "Non pauvres" = "#2c7bb6")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(file.path(graph_path, "conso_pauvres_vs_nonpauvres.png"), p5, width = 12, height = 8, dpi = 150)
  saveRDS(p5, file.path(graph_path, "conso_pauvres_vs_nonpauvres.rds"))
  message("  conso_pauvres_vs_nonpauvres.png sauvegardé")

  # Données contrainte totale par statut de pauvreté
  rav_contrainte_pauv <- map_dfr(names(resultats_annuels), function(a) {
    df <- resultats_annuels[[a]]
    if (is.null(df)) return(NULL)
    df <- df |> filter(!is.na(reste_a_vivre))

    mediane_nv <- Hmisc::wtd.quantile(df$nivviem, weights = df$wprm, probs = 0.5)
    seuil60 <- 0.6 * mediane_nv
    df$pauvre <- ifelse(df$nivviem < seuil60, "Pauvres", "Non pauvres")

    df |>
      group_by(pauvre) |>
      summarise(
        annee = as.integer(a),
        nivvie_moy = weighted.mean(nivviem, wprm),
        conso_contr_moy = weighted.mean(conso_contrainte, wprm),
        rav_moy = weighted.mean(reste_a_vivre, wprm),
        part_contrainte = 100 * weighted.mean(conso_contrainte, wprm) /
                          weighted.mean(nivviem, wprm),
        rav_mensuel = weighted.mean(reste_a_vivre, wprm) / 12,
        .groups = "drop"
      )
  }) |>
    mutate(
      tooltip_pc = paste0(pauvre, "\n", annee, " : ", round(part_contrainte, 1), " %"),
      tooltip_rv = paste0(pauvre, "\n", annee, " : ", format(round(rav_mensuel), big.mark = " "), " EUR/mois"),
      data_id_pc = paste0(pauvre, "_pc_", annee),
      data_id_rv = paste0(pauvre, "_rv_", annee)
    )

  # p6 : Part de la consommation contrainte, pauvres vs non-pauvres
  p6 <- ggplot(rav_contrainte_pauv, aes(x = annee, y = part_contrainte, color = pauvre)) +
    geom_line_interactive(linewidth = 1.2) +
    geom_point_interactive(aes(tooltip = tooltip_pc, data_id = data_id_pc), size = 3) +
    labs(x = "Année", y = "% du niveau de vie", color = "") +
    scale_x_continuous(breaks = unique(rav_contrainte_pauv$annee)) +
    scale_color_manual(values = c("Pauvres" = "#d7191c", "Non pauvres" = "#2c7bb6")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

  ggsave(file.path(graph_path, "part_contrainte_pauvres.png"), p6, width = 10, height = 6, dpi = 150)
  saveRDS(p6, file.path(graph_path, "part_contrainte_pauvres.rds"))
  message("  part_contrainte_pauvres.png sauvegardé")

  # p7 : Reste à vivre mensuel, pauvres vs non-pauvres
  p7 <- ggplot(rav_contrainte_pauv, aes(x = annee, y = rav_mensuel, color = pauvre)) +
    geom_line_interactive(linewidth = 1.2) +
    geom_point_interactive(aes(tooltip = tooltip_rv, data_id = data_id_rv), size = 3) +
    labs(x = "Année", y = "Euros par mois", color = "") +
    scale_x_continuous(breaks = unique(rav_contrainte_pauv$annee)) +
    scale_color_manual(values = c("Pauvres" = "#d7191c", "Non pauvres" = "#2c7bb6")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

  ggsave(file.path(graph_path, "rav_mensuel_pauvres.png"), p7, width = 10, height = 6, dpi = 150)
  saveRDS(p7, file.path(graph_path, "rav_mensuel_pauvres.rds"))
  message("  rav_mensuel_pauvres.png sauvegardé")

  # p8 : Décomposition empilée par poste, pauvres vs non-pauvres
  p8 <- ggplot(rav_pauvrete, aes(x = annee, y = part_nivvie, fill = poste)) +
    geom_area_interactive(aes(tooltip = tooltip, data_id = data_id), position = "stack") +
    facet_wrap(~ pauvre) +
    labs(x = "Année", y = "% du niveau de vie", fill = "Poste") +
    scale_x_continuous(breaks = unique(rav_pauvrete$annee)) +
    scale_fill_manual(values = c(
      "Alimentation" = "#e41a1c",
      "Logement, énergie" = "#ff7f00",
      "Transports" = "#377eb8",
      "Autres" = "#4daf4a"
    )) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(file.path(graph_path, "structure_conso_pauvres.png"), p8, width = 12, height = 6, dpi = 150)
  saveRDS(p8, file.path(graph_path, "structure_conso_pauvres.rds"))
  message("  structure_conso_pauvres.png sauvegardé")
}

cat("Graphiques reste à vivre sauvegardés dans", normalizePath(graph_path), "\n")
