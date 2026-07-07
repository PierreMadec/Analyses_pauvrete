# ==============================================================================
# simulation_contrefactuelle.R
#
# Extension 5 : Simulations contrefactuelles
#
# Deux scénarios :
#
# SCÉNARIO A — "Et si la structure des ménages travailleurs était restée celle de 2010 ?"
#   Pour chaque année t, calculer le taux de pauvreté laborieuse qui aurait
#   prévalu si la distribution des configurations (typmen × biactivite) avait
#   été celle de 2010-2012.
#   = repondération des travailleurs de chaque année avec les poids de 2010-2012.
#   Mesure l'effet pur de la recomposition de la population active.
#
# SCÉNARIO B — "Et si les familles monoparentales avaient le même taux d'emploi
#   que les couples bi-actifs ?" (simulation structurelle)
#   Pour chaque année, calculer quel serait le taux de pauvreté laborieuse si
#   les familles monoparentales étaient en emploi dans les mêmes proportions
#   que les couples bi-actifs à enfants.
#   = augmentation artificielle du taux d'emploi des familles monoparentales
#     et observation de l'impact.
#
# Figures produites :
#   simulation_reweight.rds    — taux observé vs scénario A (2010-2023)
#   simulation_biact.rds       — gap "si bi-activité" par année
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

caption_base <- paste0(
  "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
  "Champ : personnes de référence du ménage (PR), 18-64 ans."
)

# ==============================================================================
# 1. Base complète — tous PR 18-64 (travailleurs ET non-travailleurs)
# ==============================================================================

base_sim <- data_all |>
  filter(lpr == 1, !is.na(acteu_ind),
         !is.na(age_num), age_num >= 18, age_num <= 64,
         !is.na(wprm), wprm > 0,
         !is.na(typmen), !is.na(biactivite)) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std)) |>
  mutate(
    pauvre   = as.integer(nivviem < seuil_std),
    en_emploi = (acteu_ind == "Emploi"),
    config   = case_when(
      typmen == "Personne seule"             ~ "Seul sans enfant",
      typmen == "Famille monoparentale"      ~ "Parent seul",
      typmen == "Couple sans enfant"   & biactivite == "Bi-actif" ~ "Couple bi-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite == "Bi-actif" ~ "Couple bi-actif av.enf.",
      typmen == "Couple sans enfant"   & biactivite != "Bi-actif" ~ "Couple mono-actif s.enf.",
      typmen == "Couple avec enfant(s)"& biactivite != "Bi-actif" ~ "Couple mono-actif av.enf.",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(config))

# ==============================================================================
# 2. SCÉNARIO A — Repondération par distribution de 2010-2012
# ==============================================================================

annees_ref <- 2010:2012
annees_ref <- annees_ref[annees_ref %in% unique(base_sim$annee)]

# Distribution de référence des configs PARMI LES TRAVAILLEURS en 2010-2012
ref_distrib <- base_sim |>
  filter(annee %in% annees_ref, en_emploi) |>
  group_by(config) |>
  summarise(poids_ref = sum(wprm), .groups = "drop") |>
  mutate(part_ref = poids_ref / sum(poids_ref))

cat("Distribution de référence 2010-2012 :\n")
print(ref_distrib)

# Pour chaque année, calculer le taux observé ET le taux contrefactuel
resultats_simA <- map_dfr(sort(unique(base_sim$annee)), function(an) {

  # Travailleurs de cette année
  d <- base_sim |> filter(annee == an, en_emploi)

  # Distribution actuelle des configs
  dist_an <- d |>
    group_by(config) |>
    summarise(poids_obs = sum(wprm), .groups = "drop") |>
    mutate(part_obs = poids_obs / sum(poids_obs))

  # Jointure avec distribution de référence
  d_join <- d |>
    left_join(dist_an |> select(config, part_obs), by = "config") |>
    left_join(ref_distrib |> select(config, part_ref), by = "config") |>
    # Poids repondéré : on rescale pour que la distribution de configs
    # corresponde à celle de 2010-2012
    mutate(
      ratio_reweight = ifelse(is.na(part_ref) | part_obs == 0, 1,
                              part_ref / part_obs),
      wprm_cf = wprm * ratio_reweight
    )

  if (nrow(d_join) < 50 || all(is.na(d_join$wprm_cf))) return(NULL)

  # Taux observé
  tp_obs <- weighted.mean(d_join$pauvre, d_join$wprm, na.rm = TRUE) * 100

  # Taux contrefactuel (repondéré)
  tp_cf <- weighted.mean(d_join$pauvre, d_join$wprm_cf, na.rm = TRUE) * 100

  # Décomposition par config : combien chaque config contribue à l'écart
  contrib_par_config <- d_join |>
    group_by(config) |>
    summarise(
      TP_k     = weighted.mean(pauvre, wprm, na.rm = TRUE) * 100,
      part_obs = sum(wprm) / sum(d_join$wprm) * 100,
      part_cf  = sum(wprm_cf) / sum(d_join$wprm_cf) * 100,
      .groups  = "drop"
    ) |>
    mutate(
      contrib_obs = TP_k * part_obs / 100,
      contrib_cf  = TP_k * part_cf  / 100,
      contrib_gap = contrib_cf - contrib_obs
    )

  tibble(
    annee  = an,
    tp_obs = tp_obs,
    tp_cf  = tp_cf,
    gap_cf = tp_cf - tp_obs  # positif = on aurait été plus pauvre avec distrib 2010
  )
}) |>
  filter(!is.na(tp_obs))

cat("Scénario A calculé pour", nrow(resultats_simA), "années\n")
print(resultats_simA)

# ==============================================================================
# 3. FIGURE 1 — Taux observé vs contrefactuel (scénario A)
# ==============================================================================

sim_long <- resultats_simA |>
  pivot_longer(cols = c(tp_obs, tp_cf),
               names_to = "scenario", values_to = "taux") |>
  mutate(
    scenario = case_when(
      scenario == "tp_obs" ~ "Taux de pauvreté laborieuse observé",
      scenario == "tp_cf"  ~ "Contrefactuel : si composition 2010-2012"
    ),
    tooltip  = paste0(scenario, "\n", annee, " : ", round(taux, 2), " %"),
    data_id  = paste0(scenario, "_", annee)
  )

# Annotation de l'écart en 2023 (ou dernière année)
an_fin_sim <- max(resultats_simA$annee)
gap_fin    <- resultats_simA$gap_cf[resultats_simA$annee == an_fin_sim]
label_gap  <- sprintf(
  "Écart en %d :\n%+.2f pt\n(%s avec la structure 2010-2012)",
  an_fin_sim, gap_fin,
  ifelse(gap_fin > 0, "plus pauvre", "moins pauvre")
)

g_simA <- ggplot(sim_long,
                 aes(x = annee, y = taux, colour = scenario, linetype = scenario)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  annotate("text",
           x = an_fin_sim - 0.3, y = mean(resultats_simA$tp_obs[
             resultats_simA$annee == an_fin_sim], resultats_simA$tp_cf[
               resultats_simA$annee == an_fin_sim]),
           label = label_gap,
           hjust = 1, size = 3, colour = "grey30") +
  scale_colour_manual(values = c(
    "Taux de pauvreté laborieuse observé"      = "#e31a1c",
    "Contrefactuel : si composition 2010-2012" = "#1f78b4"
  )) +
  scale_linetype_manual(values = c(
    "Taux de pauvreté laborieuse observé"      = "solid",
    "Contrefactuel : si composition 2010-2012" = "dashed"
  )) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "Taux de pauvreté laborieuse (%)",
    colour  = NULL,
    linetype = NULL,
    caption = paste0(
      caption_base, "\n",
      "Contrefactuel : poids des travailleurs repondérés pour que la distribution des configurations ",
      "de ménage (typmen × biactivite) corresponde à celle de 2010-2012. ",
      "Mesure l'effet pur de la recomposition de la population active sur le taux de pauvreté."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_simA, file.path(path_fig, "simulation_reweight.rds"))
cat("simulation_reweight : ok\n")

# ==============================================================================
# 4. SCÉNARIO B — "Et si les familles monoparentales avaient le taux d'emploi
#    des couples bi-actifs sans enfant ?"
#
# Méthode :
#   Pour chaque année, calculer le taux de pauvreté laborieuse si les familles
#   monoparentales étaient embauchées aussi massivement que les couples bi-actifs
#   s'ils étaient dans la population PR 18-64.
#   = on augmente le poids des familles monoparentales EN EMPLOI en leur donnant
#     le même taux d'emploi que les couples bi-actifs sans enfant.
# ==============================================================================

resultats_simB <- map_dfr(sort(unique(base_sim$annee)), function(an) {
  d <- base_sim |> filter(annee == an)

  # Taux d'emploi par config
  te_config <- d |>
    group_by(config) |>
    summarise(
      te = weighted.mean(en_emploi, wprm, na.rm = TRUE),
      N  = sum(wprm),
      .groups = "drop"
    )

  # Cible : taux d'emploi des couples bi-actifs sans enfant (ou sans enfant tous)
  # (proxy "meilleure situation observable")
  te_cible <- te_config$te[te_config$config == "Couple bi-actif s.enf."]
  if (length(te_cible) == 0 || is.na(te_cible)) {
    te_cible <- max(te_config$te, na.rm = TRUE)
  }
  te_cible <- min(te_cible, 0.95)  # plafonner à 95%

  # Pour "Parent seul" : combien d'individus supplémentaires en emploi ?
  parent_seul <- te_config |> filter(config == "Parent seul")
  if (nrow(parent_seul) == 0) return(NULL)

  te_actuel <- parent_seul$te
  N_ps      <- parent_seul$N

  # Nombre de parents seuls à basculer vers l'emploi
  n_emploi_supp <- (te_cible - te_actuel) * N_ps

  if (n_emploi_supp <= 0) {
    # Déjà au-dessus de la cible
    tp_obs <- d |> filter(en_emploi) |>
      summarise(tp = weighted.mean(pauvre, wprm, na.rm=T)*100) |> pull()
    return(tibble(annee = an, tp_obs = tp_obs, tp_cf_B = tp_obs, gap_B = 0,
                  n_emploi_supp = 0, te_ps = te_actuel, te_cible = te_cible))
  }

  # Taux de pauvreté des parents seuls en emploi
  # (ces "nouveaux" travailleurs auront probablement le même taux de pauvreté)
  tp_ps_emploi <- d |>
    filter(config == "Parent seul", en_emploi) |>
    summarise(tp = weighted.mean(pauvre, wprm, na.rm=T)*100) |>
    pull()

  # Travailleurs existants
  travailleurs_obs <- d |> filter(en_emploi) |>
    summarise(tp_obs = weighted.mean(pauvre, wprm, na.rm=T)*100,
              N_obs  = sum(wprm)) |>
    as.list()

  # Travailleurs contrefactuels = actuels + nouveaux parents seuls en emploi
  N_cf    <- travailleurs_obs$N_obs + n_emploi_supp
  tp_cf_B <- (travailleurs_obs$tp_obs * travailleurs_obs$N_obs +
                tp_ps_emploi          * n_emploi_supp) / N_cf

  tibble(
    annee         = an,
    tp_obs        = travailleurs_obs$tp_obs,
    tp_cf_B       = tp_cf_B,
    gap_B         = tp_cf_B - travailleurs_obs$tp_obs,
    n_emploi_supp = n_emploi_supp,
    te_ps         = te_actuel,
    te_cible      = te_cible
  )
}) |>
  filter(!is.na(tp_obs))

cat("\nScénario B (bi-activité) :\n")
print(resultats_simB |> select(annee, tp_obs, tp_cf_B, gap_B, te_ps, te_cible))

# ==============================================================================
# 5. FIGURE 2 — Scénario B : gap "si bi-activité des parents seuls"
# ==============================================================================

g_simB <- resultats_simB |>
  mutate(
    tooltip_obs = paste0("Observé — ", annee, " : ", round(tp_obs, 2), " %"),
    tooltip_cf  = paste0("Contrefactuel — ", annee, " : ", round(tp_cf_B, 2), " %\n",
                         "Si taux d'emploi parents seuls = ", round(te_cible*100, 0), " %\n",
                         "(+", round(n_emploi_supp/1e3, 0), " k parents seuls en emploi)"),
    data_id_obs = paste0("obs_B_", annee),
    data_id_cf  = paste0("cf_B_", annee)
  ) |>
  ggplot(aes(x = annee)) +
  geom_ribbon(aes(ymin = pmin(tp_obs, tp_cf_B), ymax = pmax(tp_obs, tp_cf_B)),
              fill = "#1f78b4", alpha = 0.15) +
  geom_line_interactive(aes(y = tp_obs, colour = "Observé"), linewidth = 1.1) +
  geom_point_interactive(
    aes(y = tp_obs, tooltip = tooltip_obs, data_id = data_id_obs,
        colour = "Observé"), size = 2.5
  ) +
  geom_line_interactive(aes(y = tp_cf_B, colour = "Contrefactuel"),
                        linetype = "dashed", linewidth = 1.1) +
  geom_point_interactive(
    aes(y = tp_cf_B, tooltip = tooltip_cf, data_id = data_id_cf,
        colour = "Contrefactuel"), size = 2.5
  ) +
  scale_colour_manual(values = c("Observé" = "#e31a1c", "Contrefactuel" = "#1f78b4")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y       = "Taux de pauvreté laborieuse (%)",
    colour  = NULL,
    caption = paste0(
      caption_base, "\n",
      "Contrefactuel : si le taux d'emploi des familles monoparentales était égal à celui\n",
      "des couples bi-actifs sans enfant (meilleure configuration observable).\n",
      "Les nouveaux travailleurs se voient attribuer le taux de pauvreté conditionnel ",
      "des parents seuls déjà en emploi."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_simB, file.path(path_fig, "simulation_biact.rds"))
cat("simulation_biact : ok\n")

cat("\n=== simulation_contrefactuelle.R terminé ===\n")
