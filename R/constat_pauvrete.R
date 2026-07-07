# ==============================================================================
# constat_pauvrete.R
#
# Figures pour la Partie 1 "Le constat" :
#   g0_emploi_pauvrete  — double axe : taux d'emploi 18-64 + taux de pauvreté
#
# Prérequis : data_all et seuils_annuels (figure/data_all.rds)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

caption_base <- "Source : INSEE, ERFS 2005-2023, calculs de l'auteur."

# ==============================================================================
# Graphique 1 : Contribution de chaque statut d'activité à l'évolution
# du taux de pauvreté global (décomposition additive, base = première année)
#
# Méthode : contribution_k(t) = taux_k(t) × part_k(t)
#           variation_k(t)    = contribution_k(t) - contribution_k(t0)
# La somme des variations = variation du taux de pauvreté global vs t0.
# ==============================================================================

base_activite <- data_all |>
  filter(!is.na(acteu_ind), !is.na(wprm), wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(seuil_std), !is.na(nivviem)) |>
  mutate(
    # Scinder les inactifs en "âge actif" (< 65 ans) et "retraités" (65+)
    # age_num peut être NA pour quelques observations → on les laisse dans "Inactif·ve"
    statut4 = case_when(
      acteu_ind == "Emploi"                                       ~ "En emploi",
      acteu_ind == "Chomage"                                      ~ "Au chômage",
      acteu_ind == "Inactif" & !is.na(age_num) & age_num >= 65   ~ "Retraité·e (65+)",
      acteu_ind == "Inactif"                                      ~ "Inactif·ve (18-64 ans)",
      TRUE                                                        ~ NA_character_
    )
  ) |>
  filter(!is.na(statut4))

# Taux de pauvreté par statut (4 catégories) et part dans population, par année
contribs_raw <- base_activite |>
  group_by(annee, statut4) |>
  summarise(
    taux   = weighted.mean(nivviem < seuil_std, wprm, na.rm = TRUE),
    poids  = sum(wprm),
    .groups = "drop"
  ) |>
  group_by(annee) |>
  mutate(
    share   = poids / sum(poids),
    contrib = taux * share          # contribution au taux global
  ) |>
  ungroup()

# Année de base : première disponible >= 2010
annee_base <- min(contribs_raw$annee[contribs_raw$annee >= 2010])

base_vals <- contribs_raw |>
  filter(annee == annee_base) |>
  select(statut4, contrib_base = contrib)

contrib_delta <- contribs_raw |>
  left_join(base_vals, by = "statut4") |>
  mutate(
    delta   = (contrib - contrib_base) * 100,
    tooltip = paste0(statut4, " — ", annee, "\n",
                     sprintf("%+.2f", delta), " pts vs ", annee_base),
    data_id = paste0(statut4, "_", annee)
  ) |>
  filter(!is.na(delta))

# Total observé (vérification)
total_delta <- contrib_delta |>
  group_by(annee) |>
  summarise(total = sum(delta), .groups = "drop")

# Ordre des catégories dans la légende
ordre_statut <- c("En emploi", "Au chômage",
                  "Inactif·ve (18-64 ans)", "Retraité·e (65+)")
contrib_delta <- contrib_delta |>
  mutate(statut4 = factor(statut4, levels = ordre_statut))

pal_activite <- c(
  "En emploi"              = "#1f78b4",
  "Au chômage"             = "#e31a1c",
  "Inactif·ve (18-64 ans)" = "#ff7f00",
  "Retraité·e (65+)"       = "#6a3d9a"
)

g_contrib_activite <- ggplot(
  contrib_delta,
  aes(x = annee, y = delta, fill = statut4)
) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.7
  ) +
  # Ligne du total (variation du taux de pauvreté global)
  geom_line(data = total_delta,
            aes(x = annee, y = total, fill = NULL),
            colour = "black", linewidth = 0.9, linetype = "dashed",
            inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
  scale_fill_manual(values = pal_activite, drop = FALSE) +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x       = NULL,
    y       = paste0("Variation du taux de pauvreté vs ", annee_base, " (pts de %)"),
    fill    = NULL,
    caption = paste0(
      caption_base, "\n",
      "Lecture : contribution_k = taux_pauvreté_k × part_k dans la population. ",
      "La ligne pointillée est la variation totale du taux de pauvreté.\n",
      "Retraité·e : individus inactifs de 65 ans et plus. Statut d'activité individuel (acteu)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_contrib_activite, file.path(path_fig, "g11b_contrib_activite.rds"))
cat("g11b_contrib_activite : ok\n")

# ==============================================================================
# Graphique 1b : Décomposition shift-share des contributions
#
# Pour chaque statut k et année t :
#   Δcontrib_k(t) = effet_taux + effet_structure + interaction
#
#   effet_taux      = (taux_k(t) - taux_k(t0)) × part_k(t0)   [pauvreté conditionnelle]
#   effet_structure = taux_k(t0) × (part_k(t) - part_k(t0))   [part dans la population]
#   interaction     = (Δtaux_k) × (Δpart_k)                    [résidu croisé]
#
# L'effet structure capture la hausse/baisse de la part du groupe dans la population.
# L'effet taux capture la variation de la pauvreté au sein du groupe.
# ==============================================================================

ss_base <- contribs_raw |>
  filter(annee == annee_base) |>
  select(statut4, taux_base = taux, part_base = share)

ss_data <- contribs_raw |>
  left_join(ss_base, by = "statut4") |>
  filter(annee != annee_base) |>
  mutate(
    delta_taux  = taux  - taux_base,
    delta_part  = share - part_base,
    effet_taux  = delta_taux * part_base * 100,
    effet_struc = taux_base  * delta_part * 100,
    interaction = delta_taux * delta_part * 100,
    total_check = effet_taux + effet_struc + interaction   # doit égaler Δcontrib
  )

ss_long <- ss_data |>
  select(annee, statut4, effet_taux, effet_struc, interaction) |>
  pivot_longer(
    cols      = c(effet_taux, effet_struc, interaction),
    names_to  = "composante",
    values_to = "valeur"
  ) |>
  mutate(
    composante = case_when(
      composante == "effet_taux"  ~ "Effet taux\n(pauvreté conditionnelle)",
      composante == "effet_struc" ~ "Effet structure\n(part dans la pop.)",
      composante == "interaction" ~ "Interaction"
    ),
    composante = factor(composante, levels = c(
      "Effet taux\n(pauvreté conditionnelle)",
      "Effet structure\n(part dans la pop.)",
      "Interaction"
    )),
    tooltip = paste0(
      statut4, " — ", annee,
      "\n", sub("\n", " ", composante), " : ", sprintf("%+.3f", valeur), " pt"
    ),
    data_id = paste0(composante, "_", statut4, "_", annee)
  )

# Ligne des totaux (delta contribution = somme des 3 effets)
ss_total <- ss_data |>
  select(annee, statut4, total = total_check)

pal_ss <- c(
  "Effet taux\n(pauvreté conditionnelle)" = "#e31a1c",
  "Effet structure\n(part dans la pop.)"  = "#1f78b4",
  "Interaction"                           = "#b3b3b3"
)

ordre_statut <- c("En emploi", "Au chômage",
                  "Inactif·ve (18-64 ans)", "Retraité·e (65+)")
ss_long  <- ss_long  |> mutate(statut4 = factor(statut4,  levels = ordre_statut))
ss_total <- ss_total |> mutate(statut4 = factor(statut4, levels = ordre_statut))

g_shiftshare <- ggplot(ss_long,
                       aes(x = annee, y = valeur, fill = composante)) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.7
  ) +
  geom_line(
    data        = ss_total,
    aes(x = annee, y = total, fill = NULL),
    colour      = "black", linewidth = 0.8, linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  facet_wrap(~statut4, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = pal_ss) +
  scale_x_continuous(breaks = seq(2010, 2023, 4)) +
  scale_y_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x       = NULL,
    y       = paste0("Contribution à l'écart vs ", annee_base, " (pts de %)"),
    fill    = NULL,
    caption = paste0(
      caption_base, "\n",
      "Lecture : effet structure = taux_base × Δpart ; ",
      "effet taux = Δtaux × part_base. Somme = variation totale de la contribution (ligne pointillée).\n",
      "Retraité·e : individus inactifs de 65 ans et plus."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    strip.text        = element_text(face = "bold", size = 10),
    plot.caption      = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_shiftshare, file.path(path_fig, "g11c_shiftshare.rds"))
cat("g11c_shiftshare : ok\n")

# ==============================================================================
# Graphique 0 : Taux d'emploi 18-64 ans et taux de pauvreté (double axe)
# ==============================================================================

base_constat <- data_all |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(!is.na(wprm), wprm > 0, !is.na(acteu_ind), !is.na(nivviem), !is.na(age_num))

# Taux de pauvreté global (tous âges)
taux_pauvrete <- base_constat |>
  group_by(annee) |>
  summarise(
    taux_pauv = 100 * sum(wprm * (nivviem < seuil_std), na.rm = TRUE) / sum(wprm),
    .groups = "drop"
  )

# Taux d'emploi : 18-64 ans
taux_emploi <- base_constat |>
  filter(age_num >= 18, age_num <= 64) |>
  group_by(annee) |>
  summarise(
    taux_empl = 100 * sum(wprm * (acteu_ind == "Emploi"), na.rm = TRUE) / sum(wprm),
    .groups = "drop"
  )

# Jointure
g0_data <- taux_pauvrete |>
  inner_join(taux_emploi, by = "annee") |>
  mutate(
    tooltip_pauv = paste0("Taux de pauvreté — ", annee, " : ", round(taux_pauv, 1), " %"),
    tooltip_empl = paste0("Taux d'emploi 18-64 — ", annee, " : ", round(taux_empl, 1), " %"),
    data_id_pauv = paste0("pauv_", annee),
    data_id_empl = paste0("empl_", annee)
  )

# Facteur de mise à l'échelle pour le double axe
# On veut que l'axe droit (pauvreté) aille de ~12 à ~16,
# et l'axe gauche (emploi) de ~55 à ~70.
# Transformation : taux_pauv_scaled = (taux_pauv - a) / b
# On aligne les plages : empl ~ [55,70], pauv ~ [12,16]
empl_min <- floor(min(g0_data$taux_empl)) - 1
empl_max <- ceiling(max(g0_data$taux_empl)) + 1
pauv_min <- floor(min(g0_data$taux_pauv)) - 0.5
pauv_max <- ceiling(max(g0_data$taux_pauv)) + 0.5

scale_fac <- (empl_max - empl_min) / (pauv_max - pauv_min)
shift     <- empl_min - pauv_min * scale_fac

g0_data <- g0_data |>
  mutate(taux_pauv_scaled = taux_pauv * scale_fac + shift)

g0_emploi_pauvrete <- ggplot(g0_data, aes(x = annee)) +
  # Taux d'emploi (axe gauche)
  geom_line_interactive(
    aes(y = taux_empl, colour = "Taux d'emploi 18-64 ans", group = 1),
    linewidth = 1.2
  ) +
  geom_point_interactive(
    aes(y = taux_empl, tooltip = tooltip_empl, data_id = data_id_empl,
        colour = "Taux d'emploi 18-64 ans"),
    size = 2.5
  ) +
  # Taux de pauvreté (axe droit, transformé sur l'échelle gauche)
  geom_line_interactive(
    aes(y = taux_pauv_scaled, colour = "Taux de pauvreté (éch. droite)", group = 1),
    linewidth = 1.2, linetype = "dashed"
  ) +
  geom_point_interactive(
    aes(y = taux_pauv_scaled, tooltip = tooltip_pauv, data_id = data_id_pauv,
        colour = "Taux de pauvreté (éch. droite)"),
    size = 2.5
  ) +
  # Axe droit (pauvreté)
  scale_y_continuous(
    name   = "Taux d'emploi 18-64 ans (%)",
    labels = label_number(suffix = " %"),
    sec.axis = sec_axis(
      # Closure avec valeurs baked-in : évite l'erreur "object not found"
      # lorsque le graphique est rechargé depuis le RDS dans une autre session.
      transform = local({
        .sh <- shift
        .sf <- scale_fac
        function(x) (x - .sh) / .sf
      }),
      name      = "Taux de pauvreté (%)",
      labels    = label_number(suffix = " %")
    )
  ) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_colour_manual(
    values = c(
      "Taux d'emploi 18-64 ans"         = "#1f78b4",
      "Taux de pauvreté (éch. droite)"  = "#e31a1c"
    )
  ) +
  labs(
    x       = NULL,
    colour  = NULL,
    caption = caption_base
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    axis.title.y.right = element_text(colour = "#e31a1c"),
    axis.text.y.right  = element_text(colour = "#e31a1c"),
    axis.title.y.left  = element_text(colour = "#1f78b4"),
    axis.text.y.left   = element_text(colour = "#1f78b4"),
    plot.caption = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g0_emploi_pauvrete, file.path(path_fig, "g0_emploi_pauvrete.rds"))
cat("g0_emploi_pauvrete : ok\n")

cat("\n=== constat_pauvrete.R terminé ===\n")
