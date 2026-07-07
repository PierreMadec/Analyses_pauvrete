# ==============================================================================
# composition_travailleurs.R
#
# Décomposition de l'effet de composition Oaxaca en deux sous-effets :
#
#   (1) Effet démographique  : la part de chaque type de ménage dans la
#       population totale (PR 18-64) a-t-elle changé ?
#       → plus de familles monoparentales dans la population
#
#   (2) Effet taux d'emploi : le taux d'emploi de chaque type de ménage
#       a-t-il changé ?
#       → les familles monoparentales travaillent-elles davantage ?
#
# Les deux effets contribuent ensemble à la modification de la composition
# de la population des travailleurs. La distinction est cruciale pour
# l'interprétation normative : l'effet (2) est tendanciellement positif
# (insertion dans l'emploi), l'effet (1) reflète une évolution structurelle.
#
# Méthode : shift-share appliqué aux parts dans la population des travailleurs.
#   w_k(t) = s_k(t) × e_k(t) / R(t)
#   Δw_k ≈ Δs_k × e_k(t0) / R(t0)   [effet démographique]
#           + s_k(t0) × Δe_k / R(t0)  [effet taux d'emploi]
#
# Figures produites :
#   compo_shift_share_typmen — décomposition des Δpart parmi les travailleurs
#   compo_taux_emploi_typmen — taux d'emploi par type de ménage (2005-2023)
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
# 1. Base : tous les PR 18-64 (pas seulement ceux en emploi)
# ==============================================================================

base_pop <- data_all |>
  filter(!is.na(typmen), !is.na(acteu_ind), !is.na(wprm), wprm > 0,
         !is.na(age_num), age_num >= 18, age_num <= 64) |>
  filter(lpr == 1) |>
  # Simplifier typmen en 4 catégories lisibles
  mutate(
    typmen4 = case_when(
      typmen == "Personne seule"           ~ "Personne seule",
      typmen == "Famille monoparentale"    ~ "Famille monoparentale",
      typmen %in% c("Couple sans enfant",
                    "Couple avec enfant(s)") ~ "Couple",
      TRUE                                 ~ "Autre"
    )
  ) |>
  filter(typmen4 != "Autre")

# ==============================================================================
# 2. Calcul pour chaque année :
#    - s_k  = part de typmen k dans la population PR 18-64
#    - e_k  = taux d'emploi de typmen k
#    - R    = taux d'emploi global (= Σ s_k × e_k)
#    - w_k  = part de typmen k parmi les travailleurs (= s_k × e_k / R)
# ==============================================================================

stats_annee <- base_pop |>
  group_by(annee, typmen4) |>
  summarise(
    pop_k  = sum(wprm),
    emp_k  = sum(wprm * (acteu_ind == "Emploi")),
    .groups = "drop"
  ) |>
  group_by(annee) |>
  mutate(
    pop_tot = sum(pop_k),
    emp_tot = sum(emp_k),
    s_k     = pop_k / pop_tot,          # part dans la population
    e_k     = emp_k / pop_k,            # taux d'emploi
    R       = emp_tot / pop_tot,        # taux d'emploi global
    w_k     = emp_k / emp_tot           # part parmi les travailleurs
  ) |>
  ungroup()

# ==============================================================================
# 3. Shift-share : Δw_k décomposé en effet_demo et effet_emploi
#    Année de base = première disponible >= 2010
# ==============================================================================

annee_base <- min(stats_annee$annee[stats_annee$annee >= 2010])

base_vals <- stats_annee |>
  filter(annee == annee_base) |>
  select(typmen4, s_k0 = s_k, e_k0 = e_k, R0 = R, w_k0 = w_k)

ss_compo <- stats_annee |>
  left_join(base_vals, by = "typmen4") |>
  filter(annee != annee_base) |>
  mutate(
    delta_s     = s_k - s_k0,
    delta_e     = e_k - e_k0,
    # Effet démographique : population share change, emploi rate fixé
    effet_demo  = delta_s * e_k0 / R0 * 100,
    # Effet taux d'emploi : emploi rate change, population share fixé
    effet_emploi = s_k0 * delta_e / R0 * 100,
    # Interaction (second ordre)
    interaction  = delta_s * delta_e / R0 * 100,
    # Variation totale observée (pt de %)
    delta_w      = (w_k - w_k0) * 100
  )

# Labels
pal_typmen <- c(
  "Personne seule"        = "#1f78b4",
  "Famille monoparentale" = "#e31a1c",
  "Couple"                = "#33a02c"
)

ordre_typmen <- c("Famille monoparentale", "Personne seule", "Couple")

ss_long <- ss_compo |>
  select(annee, typmen4, effet_demo, effet_emploi, interaction, delta_w) |>
  pivot_longer(
    cols      = c(effet_demo, effet_emploi, interaction),
    names_to  = "composante",
    values_to = "valeur"
  ) |>
  mutate(
    composante = case_when(
      composante == "effet_demo"   ~ "Effet démographique\n(part dans la pop.)",
      composante == "effet_emploi" ~ "Effet taux d'emploi\n(emploi par typmen)",
      composante == "interaction"  ~ "Interaction"
    ),
    composante = factor(composante, levels = c(
      "Effet taux d'emploi\n(emploi par typmen)",
      "Effet démographique\n(part dans la pop.)",
      "Interaction"
    )),
    typmen4 = factor(typmen4, levels = ordre_typmen),
    tooltip = paste0(
      typmen4, " — ", annee, "\n",
      sub("\n", " ", composante), " : ",
      sprintf("%+.2f", valeur), " pt"
    ),
    data_id = paste0(composante, "_", typmen4, "_", annee)
  )

total_ss <- ss_compo |>
  mutate(typmen4 = factor(typmen4, levels = ordre_typmen))

pal_compo <- c(
  "Effet taux d'emploi\n(emploi par typmen)"  = "#1f78b4",
  "Effet démographique\n(part dans la pop.)"  = "#e31a1c",
  "Interaction"                               = "#b3b3b3"
)

g_compo_shift <- ggplot(ss_long,
                        aes(x = annee, y = valeur, fill = composante)) +
  geom_col_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    position = "stack", width = 0.7
  ) +
  geom_line(
    data        = total_ss,
    aes(x = annee, y = delta_w, fill = NULL),
    colour      = "black", linewidth = 0.8, linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  facet_wrap(~typmen4, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = pal_compo) +
  scale_x_continuous(breaks = seq(2010, 2023, 4)) +
  scale_y_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x       = NULL,
    y       = paste0("Variation de la part dans les travailleurs vs ",
                     annee_base, " (pts de %)"),
    fill    = NULL,
    caption = paste0(
      caption_base, "\n",
      "Lecture : effet taux d'emploi = s_k(t0) × Δe_k / R(t0) ; ",
      "effet démographique = Δs_k × e_k(t0) / R(t0).\n",
      "La ligne pointillée est la variation totale de la part dans les travailleurs."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  )

saveRDS(g_compo_shift, file.path(path_fig, "compo_shift_share_typmen.rds"))
cat("compo_shift_share_typmen : ok\n")

# ==============================================================================
# 4. Figure complémentaire : taux d'emploi par type de ménage (2005-2023)
#    Répond directement à "les familles monoparentales travaillent-elles plus ?"
# ==============================================================================

taux_emploi_typmen <- stats_annee |>
  filter(typmen4 %in% ordre_typmen) |>
  mutate(
    typmen4 = factor(typmen4, levels = ordre_typmen),
    taux_empl_pct = e_k * 100,
    tooltip = paste0(typmen4, " — ", annee, " : ",
                     round(taux_empl_pct, 1), " %"),
    data_id = paste0(typmen4, "_", annee)
  )

g_taux_emploi_typmen <- ggplot(
  taux_emploi_typmen,
  aes(x = annee, y = taux_empl_pct, colour = typmen4, group = typmen4)
) +
  geom_line_interactive(linewidth = 1.2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = pal_typmen) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %"),
                     limits = c(0, NA)) +
  labs(
    x       = NULL,
    y       = "Taux d'emploi (%)",
    colour  = NULL,
    caption = caption_base
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_taux_emploi_typmen, file.path(path_fig, "compo_taux_emploi_typmen.rds"))
cat("compo_taux_emploi_typmen : ok\n")

cat("\n=== composition_travailleurs.R terminé ===\n")
