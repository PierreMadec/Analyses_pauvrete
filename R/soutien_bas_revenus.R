# ==============================================================================
# soutien_bas_revenus.R
#
# Effet amortisseur des dispositifs de soutien aux BAS REVENUS D'ACTIVITÉ sur la
# pauvreté laborieuse, reconstitués en une enveloppe cohérente 2005-2024 :
#   - PPE  (Prime pour l'emploi)        : 2005-2015   [ppe]
#   - RSA activité                      : 2009-2015   [m_rsa_actm]
#   - Prime d'activité (PPA)            : 2016-2024   [ppa]
# La PPA (2016) remplace PPE + RSA activité : bascule nette, sans recouvrement.
#
# Contrefactuel : on retire l'enveloppe du niveau de vie et on recalcule le taux
# de pauvreté laborieuse. L'écart = nombre de points de pauvreté "amortis" par
# ces dispositifs. Mesure directe de l'effet de coefficients négatif de l'Oaxaca.
#
# Prérequis : figure/ppe_2005_2015.rds (extrait des fichiers ERFS source par
#             extract_ppe ; m_rsa_actm et ppa sont déjà dans data_all).
#
# Figures produites (figure/) :
#   soutien_effet_amortisseur — taux de pauvreté laborieuse observé vs sans enveloppe
#   soutien_enveloppe_montant — montant moyen et taux de recours de l'enveloppe
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"
ppe_path <- file.path(path_fig, "ppe_2005_2015.rds")
if (!file.exists(ppe_path))
  stop("figure/ppe_2005_2015.rds manquant — lancer d'abord l'extraction de la PPE.")
ppe <- readRDS(ppe_path)

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title.x     = element_blank()
    )
}

# ── Base travailleurs + enveloppe ─────────────────────────────────────────────
base <- data_all |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  left_join(ppe, by = c("annee", "ident")) |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64,
         !is.na(seuil_std)) |>
  mutate(
    ppe        = coalesce(ppe, 0),
    m_rsa_actm = coalesce(as.numeric(m_rsa_actm), 0),
    ppa        = coalesce(as.numeric(ppa), 0),
    env        = ppe + m_rsa_actm + ppa,                 # enveloppe annuelle, ménage
    nivvie_sans = nivviem - env / nb_uci
  )

# ── Taux observé vs contrefactuel (sans enveloppe) ────────────────────────────
serie <- base |>
  group_by(annee) |>
  summarise(
    `Observé`                = 100 * weighted.mean(nivviem     < seuil_std, wprm),
    `Sans soutien activité`  = 100 * weighted.mean(nivvie_sans < seuil_std, wprm),
    .groups = "drop"
  ) |>
  mutate(effet = `Sans soutien activité` - `Observé`)

serie_long <- serie |>
  select(annee, `Observé`, `Sans soutien activité`) |>
  pivot_longer(-annee, names_to = "scenario", values_to = "taux") |>
  mutate(
    scenario = factor(scenario, levels = c("Sans soutien activité", "Observé")),
    tooltip  = paste0(scenario, "\n", annee, " : ", round(taux, 1), " %"),
    data_id  = paste0(gsub("[^a-z]", "", tolower(scenario)), "_", annee)
  )

# Étiquettes d'écart (effet amortisseur) à années clés
etiq <- serie |>
  filter(annee %in% c(2010, 2016, 2019, 2024)) |>
  mutate(
    ymid    = (`Observé` + `Sans soutien activité`) / 2,
    label   = paste0("-", round(effet, 1), " pt"),
    tooltip = paste0("Effet amortisseur ", annee, " : -", round(effet, 1), " pt"),
    data_id = paste0("effet_", annee)
  )

pal_sc <- c("Sans soutien activité" = "#E91422", "Observé" = "#2674DD")

g_amort <- ggplot(serie_long, aes(x = annee, y = taux, colour = scenario, group = scenario)) +
  # repères de réforme
  geom_vline(xintercept = c(2009, 2016, 2019), linetype = "dotted",
             colour = "grey60", linewidth = 0.4) +
  annotate("text", x = 2009, y = Inf, label = "RSA activité", vjust = 1.3, hjust = -0.05,
           size = 2.8, colour = "grey45") +
  annotate("text", x = 2016, y = Inf, label = "Prime d'activité", vjust = 1.3, hjust = -0.05,
           size = 2.8, colour = "grey45") +
  annotate("text", x = 2019, y = Inf, label = "Revalo. 2019", vjust = 2.8, hjust = -0.05,
           size = 2.8, colour = "grey45") +
  geom_ribbon(data = serie, inherit.aes = FALSE,
              aes(x = annee, ymin = `Observé`, ymax = `Sans soutien activité`),
              fill = "grey80", alpha = 0.45) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.3) +
  geom_point_interactive(data = etiq, inherit.aes = FALSE,
                        aes(x = annee, y = ymid, tooltip = tooltip, data_id = data_id),
                        alpha = 0, size = 6) +
  scale_colour_manual(values = pal_sc) +
  scale_x_continuous(breaks = seq(2005, 2025, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    y = "taux de pauvreté laborieuse (%)",
    caption = paste0(
      "Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
      "Enveloppe soutien aux bas revenus d'activité : PPE (2005-2015) + RSA activité ",
      "(2009-2015) + prime d'activité (2016-2024). Effet amortisseur : +0,9 pt en 2010, ",
      "+2,0 pts en 2019.\n",
      "Contrefactuel : niveau de vie diminué de l'enveloppe. Champ : PR en emploi, 18-64 ans."
    )
  ) +
  theme_erfs()
saveRDS(g_amort, file.path(path_fig, "soutien_effet_amortisseur.rds"))
cat("soutien_effet_amortisseur : ok\n")

# ── Montant moyen (bénéficiaires) + taux de recours ───────────────────────────
# Figure facettée (deux panneaux) : robuste au rechargement RDS, sans variable
# d'environnement capturée par un axe secondaire.
montant <- base |>
  group_by(annee) |>
  summarise(
    `Montant annuel moyen par bénéficiaire (€)` = weighted.mean(env[env > 0], wprm[env > 0]),
    `Taux de recours (%)`                       = 100 * weighted.mean(env > 0, wprm),
    .groups = "drop"
  ) |>
  pivot_longer(-annee, names_to = "indicateur", values_to = "valeur") |>
  mutate(
    indicateur = factor(indicateur,
                        levels = c("Montant annuel moyen par bénéficiaire (€)",
                                   "Taux de recours (%)")),
    tooltip = paste0(annee, " : ",
                     ifelse(grepl("Montant", indicateur),
                            paste0(round(valeur), " €/an"),
                            paste0(round(valeur), " %"))),
    data_id = paste0(gsub("[^a-z]", "", tolower(indicateur)), "_", annee)
  )

g_montant <- ggplot(montant, aes(x = annee, y = valeur)) +
  geom_vline(xintercept = c(2009, 2016, 2019), linetype = "dotted",
             colour = "grey60", linewidth = 0.4) +
  geom_line(colour = "#1f78b4", linewidth = 1.0) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id),
                         colour = "#1f78b4", size = 2.3) +
  facet_wrap(~ indicateur, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2005, 2025, 2)) +
  expand_limits(y = 0) +
  labs(
    y = NULL,
    caption = paste0(
      "Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
      "Enveloppe PPE (2005-2015) + RSA activité (2009-2015) + prime d'activité (2016-2024). ",
      "Champ : PR en emploi, 18-64 ans."
    )
  ) +
  theme_erfs() +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "bold", size = 10))
saveRDS(g_montant, file.path(path_fig, "soutien_enveloppe_montant.rds"))
cat("soutien_enveloppe_montant : ok\n")

# ── Effet amortisseur par configuration de ménage (2022-2024) ─────────────────
lab_cfg <- c("Personne seule", "Famille monoparentale",
             "Couple mono-actif sans enfant", "Couple mono-actif avec enfant(s)",
             "Couple bi-actif")
parcfg <- base |>
  mutate(config = case_when(
    typmen == "Personne seule"                                   ~ "Personne seule",
    typmen == "Famille monoparentale"                            ~ "Famille monoparentale",
    typmen == "Couple sans enfant"    & biactivite != "Bi-actif" ~ "Couple mono-actif sans enfant",
    typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif" ~ "Couple mono-actif avec enfant(s)",
    biactivite == "Bi-actif"                                     ~ "Couple bi-actif",
    TRUE ~ NA_character_)) |>
  filter(!is.na(config), annee %in% 2022:2024) |>
  group_by(config) |>
  summarise(
    `Observé`               = 100 * weighted.mean(nivviem     < seuil_std, wprm),
    `Sans soutien activité` = 100 * weighted.mean(nivvie_sans < seuil_std, wprm),
    .groups = "drop") |>
  mutate(effet = `Sans soutien activité` - `Observé`,
         config = factor(config, levels = lab_cfg))

parcfg_long <- parcfg |>
  select(config, `Observé`, `Sans soutien activité`) |>
  pivot_longer(-config, names_to = "scenario", values_to = "taux") |>
  mutate(scenario = factor(scenario, levels = c("Sans soutien activité", "Observé")),
         tooltip = paste0(config, "\n", scenario, " : ", round(taux, 1), " %"),
         data_id = paste0(gsub("[^a-z]", "", tolower(config)), "_",
                          gsub("[^a-z]", "", tolower(scenario))))
etiq_cfg <- parcfg |>
  mutate(label = paste0("-", round(effet, 1), " pt"),
         tooltip = paste0(config, " : effet amortisseur -", round(effet, 1), " pt"),
         data_id = paste0("effet_", gsub("[^a-z]", "", tolower(config))))

g_parcfg <- ggplot(parcfg_long, aes(x = config, y = taux, fill = scenario)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id),
                       position = position_dodge(width = 0.7), width = 0.65) +
  geom_text_interactive(data = etiq_cfg, inherit.aes = FALSE,
                        aes(x = config, y = `Sans soutien activité`, label = label,
                            tooltip = tooltip, data_id = data_id),
                        vjust = -0.4, size = 3, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Sans soutien activité" = "#e31a1c", "Observé" = "#1f78b4")) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  coord_flip() +
  labs(x = NULL, y = "Taux de pauvreté laborieuse (%)",
       caption = paste0(
         "Source : INSEE, ERFS 2022-2024, calculs de l'auteur.\n",
         "Effet amortisseur de l'enveloppe PPE + RSA activité + prime d'activité, par configuration. ",
         "Champ : PR en emploi, 18-64 ans.")) +
  theme_erfs()
saveRDS(g_parcfg, file.path(path_fig, "soutien_par_config.rds"))
cat("soutien_par_config : ok\n")

cat("\n=== soutien_bas_revenus.R terminé ===\n")
