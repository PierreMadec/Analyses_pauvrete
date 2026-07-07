# ==============================================================================
# piste3_decompo_transferts.R
#
# Décomposition complète des transferts protégeant les travailleurs pauvres
# (Piste 3) : retrait séquentiel de chaque poste (APL, prestations familiales,
# enveloppe activité, minima sociaux).
#
# Figures produites :
#   decompo_transferts_evol.rds    — effet amortisseur de chaque poste, 2005-2023
#   decompo_transferts_config.rds  — décomposition par configuration, 2021-2023
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(
    uc = nb_uci,
    pauvre_obs   = nivviem < seuil_std,
    # Chaque poste ramené par UC (montant annuel ménage → par UC)
    apt_uc  = prest_logement / uc,
    pfam_uc = (prest_fam_petite_enfance + prest_fam_autres) / uc,
    act_uc  = (ppa + m_rsa_actm) / uc,
    prec_uc = (prest_precarite_rsa + prest_precarite_rmi + prest_precarite_api_rmi) / uc,
    # Contrefactuels (retrait d'un seul poste à la fois)
    pauvre_sans_apt  = (nivviem - apt_uc)  < seuil_std,
    pauvre_sans_pfam = (nivviem - pfam_uc) < seuil_std,
    pauvre_sans_act  = (nivviem - act_uc)  < seuil_std,
    pauvre_sans_prec = (nivviem - prec_uc) < seuil_std
  )

# ── Figure 1 : évolution temporelle des effets amortisseurs ──────────────────

evol <- base |>
  group_by(annee) |>
  summarise(
    tx_obs       = weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    effet_apt    = weighted.mean(pauvre_sans_apt,  wprm, na.rm = TRUE) -
                   weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    effet_pfam   = weighted.mean(pauvre_sans_pfam, wprm, na.rm = TRUE) -
                   weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    effet_act    = weighted.mean(pauvre_sans_act,  wprm, na.rm = TRUE) -
                   weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    effet_prec   = weighted.mean(pauvre_sans_prec, wprm, na.rm = TRUE) -
                   weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    .groups = "drop"
  )

evol_long <- evol |>
  pivot_longer(starts_with("effet_"), names_to = "poste", values_to = "effet") |>
  mutate(
    poste_lab = case_when(
      poste == "effet_apt"  ~ "Aides au logement (APL)",
      poste == "effet_pfam" ~ "Prestations familiales",
      poste == "effet_act"  ~ "Enveloppe activité\n(PPE + RSA act. + Prime d'activité)",
      poste == "effet_prec" ~ "Minima sociaux (RSA socle)"
    ),
    poste_lab = factor(poste_lab, levels = c(
      "Prestations familiales",
      "Aides au logement (APL)",
      "Enveloppe activité\n(PPE + RSA act. + Prime d'activité)",
      "Minima sociaux (RSA socle)"
    ))
  )

pal_postes <- c(
  "Prestations familiales"                              = "#1f78b4",
  "Aides au logement (APL)"                            = "#33a02c",
  "Enveloppe activité\n(PPE + RSA act. + Prime d'activité)" = "#ff7f00",
  "Minima sociaux (RSA socle)"                         = "#984ea3"
)

g_evol <- ggplot(evol_long,
  aes(x = annee, y = effet * 100, colour = poste_lab,
      tooltip = sprintf("%s — %d : +%.2f pt", poste_lab, annee, effet * 100),
      data_id = paste(poste_lab, annee))) +
  geom_line_interactive(linewidth = 1.0) +
  geom_point_interactive(size = 2.0) +
  scale_colour_manual(values = pal_postes) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x = NULL,
    y = "Points de pauvreté laborieuse amortis",
    colour = NULL,
    caption = paste0("Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans. Retraits indépendants (effets de premier ordre).")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_evol, file.path(path_fig, "decompo_transferts_evol.rds"))

# ── Figure 2 : décomposition par configuration (2021-2023) ───────────────────

config_decompo <- base |>
  filter(annee >= 2021) |>
  group_by(groupe) |>
  summarise(
    tx_obs     = weighted.mean(pauvre_obs,       wprm, na.rm = TRUE),
    effet_apt  = weighted.mean(pauvre_sans_apt,  wprm, na.rm = TRUE) - weighted.mean(pauvre_obs, wprm, na.rm = TRUE),
    effet_pfam = weighted.mean(pauvre_sans_pfam, wprm, na.rm = TRUE) - weighted.mean(pauvre_obs, wprm, na.rm = TRUE),
    effet_act  = weighted.mean(pauvre_sans_act,  wprm, na.rm = TRUE) - weighted.mean(pauvre_obs, wprm, na.rm = TRUE),
    effet_prec = weighted.mean(pauvre_sans_prec, wprm, na.rm = TRUE) - weighted.mean(pauvre_obs, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Garder configurations avec taux > 2%
  filter(tx_obs > 0.02) |>
  mutate(
    groupe_lab = case_when(
      grepl("Famille monoparentale / Mono", groupe) ~ "Famille monoparentale\n(mono-active)",
      grepl("Couple sans enfant / Mono",    groupe) ~ "Couple sans enfant\n(mono-actif)",
      grepl("Personne seule",               groupe) ~ "Personne seule",
      grepl("Famille monoparentale / Bi",   groupe) ~ "Famille monoparentale\n(bi-active)",
      grepl("Couple avec enfant.*Mono",     groupe) ~ "Couple avec enfants\n(mono-actif)",
      TRUE ~ as.character(groupe)
    )
  ) |>
  pivot_longer(starts_with("effet_"), names_to = "poste", values_to = "effet") |>
  mutate(
    poste_lab = case_when(
      poste == "effet_apt"  ~ "APL",
      poste == "effet_pfam" ~ "Prestations familiales",
      poste == "effet_act"  ~ "Enveloppe activité",
      poste == "effet_prec" ~ "Minima sociaux"
    ),
    poste_lab = factor(poste_lab, levels = c(
      "Prestations familiales", "APL", "Enveloppe activité", "Minima sociaux"
    )),
    groupe_lab = reorder(groupe_lab, tx_obs)
  )

pal_postes2 <- c(
  "Prestations familiales" = "#1f78b4",
  "APL"                    = "#33a02c",
  "Enveloppe activité"     = "#ff7f00",
  "Minima sociaux"         = "#984ea3"
)

g_config <- ggplot(config_decompo,
  aes(y = groupe_lab, x = effet * 100, fill = poste_lab,
      tooltip = sprintf("%s — %s : +%.2f pt", poste_lab, groupe_lab, effet * 100),
      data_id = paste(poste_lab, groupe_lab))) +
  geom_col_interactive(position = "dodge", width = 0.7) +
  scale_fill_manual(values = pal_postes2) +
  scale_x_continuous(labels = label_number(suffix = " pt")) +
  labs(
    x = "Points de pauvreté laborieuse amortis",
    y = NULL, fill = NULL,
    caption = paste0("Source : INSEE, ERFS 2021-2023, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans. Retraits indépendants.")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())

saveRDS(g_config, file.path(path_fig, "decompo_transferts_config.rds"))

message("Piste 3 : figures sauvegardées.")
