# ==============================================================================
# retraites_gel_pensions_pfjss2028.R
#
# Profil des retraités selon le montant de pension, au regard des tranches de
# l'avant-projet de LFSS 2028 :
#   - < 1 260 €/mois        : hors mesure (indexation normale)
#   - 1 260 - 2 034 €/mois  : sous-indexation
#   - >= 2 034 €/mois       : gel total
#
# Champ : individus (champ_calcul == 1) percevant une pension de retraite au
# sens strict (retraites_i > 0 — base + complémentaire ; exclut minimum
# vieillesse/ASPA, comptée à part dans prest_precarite_vieil). Le montant
# mensuel est une approximation (retraites_i annuel / 12) et correspond au
# concept ERFS "net de CSG déductible", pas au montant brut utilisé par les
# textes réglementaires — à prendre comme ordre de grandeur.
#
# On regarde, par tranche :
#   - statut d'occupation de la résidence principale (logT, ménage)
#   - part bénéficiaire d'une aide au logement (prest_logement > 0, ménage) —
#     proxy de bénéfice observé, pas d'éligibilité stricte aux APL
#   - niveau de vie moyen/médian du ménage
#
# Poids : wprm (poids individuel = poids ménage dupliqué par personne).
# ==============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tidyr)
})

base_path <- "/Users/pierremadec/Documents/ERFS_backup/ERFS 2023"

indiv <- read_dta(file.path(base_path, "fpr_indiv_2023.dta")) |>
  select(ident23, noi, champ_calcul, wprm, AGE, retraites_i) |>
  filter(champ_calcul == 1, retraites_i > 0) |>
  mutate(
    pension_mens = retraites_i / 12,
    tranche = cut(
      pension_mens,
      breaks = c(-Inf, 1260, 2034, Inf),
      labels = c("< 1 260 €/mois (hors mesure)",
                 "1 260 - 2 034 €/mois (sous-indexation)",
                 "≥ 2 034 €/mois (gel total)"),
      right = FALSE
    )
  )

menage <- read_dta(file.path(base_path, "fpr_menage_2023.dta")) |>
  select(ident23, champ_calcul, logT, prest_logement, revdispm, nivviem) |>
  filter(champ_calcul == 1) |>
  select(-champ_calcul) |>
  mutate(
    statut_occ = case_when(
      logT %in% c("1", "2") ~ "Propriétaire (accédant ou non)",
      logT == "3"            ~ "Locataire HLM",
      logT %in% c("4", "5")  ~ "Locataire parc privé",
      TRUE                   ~ "Logé gratuitement / autre"
    )
  )

d <- indiv |> inner_join(menage, by = "ident23")

wquant <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) * p)[1]]
}

# ── Effectifs et pension par tranche ───────────────────────────────────────────
synth <- d |>
  group_by(tranche) |>
  summarise(
    n_pop           = sum(wprm),
    pension_moy     = weighted.mean(pension_mens, wprm),
    pension_med     = wquant(pension_mens, wprm, 0.5),
    nivvie_moy      = weighted.mean(nivviem, wprm),
    nivvie_med      = wquant(nivviem, wprm, 0.5),
    part_aide_logt  = 100 * weighted.mean(prest_logement > 0, wprm),
    aide_logt_moy_benef = weighted.mean(prest_logement[prest_logement > 0],
                                         wprm[prest_logement > 0]),
    .groups = "drop"
  )
cat("\n=== Effectifs, pension, niveau de vie et aide au logement par tranche ===\n")
print(synth, width = Inf)

# ── Statut d'occupation par tranche ────────────────────────────────────────────
statut <- d |>
  group_by(tranche, statut_occ) |>
  summarise(n_pop = sum(wprm), .groups = "drop") |>
  group_by(tranche) |>
  mutate(part = 100 * n_pop / sum(n_pop)) |>
  ungroup()
cat("\n=== Statut d'occupation de la résidence principale, par tranche ===\n")
print(statut |> select(tranche, statut_occ, part) |> pivot_wider(names_from = statut_occ, values_from = part), width = Inf)

path_fig <- "../figure"
if (!dir.exists(path_fig)) dir.create(path_fig)
saveRDS(list(synth = synth, statut = statut), file.path(path_fig, "retraites_gel_pensions_2028.rds"))

# ── Graphiques ─────────────────────────────────────────────────────────────────
theme_cnle <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(colour = "grey30", size = 10.5),
      plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0),
      axis.title       = element_blank(),
      axis.text.x      = element_text(size = 10),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

pal_statut <- c(
  "Propriétaire (accédant ou non)" = "#2674DD",
  "Locataire HLM"                  = "#D4308D",
  "Locataire parc privé"           = "#F2A93B",
  "Logé gratuitement / autre"      = "#8D30D4"
)

p_statut <- ggplot(statut, aes(x = tranche, y = part, fill = statut_occ)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(part >= 3, paste0(gsub("\\.", ",", round(part)), " %"), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.6, fontface = "bold") +
  scale_fill_manual(values = pal_statut) +
  scale_y_continuous(labels = label_number(suffix = " %", decimal.mark = ",")) +
  labs(
    title = "Statut d'occupation des retraités, par tranche de pension mensuelle",
    subtitle = "France, 2023 — tranches de l'avant-projet de LFSS 2028 (sous-indexation 1 260-2 034 €, gel au-delà)",
    caption = "Champ : personnes percevant une pension de retraite (retraites_i > 0), champ_calcul = 1. Pondération par personne (wprm).\nPension mensuelle = retraites_i (annuel, net de CSG déductible) / 12 — approximation, pas le montant brut réglementaire.\nSource : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE)."
  ) +
  theme_cnle() +
  guides(fill = guide_legend(nrow = 2))

ggsave("retraites_gel_statut_logement.png", p_statut, width = 9.5, height = 6, dpi = 200, bg = "white")
message("Graphique enregistré : retraites_gel_statut_logement.png")

p_aide <- ggplot(synth, aes(x = tranche, y = part_aide_logt)) +
  geom_col(fill = "#2674DD", width = 0.55) +
  geom_text(aes(label = paste0(gsub("\\.", ",", round(part_aide_logt, 1)), " %")),
            vjust = -0.6, size = 4.2, fontface = "bold") +
  scale_y_continuous(limits = c(0, max(synth$part_aide_logt) * 1.3),
                      labels = label_number(suffix = " %", decimal.mark = ",")) +
  labs(
    title = "Part des retraités bénéficiaires d'une aide au logement, par tranche de pension",
    subtitle = "France, 2023 — tranches de l'avant-projet de LFSS 2028",
    caption = "Champ : personnes percevant une pension de retraite (retraites_i > 0), champ_calcul = 1. Pondération par personne (wprm).\nBénéfice observé d'une aide au logement (prest_logement > 0) — proxy, pas une éligibilité stricte aux APL.\nSource : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE)."
  ) +
  theme_cnle() +
  theme(axis.text.x = element_text(size = 9))

ggsave("retraites_gel_aide_logement.png", p_aide, width = 9, height = 5.5, dpi = 200, bg = "white")
message("Graphique enregistré : retraites_gel_aide_logement.png")
