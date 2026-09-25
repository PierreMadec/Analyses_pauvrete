# ==============================================================================
# retraites_tableaux_logement.R
#
# Deux tableaux, tranches de pension de l'avant-projet de LFSS 2028 :
#   1. Statut d'occupation de la résidence principale par tranche
#   2. Bénéfice d'une aide au logement (oui/non) par tranche
#
# Champ, définitions et poids : voir retraites_gel_pensions_pfjss2028.R.
# ==============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(gt)
})

base_path <- "/Users/pierremadec/Documents/ERFS_backup/ERFS 2023"

indiv <- read_dta(file.path(base_path, "fpr_indiv_2023.dta")) |>
  select(ident23, noi, champ_calcul, wprm, retraites_i) |>
  filter(champ_calcul == 1, retraites_i > 0) |>
  mutate(
    pension_mens = retraites_i / 12,
    tranche = cut(
      pension_mens,
      breaks = c(-Inf, 1260, 2034, Inf),
      labels = c("< 1 260 €/mois", "1 260 - 2 034 €/mois", "≥ 2 034 €/mois"),
      right = FALSE
    )
  )

menage <- read_dta(file.path(base_path, "fpr_menage_2023.dta")) |>
  select(ident23, champ_calcul, logT, prest_logement) |>
  filter(champ_calcul == 1) |>
  select(-champ_calcul) |>
  mutate(
    statut_occ = case_when(
      logT %in% c("1", "2") ~ "Propriétaire",
      logT == "3"            ~ "Locataire HLM",
      logT %in% c("4", "5")  ~ "Locataire parc privé",
      TRUE                   ~ "Logé gratuitement / autre"
    ),
    aide_logt = if_else(prest_logement > 0, "Oui", "Non")
  )

d <- indiv |> inner_join(menage, by = "ident23")

# ── Tableau 1 : statut d'occupation par tranche ────────────────────────────────
tab1 <- d |>
  count(tranche, statut_occ, wt = wprm, name = "n_pop") |>
  group_by(tranche) |>
  mutate(part = 100 * n_pop / sum(n_pop)) |>
  ungroup() |>
  select(tranche, statut_occ, part) |>
  pivot_wider(names_from = statut_occ, values_from = part, values_fill = 0) |>
  relocate(tranche, Propriétaire, `Locataire HLM`, `Locataire parc privé`, `Logé gratuitement / autre`)

effectifs1 <- d |> count(tranche, wt = wprm, name = "n_pop") |> mutate(n_pop = n_pop / 1e6)
tab1 <- tab1 |> left_join(effectifs1, by = "tranche") |> rename(`Effectif (millions)` = n_pop)

gt1 <- tab1 |>
  gt(rowname_col = "tranche") |>
  tab_header(
    title = "Statut d'occupation de la résidence principale, par tranche de pension",
    subtitle = "France, 2023 — retraités (ERFS), tranches de l'avant-projet de LFSS 2028"
  ) |>
  fmt_number(columns = c(Propriétaire, `Locataire HLM`, `Locataire parc privé`, `Logé gratuitement / autre`),
             decimals = 1, pattern = "{x} %", dec_mark = ",") |>
  fmt_number(columns = `Effectif (millions)`, decimals = 2, dec_mark = ",") |>
  tab_source_note("Champ : personnes percevant une pension de retraite (retraites_i > 0), champ_calcul = 1. Pondération par personne (wprm).") |>
  tab_source_note("Source : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE).") |>
  cols_label(tranche = "Tranche de pension") |>
  tab_options(table.font.size = 14, heading.title.font.size = 17, column_labels.font.weight = "bold")

gtsave(gt1, "retraites_tab1_statut_occupation.png", vwidth = 1000, zoom = 2)
message("Tableau enregistré : retraites_tab1_statut_occupation.png")

# ── Tableau 2 : bénéfice d'une aide au logement, oui/non, par tranche ─────────
tab2 <- d |>
  count(tranche, aide_logt, wt = wprm, name = "n_pop") |>
  group_by(tranche) |>
  mutate(part = 100 * n_pop / sum(n_pop)) |>
  ungroup() |>
  select(tranche, aide_logt, part) |>
  pivot_wider(names_from = aide_logt, values_from = part, values_fill = 0) |>
  relocate(tranche, Oui, Non)

effectifs2 <- d |> filter(aide_logt == "Oui") |> count(tranche, wt = wprm, name = "n_pop") |> mutate(n_pop = n_pop / 1e3)
tab2 <- tab2 |> left_join(effectifs2, by = "tranche") |> rename(`Bénéficiaires (milliers)` = n_pop)

gt2 <- tab2 |>
  gt(rowname_col = "tranche") |>
  tab_header(
    title = "Bénéfice d'une aide au logement, par tranche de pension",
    subtitle = "France, 2023 — retraités (ERFS), tranches de l'avant-projet de LFSS 2028"
  ) |>
  fmt_number(columns = c(Oui, Non), decimals = 1, pattern = "{x} %", dec_mark = ",") |>
  fmt_number(columns = `Bénéficiaires (milliers)`, decimals = 0, dec_mark = ",", sep_mark = " ") |>
  tab_source_note("Champ : personnes percevant une pension de retraite (retraites_i > 0), champ_calcul = 1. Pondération par personne (wprm).") |>
  tab_source_note("Bénéfice observé d'une aide au logement (prest_logement > 0) — proxy, pas une éligibilité stricte aux APL.") |>
  tab_source_note("Source : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE).") |>
  cols_label(tranche = "Tranche de pension") |>
  tab_options(table.font.size = 14, heading.title.font.size = 17, column_labels.font.weight = "bold")

gtsave(gt2, "retraites_tab2_aide_logement.png", vwidth = 900, zoom = 2)
message("Tableau enregistré : retraites_tab2_aide_logement.png")

print(tab1)
print(tab2)
