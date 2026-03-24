# ==============================================================================
# Analyse approfondie : biactivité, enfants, et mécanismes redistributifs
# ==============================================================================

library(tidyverse)

cat("Chargement des données ERFS...\n")
source("charger_erfs.R")
source("decomposition_pauvrete.R")

# ==============================================================================
# 1. ÉVOLUTION DE LA BIACTIVITÉ PAR TYPE DE COUPLE
# ==============================================================================

cat("\n=== 1. BIACTIVITÉ PAR GROUPE (couples avec/sans enfants) ===\n\n")

biactivite_detail <- data_all |>
  filter(annee %in% c(2017, 2023), grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    groupe_enfants = case_when(
      nb_enfants == "0 enfant" ~ "Sans enfant",
      nb_enfants %in% c("1 enfant", "2 enfants") ~ "1-2 enfants",
      nb_enfants == "3+ enfants" ~ "3+ enfants",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(annee, groupe_enfants) |>
  summarise(
    n_couples = n(),
    pct_biactif = 100 * sum(biactivite == "Bi-actif", na.rm = TRUE) / n(),
    pct_monoactif = 100 * sum(biactivite == "Mono-actif", na.rm = TRUE) / n(),
    pct_sans_emploi = 100 * sum(biactivite == "Sans emploi", na.rm = TRUE) / n(),
    .groups = "drop"
  ) |>
  arrange(groupe_enfants, annee)

print(biactivite_detail)

# ==============================================================================
# 2. COMPOSITION DES COUPLES AVEC ENFANTS PAR NIVEAU DE VIE
# ==============================================================================

cat("\n\n=== 2. NIVEAU DE VIE MOYEN : couples avec/sans enfants ===\n\n")

nv_couples <- data_all |>
  filter(annee %in% c(2017, 2023), grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    groupe_enfants = case_when(
      nb_enfants == "0 enfant" ~ "Sans enfant",
      nb_enfants %in% c("1 enfant", "2 enfants") ~ "1-2 enfants",
      TRUE ~ "3+ enfants"
    )
  ) |>
  group_by(annee, groupe_enfants) |>
  summarise(
    nv_moyen = weighted.mean(nivviem, wprm, na.rm = TRUE),
    pct_pauvre = 100 * sum(nivviem < 1020, na.rm = TRUE) / n(), # seuil ~2017
    n_couples = n(),
    .groups = "drop"
  )

print(nv_couples)

# ==============================================================================
# 3. PARTS DE LA PA DANS LE REVENU : couples avec enfants
# ==============================================================================

cat("\n\n=== 3. RÔLE DE LA PRIME D'ACTIVITÉ ===\n\n")

pa_detail <- data_all |>
  filter(
    annee %in% c(2017, 2023),
    grepl("^Couple", typmen),
    nb_enfants %in% c("1 enfant", "2 enfants")
  ) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    m_rsa_actm = if_else(is.na(m_rsa_actm), 0, m_rsa_actm),
    pct_pa_dans_revenu = 100 * m_rsa_actm / (rev_avant_redist + 1) # +1 pour éviter division par 0
  ) |>
  group_by(annee, biactivite) |>
  summarise(
    n = n(),
    pa_moyenne = weighted.mean(m_rsa_actm, wprm, na.rm = TRUE),
    pa_median = weighted.median(m_rsa_actm, wprm),
    pct_beneficiaires = 100 * sum(m_rsa_actm > 0) / n(),
    nv_moyen = weighted.mean(nivviem, wprm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(annee, biactivite)

print(pa_detail)

# ==============================================================================
# 4. SÉLECTION : qui a des enfants en 2017 vs 2023 ?
# ==============================================================================

cat("\n\n=== 4. SÉLECTION : profil des couples avec enfants ===\n\n")

selection_enfants <- data_all |>
  filter(annee %in% c(2017, 2023), grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    avec_enfants = nb_enfants != "0 enfant"
  ) |>
  group_by(annee, avec_enfants) |>
  summarise(
    n_couples = n(),
    pct_biactif = 100 * sum(biactivite == "Bi-actif", na.rm = TRUE) / n(),
    pct_couple_monoparent = 100 * sum(grepl("monoparent", typmen)) / n(),
    nv_moyen = weighted.mean(nivviem, wprm, na.rm = TRUE),
    pct_pauvre = 100 * sum(pauvre) / n(),
    age_moyen = weighted.mean(age_num, wprm, na.rm = TRUE),
    .groups = "drop"
  )

print(selection_enfants)

# ==============================================================================
# 5. CONTRIBUTION DE LA PA À LA PAUVRETÉ CHEZ LES COUPLES AVEC ENFANTS
# ==============================================================================

cat("\n\n=== 5. IMPACT DE LA PA SUR LE TAUX DE PAUVRETÉ ===\n\n")

seuil_2017 <- 1020
seuil_2023 <- 1355

pa_impact <- data_all |>
  filter(annee %in% c(2017, 2023), grepl("^Couple", typmen), nb_enfants != "0 enfant") |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    seuil = if_else(annee == 2017, seuil_2017, seuil_2023),
    nv_sans_pa = nivviem - m_rsa_actm, # retirer la PA
    pauvre_avec_pa = nivviem < seuil,
    pauvre_sans_pa = nv_sans_pa < seuil
  ) |>
  group_by(annee) |>
  summarise(
    taux_pauvre_avec_pa = 100 * sum(pauvre_avec_pa) / n(),
    taux_pauvre_sans_pa = 100 * sum(pauvre_sans_pa) / n(),
    effect_pa_pts = 100 * sum(pauvre_avec_pa - pauvre_sans_pa) / n(),
    n_couples = n(),
    .groups = "drop"
  )

print(pa_impact)

# ==============================================================================
# 6. COMPARAISON OB : est-ce composition ou coefficients ?
# ==============================================================================

cat("\n\n=== 6. DÉCOMPOSITION : couples avec 2 enfants vs autres ===\n\n")

# Calculer les parts de couples avec enfants
composition_enfants <- data_all |>
  filter(annee %in% c(2017, 2023), grepl("^Couple", typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  mutate(
    deux_enfants = nb_enfants == "2 enfants"
  ) |>
  group_by(annee) |>
  summarise(
    pct_avec_2enfants = 100 * sum(deux_enfants) / n(),
    taux_pauvre_2enfants = 100 * sum((deux_enfants & pauvre)) / sum(deux_enfants),
    taux_pauvre_autres = 100 * sum((!deux_enfants & pauvre)) / sum(!deux_enfants),
    .groups = "drop"
  )

print(composition_enfants)

cat("\n\n✓ Analyse terminée.\n")
