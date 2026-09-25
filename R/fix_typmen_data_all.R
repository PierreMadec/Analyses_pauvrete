# ==============================================================================
# fix_typmen_data_all.R
#
# Corrige l'étiquetage de `typmen` (et recalcule `typmen2`) dans
# figure/data_all.rds.
#
# BUG corrigé : construire_base_individus() étiquetait les codes 1-5 de
# typmen5 / typlog5 avec le vecteur
#   c("Personne seule", "Couple sans enfant", "Couple avec enfant(s)",
#     "Famille monoparentale", "Autre")
# alors que le codage INSEE standard est
#   1 Personne seule · 2 Famille monoparentale · 3 Couple sans enfant ·
#   4 Couple avec enfant(s) · 5 Autre.
# Les positions 2, 3, 4 étaient donc permutées (monoparentales ↔ couples).
#
# Ce script ré-étiquette le facteur sur place (les codes entiers sous-jacents
# sont inchangés : il suffit de renommer les niveaux dans le bon ordre) puis
# reconstruit typmen2 à partir du typmen corrigé et de nb_enfants.
#
# Idempotent : ne fait rien si data_all a déjà les niveaux corrigés.
# ==============================================================================

suppressMessages(library(dplyr))

rds_path <- "figure/data_all.rds"
data_all <- readRDS(rds_path)

lv_faux    <- c("Personne seule", "Couple sans enfant",
                "Couple avec enfant(s)", "Famille monoparentale", "Autre")
lv_correct <- c("Personne seule", "Famille monoparentale",
                "Couple sans enfant", "Couple avec enfant(s)", "Autre")

if (identical(levels(data_all$typmen), lv_correct)) {
  message("data_all$typmen déjà corrigé — rien à faire.")
} else {
  stopifnot(identical(levels(data_all$typmen), lv_faux))

  # Ré-étiquetage : le code entier i garde sa place, on lui donne le bon libellé
  levels(data_all$typmen) <- lv_correct

  # Reconstruction de typmen2 (typmen corrigé × nb_enfants)
  ne <- as.character(data_all$nb_enfants)
  tm <- as.character(data_all$typmen)
  data_all$typmen2 <- factor(case_when(
    tm == "Personne seule"                              ~ "Personne seule",
    tm == "Couple sans enfant"                          ~ "Couple sans enfant",
    tm == "Couple avec enfant(s)" & ne == "1 enfant"    ~ "Couple 1 enfant",
    tm == "Couple avec enfant(s)" & ne == "2 enfants"   ~ "Couple 2 enfants",
    tm == "Couple avec enfant(s)" & ne == "3+ enfants"  ~ "Couple 3+ enfants",
    tm == "Famille monoparentale" & ne == "1 enfant"    ~ "Mono 1 enfant",
    tm == "Famille monoparentale" & ne %in% c("2 enfants", "3+ enfants") ~ "Mono 2+ enfants",
    TRUE                                                ~ "Autre"
  ), levels = c("Personne seule", "Couple sans enfant",
                "Couple 1 enfant", "Couple 2 enfants", "Couple 3+ enfants",
                "Mono 1 enfant", "Mono 2+ enfants", "Autre"))

  saveRDS(data_all, rds_path)

  h <- data_all |> distinct(annee, ident, .keep_all = TRUE) |> filter(annee == 2023)
  message("figure/data_all.rds corrigé. Répartition des ménages 2023 (pondérée) :")
  print(round(100 * prop.table(xtabs(wprm ~ typmen, h)), 1))
}
