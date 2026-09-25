# ==============================================================================
# build_data_all_2024.R
#
# Ajoute le millésime ERFS 2024 à figure/data_all.rds et figure/seuils_annuels.rds,
# sans relancer tout decomposition_pauvrete.R (qui charge les 20 années de l'ERFS
# et ne tient pas en mémoire ici).
#
# - lit les 2 fichiers FPR 2024 (CSV) de /Users/pierremadec/Documents/ERFS_backup/ERFS 2024
# - réutilise construire_base_individus() et les helpers de R/decomposition_pauvrete.R
#   (évalués SANS exécuter la construction complète : coupe à la fin de la fonction)
# - empile la base 2024 sur data_all (mêmes colonnes), ré-écrit figure/data_all.rds
# - ajoute la ligne 2024 à seuils_annuels (seuil_std = 0,6 x médiane pondérée du
#   niveau de vie ; seuil_hapl = 0,6 x médiane pondérée du niveau de vie hors APL)
#
# Idempotent : ne fait rien si data_all contient déjà 2024.
# ==============================================================================

suppressMessages({library(tidyverse); library(data.table)})

repo <- "/Users/pierremadec/Documents/GitHub/Analyses_pauvrete"
setwd(repo)

rds_da <- "figure/data_all.rds"
rds_se <- "figure/seuils_annuels.rds"

data_all_old <- readRDS(rds_da)
if (2024L %in% data_all_old$annee) {
  message("data_all contient déjà 2024 — rien à faire.")
} else {

  # --- 1. Helpers + construire_base_individus() (définitions seules) ------------
  src <- readLines("R/decomposition_pauvrete.R")
  fin_fn <- grep("^construire_base_individus <- function", src)[1]
  # la fonction se referme sur le premier "^}" qui suit
  fin_fn <- fin_fn - 1 + which(src[fin_fn:length(src)] == "}")[1]
  eval(parse(text = paste(src[1:fin_fn], collapse = "\n")), envir = globalenv())
  stopifnot(is.function(construire_base_individus))

  # --- 2. Lecture ERFS 2024 (CSV FPR) -----------------------------------------
  dir24 <- "/Users/pierremadec/Documents/ERFS_backup/ERFS 2024"
  men24 <- as.data.frame(fread(file.path(dir24, "fpr_menage_2024.csv"),
                               showProgress = FALSE, na.strings = c("", "NA")))
  ind24 <- as.data.frame(fread(file.path(dir24, "fpr_indiv_2024.csv"),
                               showProgress = FALSE, na.strings = c("", "NA")))
  message(sprintf("  ERFS 2024 : ménage %d x %d, individus %d x %d",
                  nrow(men24), ncol(men24), nrow(ind24), ncol(ind24)))

  erfs <- list(menage2024 = men24, individus2024 = ind24)

  # --- 3. Construction de la base individus 2024 -----------------------------
  d2024 <- construire_base_individus(2024L)
  message(sprintf("  base 2024 : %s individus", format(nrow(d2024), big.mark = " ")))

  # --- 4. Empilement sur data_all (aligner exactement les colonnes) ---------
  cols <- names(data_all_old)
  manquantes <- setdiff(cols, names(d2024))
  if (length(manquantes) > 0)
    stop("Colonnes absentes de la base 2024 : ", paste(manquantes, collapse = ", "))
  d2024 <- d2024[, cols]

  # harmoniser les niveaux de facteurs sur ceux de data_all
  for (v in cols) {
    if (is.factor(data_all_old[[v]]) && !is.factor(d2024[[v]]))
      d2024[[v]] <- factor(d2024[[v]], levels = levels(data_all_old[[v]]))
    if (is.factor(data_all_old[[v]]) && is.factor(d2024[[v]]))
      d2024[[v]] <- factor(as.character(d2024[[v]]), levels = levels(data_all_old[[v]]))
  }

  data_all_new <- bind_rows(data_all_old, d2024)
  saveRDS(data_all_new, rds_da)
  message(sprintf("figure/data_all.rds : %s -> %s lignes (années %s)",
                  format(nrow(data_all_old), big.mark = " "),
                  format(nrow(data_all_new), big.mark = " "),
                  paste(range(data_all_new$annee), collapse = "-")))

  # --- 5. seuils_annuels : ligne 2024 -------------------------------------------
  seuils_old <- readRDS(rds_se)
  if (!(2024L %in% seuils_old$annee)) {
    med    <- mediane_ponderee(d2024$nivviem,          d2024$wprm)
    med_h  <- mediane_ponderee(d2024$nivviem_hors_apl, d2024$wprm)
    seuils_new <- bind_rows(
      seuils_old,
      tibble(annee = 2024L, seuil_std = 0.6 * med, seuil_hapl = 0.6 * med_h)
    ) |> arrange(annee)
    saveRDS(seuils_new, rds_se)
    message(sprintf("figure/seuils_annuels.rds : ligne 2024 ajoutée (seuil_std = %.0f €/an, %.0f €/mois)",
                    0.6 * med, 0.6 * med / 12))
  }

  # --- 6. Contrôles rapides --------------------------------------------------
  h24 <- d2024 |> distinct(ident, .keep_all = TRUE)
  message("\nRépartition des ménages 2024 (pondérée) — typmen :")
  print(round(100 * prop.table(xtabs(wprm ~ typmen, h24)), 1))
  message("handicap_ind 2024 : ", sum(d2024$handicap_ind %in% TRUE), " TRUE / ",
          sum(d2024$handicap_ind %in% FALSE), " FALSE / ", sum(is.na(d2024$handicap_ind)), " NA")
  message("acteu_ind 2024 : "); print(table(d2024$acteu_ind, useNA = "ifany"))
}
