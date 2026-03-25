library(tidyverse)
source("charger_erfs.R")

# Vérifier quels millésimes ont actecj3 ou actecj6
cat("=== Colonnes d'activité du conjoint par millésime ===\n\n")

for (an in names(erfs)) {
  if (grepl("individus", an)) {
    cols <- names(erfs[[an]])
    actecj_cols <- cols[grepl("actecj|acteu", cols, ignore.case = TRUE)]
    annee <- gsub("\\D", "", an)
    if (length(actecj_cols) > 0) {
      cat(sprintf("ERFS %s: %s\n", annee, paste(actecj_cols, collapse=", ")))
    } else {
      cat(sprintf("ERFS %s: (aucune colonne actecj)\n", annee))
    }
  }
}
