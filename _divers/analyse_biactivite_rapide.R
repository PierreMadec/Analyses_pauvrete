# ==============================================================================
# Analyse rapide de la biactivité (depuis les .rds existants si disponibles)
# ==============================================================================

library(tidyverse)

# Essayer de charger tp_typmen qui contient déjà la décomposition par typmen
path_g <- "graphiques_decomposition"

if (file.exists(file.path(path_g, "tp_typmen.csv"))) {
  tp_typmen <- read_csv(file.path(path_g, "tp_typmen.csv"), show_col_types = FALSE)

  # Chercher les informations de biactivité si disponibles
  cat("Données disponibles tp_typmen :\n")
  print(head(tp_typmen, 20))
} else {
  cat("Fichier tp_typmen.csv non trouvé. Compilation complète nécessaire.\n")
}

# Alternative : examiner les graphiques sauvegardés
rds_files <- list.files(path_g, pattern = "^g.*\\.rds$")
cat("\n\nGraphiques disponibles :\n")
print(rds_files)
