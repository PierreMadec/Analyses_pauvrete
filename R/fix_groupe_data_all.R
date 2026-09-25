# ==============================================================================
# fix_groupe_data_all.R
#
# Suite de fix_typmen_data_all.R : reconstruit la colonne `groupe`
# (= interaction typmen × biactivite, pré-calculée à la construction) dans
# figure/data_all.rds.
#
# fix_typmen_data_all.R n'avait ré-étiqueté que `typmen` (et `typmen2`) ; or
# `groupe` est un facteur distinct dont les libellés « <typmen> / <biactivite> »
# ont été figés au moment de la construction avec les MAUVAIS libellés typmen.
# piste3_decompo_transferts.R filtre sur `groupe` (grepl) → decompo_transferts_config
# restait mélangé (monoparentales ↔ couples).
#
# On reconstruit groupe à partir du typmen déjà corrigé, à l'identique de
# construire_base_individus() :
#   df$groupe <- interaction(df$typmen, df$biactivite, drop = TRUE, sep = " / ")
#
# Idempotent : ne fait rien si groupe est déjà cohérent avec typmen.
# ==============================================================================

suppressMessages(library(dplyr))

rds_path <- "figure/data_all.rds"
data_all <- readRDS(rds_path)

# Cohérence : pour typmen == "Famille monoparentale", groupe doit commencer par
# "Famille monoparentale / ". Si ce n'est pas le cas → groupe mélangé.
mono <- !is.na(data_all$typmen) & data_all$typmen == "Famille monoparentale" &
        !is.na(data_all$groupe)
coherent <- length(which(mono)) > 0 &&
  all(startsWith(as.character(data_all$groupe[mono]), "Famille monoparentale / "))

if (coherent) {
  message("data_all$groupe déjà cohérent avec typmen — rien à faire.")
} else {
  data_all$groupe <- interaction(data_all$typmen, data_all$biactivite,
                                 drop = TRUE, sep = " / ")
  saveRDS(data_all, rds_path)
  message("figure/data_all.rds : colonne groupe reconstruite.")
  message("Niveaux :")
  print(levels(data_all$groupe))
}
