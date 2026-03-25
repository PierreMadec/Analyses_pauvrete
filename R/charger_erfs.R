library(haven)
library(dplyr)
library(purrr)

# Normaliser les noms de colonnes (minuscule)
normaliser_noms <- function(df) {
  names(df) <- tolower(names(df))
  df
}

# Fonction de lecture robuste avec fallback encodage latin1
lire_fichier <- function(f) {
  ext <- tools::file_ext(f)
  lire <- if (ext == "sas7bdat") read_sas else read_dta
  tryCatch(
    lire(f),
    error = function(e) {
      if (grepl("encoding|byte sequence", e$message, ignore.case = TRUE) && ext == "dta") {
        message(paste("    -> Retry avec encoding latin1:", basename(f)))
        read_dta(f, encoding = "latin1")
      } else {
        stop(e)
      }
    }
  )
}

# Chemin vers les données ERFS (sauvegardées en dehors du repo pour éviter les limites de GitHub)
base_path <- "/Users/pierremadec/Documents/ERFS_backup"

annees <- 2005:2023

erfs <- list()

for (annee in annees) {
  dossier <- file.path(base_path, paste("ERFS", annee))
  if (!dir.exists(dossier)) next

  # Lister les fichiers de donnees (.sas7bdat ou .dta)
  fichiers <- list.files(dossier, pattern = "\\.(sas7bdat|dta)$", full.names = TRUE)
  if (length(fichiers) == 0) {
    message(paste("Annee", annee, ": pas de donnees trouvees, on passe."))
    next
  }

  an <- as.character(annee)

  # --- MENAGES (menage ou mrf) ---
  fichiers_menage <- fichiers[grepl("menage|mrf", basename(fichiers), ignore.case = TRUE)]
  if (length(fichiers_menage) > 0) {
    dfs <- lapply(fichiers_menage, function(f) {
      message(paste("  Lecture menage:", basename(f)))
      lire_fichier(f)
    })
    nom <- paste0("menage", annee)
    if (length(dfs) == 1) {
      erfs[[nom]] <- normaliser_noms(dfs[[1]])
    } else {
      # Merge : fpr_menage left_join fpr_mrf par identifiant menage (identXX)
      cle <- grep("^ident", intersect(tolower(names(dfs[[1]])), tolower(names(dfs[[2]]))), value = TRUE)[1]
      message(paste("    -> Merge menage par:", cle))
      erfs[[nom]] <- normaliser_noms(reduce(dfs, function(x, y) left_join(normaliser_noms(x), normaliser_noms(y), by = cle)))
    }
    message(paste("Annee", annee, ": menage OK"))
  }

  # --- INDIVIDUS (indiv ou irf) ---
  fichiers_indiv <- fichiers[grepl("indiv|irf", basename(fichiers), ignore.case = TRUE)]
  if (length(fichiers_indiv) > 0) {
    dfs <- lapply(fichiers_indiv, function(f) {
      message(paste("  Lecture individus:", basename(f)))
      lire_fichier(f)
    })
    nom <- paste0("individus", annee)
    if (length(dfs) == 1) {
      erfs[[nom]] <- normaliser_noms(dfs[[1]])
    } else {
      # Merge : fpr_indiv left_join fpr_irf par noindiv
      cle <- grep("^noindiv", intersect(tolower(names(dfs[[1]])), tolower(names(dfs[[2]]))), value = TRUE)[1]
      message(paste("    -> Merge individus par:", cle))
      erfs[[nom]] <- normaliser_noms(reduce(dfs, function(x, y) left_join(normaliser_noms(x), normaliser_noms(y), by = cle)))
    }
    message(paste("Annee", annee, ": individus OK"))
  }
}

message("\n=== Chargement termine ===")
message(paste("Bases disponibles :", paste(names(erfs), collapse = ", ")))

# Retourner erfs pour que sourcoise puisse le cacher
erfs
