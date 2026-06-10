library(haven)
library(dplyr)
library(purrr)
library(openxlsx)

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

# Chemin vers les données ERFS
base_path <- "/Users/pierremadec/Documents/ERFS_backup"

# Années à traiter (2005 à 2023, une sur deux)
annees <- seq(2005, 2023, by = 2)

message("=== Reconstitution des statuts d'occupation par décile ===\n")

# Créer une liste pour stocker les résultats par année
resultats_par_annee <- list()

for (annee in annees) {
  dossier <- file.path(base_path, paste("ERFS", annee))

  if (!dir.exists(dossier)) {
    message(paste("⚠️  Année", annee, ": dossier non trouvé, passage."))
    next
  }

  message(paste("\n📊 Traitement année", annee, "..."))

  # Lister les fichiers
  fichiers <- list.files(dossier, pattern = "\\.(sas7bdat|dta)$", full.names = TRUE)

  if (length(fichiers) == 0) {
    message(paste("  ⚠️  Pas de données trouvées"))
    next
  }

  # Charger les ménages
  fichiers_menage <- fichiers[grepl("menage|mrf", basename(fichiers), ignore.case = TRUE)]

  if (length(fichiers_menage) == 0) {
    message(paste("  ⚠️  Pas de données ménage trouvées"))
    next
  }

  # Charger et fusionner si nécessaire
  dfs_menage <- lapply(fichiers_menage, lire_fichier)

  if (length(dfs_menage) == 1) {
    menages <- normaliser_noms(dfs_menage[[1]])
  } else {
    cle <- grep("^ident", intersect(tolower(names(dfs_menage[[1]])), tolower(names(dfs_menage[[2]]))), value = TRUE)[1]
    menages <- normaliser_noms(reduce(dfs_menage, function(x, y) left_join(normaliser_noms(x), normaliser_noms(y), by = cle)))
  }

  # Chercher les variables pertinentes
  noms_col <- tolower(names(menages))

  # Variables pour le niveau de vie
  var_revenu <- c("revtot", "revenu_total", "revind", "revi", "revsalaires", "revenu_menage")[
    c("revtot", "revenu_total", "revind", "revi", "revsalaires", "revenu_menage") %in% noms_col
  ][1]

  # Variables pour la taille du ménage
  var_taille <- c("npers", "nbpers", "taille_menage", "tmenage")[
    c("npers", "nbpers", "taille_menage", "tmenage") %in% noms_col
  ][1]

  # Variables pour le statut d'occupation
  var_occupation <- c("tocc", "statut_occ", "occupation", "statoccup")[
    c("tocc", "statut_occ", "occupation", "statoccup") %in% noms_col
  ][1]

  # Variables de pondération
  var_poids <- c("poids", "pondev", "ponderations")[
    c("poids", "pondev", "ponderations") %in% noms_col
  ][1]

  if (is.na(var_revenu)) {
    message(paste("  ⚠️  Variable revenu non trouvée. Colonnes disponibles:", paste(noms_col[1:5], collapse = ", ")))
    next
  }

  if (is.na(var_taille)) {
    message(paste("  ⚠️  Variable taille ménage non trouvée"))
    next
  }

  if (is.na(var_occupation)) {
    message(paste("  ⚠️  Variable statut occupation non trouvée. Colonnes pertinentes:",
                   paste(grep("occ|statut|logement", noms_col, value = TRUE), collapse = ", ")))
    next
  }

  message(paste("  ✓ Colonnes identifiées: revenu=", var_revenu, ", taille=", var_taille, ", occupation=", var_occupation))

  # Préparer les données
  df_analyse <- menages %>%
    select(all_of(c(var_revenu, var_taille, var_occupation)),
           all_of(if(!is.na(var_poids)) var_poids else NULL)) %>%
    rename(revenu = !!var_revenu,
           taille = !!var_taille,
           occupation = !!var_occupation) %>%
    mutate(
      revenu = as.numeric(revenu),
      taille = as.numeric(taille),
      occupation = as.character(occupation),
      poids = if(!is.na(var_poids)) as.numeric(get(var_poids)) else 1
    ) %>%
    filter(!is.na(revenu), !is.na(taille), !is.na(occupation), taille > 0)

  # Calculer le niveau de vie (revenu / racine(taille))
  df_analyse <- df_analyse %>%
    mutate(
      niveau_vie = revenu / sqrt(taille),
      annee = annee
    )

  # Créer les déciles
  df_analyse <- df_analyse %>%
    mutate(
      decile = ntile(niveau_vie, 10)
    )

  # Compter les observations par décile et statut d'occupation
  tableau_croises <- df_analyse %>%
    group_by(decile, occupation) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = occupation, values_from = count, values_fill = 0)

  # Ajouter les totaux
  tableau_croises <- tableau_croises %>%
    mutate(total = rowSums(across(-decile)))

  resultats_par_annee[[as.character(annee)]] <- tableau_croises

  message(paste("  ✓ Année", annee, "traitée avec succès"))
}

if (length(resultats_par_annee) == 0) {
  stop("Aucune donnée n'a pu être traitée. Vérifiez les chemins et variables.")
}

message("\n=== Consolidation des résultats ===\n")

# Créer un fichier Excel avec les résultats
wb <- createWorkbook()

# Feuille de synthèse
addWorksheet(wb, "Synthese")
writeData(wb, "Synthese",
          data.frame(
            Annee = names(resultats_par_annee),
            NbDeciles = sapply(resultats_par_annee, nrow),
            NbCategories = sapply(resultats_par_annee, function(x) ncol(x) - 2)
          ))

# Feuille par année
for (annee in names(resultats_par_annee)) {
  sheet_name <- paste("Annee", annee)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, resultats_par_annee[[annee]])

  # Formater les nombres
  addStyle(wb, sheet_name,
           createStyle(numFmt = "#,##0"),
           rows = 2:(nrow(resultats_par_annee[[annee]]) + 1),
           cols = 2:ncol(resultats_par_annee[[annee]]),
           gridExpand = TRUE)
}

# Sauvegarder
output_file <- "statut_occupation_par_decile.xlsx"
saveWorkbook(wb, output_file)

message(paste("✓ Fichier Excel créé:", output_file))
message(paste("  Années traitées:", paste(names(resultats_par_annee), collapse = ", ")))
