# ==============================================================================
# extract_urbanisation.R
#
# Extrait le degré d'urbanisation du ménage (rural / urbain hors Paris / Paris)
# pour 2005-2023, à partir de trois variables successives dans les fichiers
# ménage ERFS (même niveau de nomenclature à 5 postes jusqu'en 2020, puis
# refonte 2020 à 9 postes) :
#
#   - tur5      (2005-2012) : 1 Rural, 2-4 Urbain, 5 Agglomération parisienne
#     (Dictionnaire des codes EEC, ERFS 2010 : "Tranche d'unité urbaine 5 postes")
#   - tuu2010r  (2013-2020) : même échelle 1-5 (confirmé par reste_a_vivre.R)
#   - tuu2020   (2021-2023) : 0 Rural, 1-7 Urbain, 8 Paris (nomenclature 2020)
#
# Produit : figure/urbanisation.rds
#   tibble(annee, ident, zone_urbaine)  — zone_urbaine à 3 postes, jointure sur
#   (annee, ident) avec data_all.
# ==============================================================================

suppressMessages({library(haven); library(dplyr); library(purrr)})

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
path_fig  <- "figure"
if (!dir.exists(path_fig)) dir.create(path_fig)

read1 <- function(f) {
  ext <- tools::file_ext(f)
  out <- tryCatch(
    if (ext == "sas7bdat") read_sas(f) else read_dta(f),
    error = function(e) {
      if (ext == "dta") read_dta(f, encoding = "latin1") else stop(e)
    }
  )
  names(out) <- tolower(names(out))
  out
}

extraire_annee <- function(an) {
  d <- file.path(base_path, paste("ERFS", an))
  if (!dir.exists(d)) return(NULL)
  fs <- list.files(d, pattern = "\\.(dta|sas7bdat)$", full.names = TRUE)
  fm <- fs[grepl("menage", basename(fs), ignore.case = TRUE)]
  if (length(fm) == 0) return(NULL)

  df <- read1(fm[1])
  # Certaines années (ex. 2007) portent la variable d'urbanisation dans le
  # fichier ménage-revenus fiscaux (mrf) plutôt que le fichier ménage.
  if (!any(c("tur5", "tuu2010r", "tuu2020") %in% names(df))) {
    fmrf <- fs[grepl("mrf", basename(fs), ignore.case = TRUE)]
    if (length(fmrf) > 0) {
      dfm <- read1(fmrf[1])
      if (any(c("tur5", "tuu2010r", "tuu2020") %in% names(dfm))) df <- dfm
    }
  }
  ident_col <- grep("^ident", names(df), value = TRUE)[1]

  if ("tur5" %in% names(df)) {
    raw <- suppressWarnings(as.integer(as.character(df$tur5)))
    zone <- case_when(
      raw == 1            ~ "Rural",
      raw %in% 2:4         ~ "Urbain hors Paris",
      raw == 5             ~ "Paris",
      TRUE ~ NA_character_
    )
    source_var <- "tur5"
  } else if ("tuu2010r" %in% names(df)) {
    raw <- suppressWarnings(as.integer(as.character(df$tuu2010r)))
    zone <- case_when(
      raw == 1            ~ "Rural",
      raw %in% 2:4         ~ "Urbain hors Paris",
      raw == 5             ~ "Paris",
      TRUE ~ NA_character_
    )
    source_var <- "tuu2010r"
  } else if ("tuu2020" %in% names(df)) {
    raw <- suppressWarnings(as.integer(as.character(df$tuu2020)))
    zone <- case_when(
      raw == 0            ~ "Rural",
      raw %in% 1:7         ~ "Urbain hors Paris",
      raw == 8             ~ "Paris",
      TRUE ~ NA_character_
    )
    source_var <- "tuu2020"
  } else {
    message("  ", an, " : aucune variable d'urbanisation trouvée"); return(NULL)
  }

  tibble(
    annee        = an,
    ident        = as.character(df[[ident_col]]),
    zone_urbaine = zone
  ) |> mutate(source_var = source_var)
}

annees <- if (exists("ANNEES_TEST")) ANNEES_TEST else 2005:2023

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d (%s) : n=%d | Rural=%.0f%% | Urbain=%.0f%% | Paris=%.0f%% | NA=%.0f%%",
                    an, x$source_var[1], nrow(x),
                    100 * mean(x$zone_urbaine == "Rural", na.rm = TRUE),
                    100 * mean(x$zone_urbaine == "Urbain hors Paris", na.rm = TRUE),
                    100 * mean(x$zone_urbaine == "Paris", na.rm = TRUE),
                    100 * mean(is.na(x$zone_urbaine))))
  }
}
urbanisation <- bind_rows(res) |> select(annee, ident, zone_urbaine)

if (!exists("ANNEES_TEST")) {
  saveRDS(urbanisation, file.path(path_fig, "urbanisation.rds"))
  message(sprintf("urbanisation.rds : %d lignes, années %s",
                  nrow(urbanisation), paste(range(urbanisation$annee), collapse = "-")))
}
