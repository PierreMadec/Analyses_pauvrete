# ==============================================================================
# extract_anciennete.R
#
# Extrait l'ancienneté dans l'entreprise (ancentr, en mois), 2005-2019.
# Variable absente des fichiers ERFS 2020-2023 (disparition du questionnaire
# EEC correspondant après la refonte de l'enquête Emploi) : la série s'arrête
# donc avant la période récente 2021-2023, contrairement au reste de l'article.
#
# Produit : figure/anciennete.rds
#   tibble(annee, ident, noindiv, ancentr_mois)
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
  fi <- fs[grepl("irf|indiv", basename(fs), ignore.case = TRUE)]
  if (length(fi) == 0) return(NULL)

  dfs <- lapply(fi, read1)
  dfs <- lapply(dfs, function(x) {
    if (!"noindiv" %in% names(x)) {
      alt <- grep("^noindiv$", names(x), value = TRUE)
      if (length(alt)) names(x)[names(x) == alt[1]] <- "noindiv"
    }
    x
  })
  keep <- vapply(dfs, function(x) "noindiv" %in% names(x) && "ancentr" %in% names(x), logical(1))
  if (!any(keep)) return(NULL)
  ind <- dfs[[which(keep)[1]]]

  ident_col <- grep("^ident", names(ind), value = TRUE)[1]

  tibble(
    annee        = an,
    ident        = as.character(ind[[ident_col]]),
    noindiv      = as.character(ind$noindiv),
    ancentr_mois = suppressWarnings(as.numeric(ind$ancentr))
  ) |>
    filter(!is.na(ancentr_mois), ancentr_mois >= 0)
}

annees <- if (exists("ANNEES_TEST")) ANNEES_TEST else 2005:2019

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d : n=%d", an, nrow(x)))
  } else {
    message("  ", an, " : pas de ancentr trouvé")
  }
}
anciennete <- bind_rows(res)

if (!exists("ANNEES_TEST")) {
  saveRDS(anciennete, file.path(path_fig, "anciennete.rds"))
  message(sprintf("anciennete.rds : %d lignes, années %s",
                  nrow(anciennete), paste(range(anciennete$annee), collapse = "-")))
}
