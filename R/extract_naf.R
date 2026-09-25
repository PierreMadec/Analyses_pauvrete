# ==============================================================================
# extract_naf.R
#
# Extrait le secteur d'activité de l'emploi principal (nafg010n, nomenclature
# NA 10 postes, NAF rév. 2) pour chaque individu, 2013-2023 (non disponible
# avant, la NAF rév. 2 n'étant introduite dans l'EEC qu'à partir de cette
# refonte de nomenclature).
#
# Codes NA10 (Dictionnaire des codes EEC, ERFS 2013) :
#   AZ Agriculture, sylviculture et pêche
#   BE Industrie manufacturière, industries extractives et autres
#   FZ Construction
#   GI Commerce, transports, hébergement et restauration
#   JZ Information et communication
#   KZ Activités financières et d'assurance
#   LZ Activités immobilières
#   MN Activités spécialisées, scientifiques, techniques, services admin. et soutien
#   OQ Administration publique, enseignement, santé humaine et action sociale
#   RU Autres activités de services
#
# Produit : figure/naf_secteur.rds
#   tibble(annee, ident, noindiv, secteur_naf)
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

lab_naf <- c(
  AZ = "Agriculture, sylviculture, pêche",
  BE = "Industrie",
  FZ = "Construction",
  GI = "Commerce, transports, hébergement-restauration",
  JZ = "Information et communication",
  KZ = "Activités financières et d'assurance",
  LZ = "Activités immobilières",
  MN = "Activités spécialisées, scientifiques et de soutien",
  OQ = "Administration publique, enseignement, santé, action sociale",
  RU = "Autres activités de services"
)

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
  keep <- vapply(dfs, function(x) "noindiv" %in% names(x) && "nafg010n" %in% names(x), logical(1))
  if (!any(keep)) {
    keep_id <- vapply(dfs, function(x) "noindiv" %in% names(x), logical(1))
    dfs2 <- dfs[keep_id]
    if (length(dfs2) == 0) return(NULL)
    ind <- reduce(dfs2, function(x, y) {
      com <- setdiff(intersect(names(x), names(y)), "noindiv")
      if (length(com)) y <- y[, setdiff(names(y), com), drop = FALSE]
      left_join(x, y, by = "noindiv")
    })
  } else {
    ind <- dfs[[which(keep)[1]]]
  }
  if (!"nafg010n" %in% names(ind)) return(NULL)

  ident_col <- grep("^ident", names(ind), value = TRUE)[1]

  tibble(
    annee       = an,
    ident       = as.character(ind[[ident_col]]),
    noindiv     = as.character(ind$noindiv),
    naf_code    = trimws(as.character(ind$nafg010n))
  ) |>
    mutate(secteur_naf = unname(lab_naf[naf_code])) |>
    filter(!is.na(secteur_naf)) |>
    select(annee, ident, noindiv, secteur_naf)
}

annees <- if (exists("ANNEES_TEST")) ANNEES_TEST else 2013:2023

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d : n=%d secteurs renseignés", an, nrow(x)))
  } else {
    message("  ", an, " : pas de nafg010n trouvé")
  }
}
naf_secteur <- bind_rows(res)

if (!exists("ANNEES_TEST")) {
  saveRDS(naf_secteur, file.path(path_fig, "naf_secteur.rds"))
  message(sprintf("naf_secteur.rds : %d lignes, années %s",
                  nrow(naf_secteur), paste(range(naf_secteur$annee), collapse = "-")))
}
