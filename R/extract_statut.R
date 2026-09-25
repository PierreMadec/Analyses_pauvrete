# ==============================================================================
# extract_statut.R
#
# Extrait le statut d'emploi (salarié / non-salarié) de la personne de
# référence, 2005-2023, à partir de la variable STATUT des fichiers source
# ERFS (individus / IRF). Deux générations de codage successives :
#   - 2005-2019 : codage détaillé (Dictionnaire des codes EEC, ERFS 2010) —
#     11 Indépendants, 12 Employeurs, 13 Aides familiaux -> non-salarié ;
#     21+ (intérim, apprentis, CDD, etc.) -> salarié.
#   - 2021-2023 : codage simplifié après refonte de l'enquête Emploi —
#     1 = Non-salarié, 2 = Salarié.
#
# Sert à vérifier si la hausse de l'emploi non salarié (auto-entrepreneurs)
# sur la période affecte les analyses de salaire horaire, qui ne sont
# calculables que pour les salariés (variable salaires_i).
#
# Produit : figure/statut_pr.rds
#   tibble(annee, ident, non_salarie) — une ligne par personne de référence,
#   jointure sur (annee, ident) avec data_all.
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
  keep <- vapply(dfs, function(x) "noindiv" %in% names(x) && "statut" %in% names(x), logical(1))
  if (!any(keep)) return(NULL)
  ind <- dfs[[which(keep)[1]]]

  ident_col <- grep("^ident", names(ind), value = TRUE)[1]
  lpr_col   <- intersect(c("lpr", "lprm"), names(ind))[1]

  statut_vec  <- suppressWarnings(as.integer(as.character(ind$statut)))
  non_sal_vec <- if (an >= 2021) statut_vec == 1 else statut_vec %in% c(11, 12, 13)

  tibble(
    annee       = an,
    ident       = as.character(ind[[ident_col]]),
    noindiv     = as.character(ind$noindiv),
    lpr         = suppressWarnings(as.integer(as.character(ind[[lpr_col]]))),
    non_salarie = non_sal_vec
  )
}

annees <- if (exists("ANNEES_TEST")) ANNEES_TEST else 2005:2023

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d : n=%d", an, nrow(x)))
  }
}
statut_pr <- bind_rows(res) |>
  filter(lpr == 1) |>
  arrange(annee, ident, noindiv) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  select(annee, ident, non_salarie)

if (!exists("ANNEES_TEST")) {
  saveRDS(statut_pr, file.path(path_fig, "statut_pr.rds"))
  message(sprintf("statut_pr.rds : %d lignes, années %s",
                  nrow(statut_pr), paste(range(statut_pr$annee), collapse = "-")))
}
