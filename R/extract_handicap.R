# ==============================================================================
# extract_handicap.R
#
# Extrait, au niveau individuel, un indicateur de situation de handicap /
# invalidité à partir des fichiers source ERFS (fichier individus).
#
# L'ERFS ne comporte pas la question GALI (« limité dans les activités que les
# gens font habituellement, depuis au moins six mois, à cause d'un problème de
# santé »). L'indicateur retenu ici est la **reconnaissance administrative du
# handicap ou de l'invalidité**, disponible depuis 2021 :
#   - ADMHANDR == 1 : reconnaissance administrative de handicap (y compris AAH
#     et invalidité)            [2021-2023]
#   - AAH      == 1 : perception de l'AAH                        [2021-2023]
#   - INVALID  == 1 : perception d'une pension d'invalidité      [2023]
#
# handicap_ind = TRUE si l'une de ces trois conditions est vraie,
#                FALSE si la personne est interrogée mais dans aucune,
#                NA    si la question n'est pas posée (enfants) ou année < 2021.
#
# Produit : figure/handicap_ind.rds
#   tibble(annee, ident, noindiv, lpr, handicap_ind, handicap_men)
#   - handicap_ind : indicateur individuel (cf. supra)
#   - handicap_men : TRUE si AU MOINS un membre du ménage a handicap_ind == TRUE
#   Jointure : positionnelle par année avec data_all (même pipeline
#   ind |> filter(!is.na(nivviem), !is.na(wprm))), ou par (annee, noindiv).
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
  out[, !duplicated(names(out))]
}

# Années où au moins ADMHANDR est disponible
ANNEES_HANDICAP <- 2021:2023

extraire_annee <- function(an) {
  dossier <- file.path(base_path, paste("ERFS", an))
  if (!dir.exists(dossier)) return(NULL)
  fs <- list.files(dossier, pattern = "\\.(dta|sas7bdat)$", full.names = TRUE)

  # Fichier ménage (pour reproduire le champ + le filtre nivviem/wprm de data_all)
  fm <- fs[grepl("menage|mrf", basename(fs), ignore.case = TRUE)]
  fi <- fs[grepl("irf|indiv", basename(fs), ignore.case = TRUE)]
  if (length(fm) == 0 || length(fi) == 0) return(NULL)

  men <- read1(fm[1])
  ind <- read1(fi[1])

  cle <- grep("^ident", names(men), value = TRUE)[1]
  if ("champ_calcul" %in% names(men)) {
    men <- men[as.integer(men$champ_calcul) == 1L, ]
  }
  cle_ind <- grep(paste0("^", cle), names(ind), value = TRUE)[1]
  if (!is.na(cle_ind) && cle_ind != cle) names(ind)[names(ind) == cle_ind] <- cle
  if (!cle %in% names(ind)) return(NULL)

  lpr_col <- intersect(c("lpr", "lprm"), names(ind))[1]

  g <- function(v) if (v %in% names(ind))
    suppressWarnings(as.integer(as.character(ind[[v]]))) else rep(NA_integer_, nrow(ind))
  admhandr <- g("admhandr")
  aah      <- g("aah")
  invalid  <- g("invalid")

  # Interrogé = au moins une des variables prend une valeur non manquante
  interroge <- !(is.na(admhandr) & is.na(aah) & is.na(invalid))
  hand <- (admhandr == 1) | (aah == 1) | (invalid == 1)
  hand <- ifelse(is.na(hand), FALSE, hand)          # NA dans une seule var -> non
  handicap_ind <- ifelse(interroge, hand, NA)

  df <- tibble(
    annee        = an,
    ident        = as.character(ind[[cle]]),
    noindiv      = if ("noindiv" %in% names(ind)) as.character(ind$noindiv) else NA_character_,
    lpr          = suppressWarnings(as.integer(as.character(ind[[lpr_col]]))),
    nivviem      = men$nivviem[match(as.character(ind[[cle]]), as.character(men[[cle]]))],
    wprm         = men$wprm[match(as.character(ind[[cle]]),    as.character(men[[cle]]))],
    handicap_ind = handicap_ind
  ) |>
    filter(!is.na(nivviem), !is.na(wprm))          # même champ que construire_base_individus()

  df |>
    group_by(ident) |>
    mutate(handicap_men = any(handicap_ind %in% TRUE)) |>
    ungroup() |>
    select(annee, ident, noindiv, lpr, handicap_ind, handicap_men)
}

annees <- if (exists("ANNEES_TEST")) intersect(ANNEES_TEST, ANNEES_HANDICAP) else ANNEES_HANDICAP

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d : n=%d, dont handicap_ind=TRUE : %d (%.1f %%)",
                    an, nrow(x), sum(x$handicap_ind %in% TRUE),
                    100 * mean(x$handicap_ind %in% TRUE)))
  }
}

handicap_ind <- bind_rows(res)

if (!exists("ANNEES_TEST")) {
  saveRDS(handicap_ind, file.path(path_fig, "handicap_ind.rds"))
  message(sprintf("handicap_ind.rds : %d lignes, années %s",
                  nrow(handicap_ind),
                  paste(range(handicap_ind$annee), collapse = "-")))
}
