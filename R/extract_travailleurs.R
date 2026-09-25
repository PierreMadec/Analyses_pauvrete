# ==============================================================================
# extract_travailleurs.R
#
# Extrait, par individu et par année (2005-2023), les variables individuelles
# nécessaires à l'analyse de la pauvreté laborieuse qui ne figurent pas dans
# data_all : salaire individuel, temps partiel, sous-emploi, lien au conjoint.
#
# Ces variables permettent de distinguer, pour chaque travailleur pauvre, les
# causes proximales de la pauvreté : salaire trop bas, durée trop faible
# (temps partiel), absence de second revenu, charges familiales.
#
# Source : fichiers ERFS individus (fpr_indiv_* + fpr_irf*), fusionnés par
# noindiv (le salaire est dans fpr_indiv, acteu parfois dans fpr_irf).
#
# Produit : figure/travailleurs_indiv.rds
#   tibble(annee, ident, noindiv, noi, noicon, lpr, emploi,
#          salaires_i, temps_partiel, txtp, sousemploi, age, sexe)
#   ident = clé ménage (jointure avec data_all$ident pour le contexte ménage).
# ==============================================================================

suppressMessages({library(haven); library(dplyr); library(purrr)})

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
path_fig  <- "figure"
if (!dir.exists(path_fig)) dir.create(path_fig)

read1 <- function(f) {
  ext <- tools::file_ext(f)
  out <- tryCatch(
    if (ext == "sas7bdat") read_sas(f)
    else if (ext == "csv") as.data.frame(data.table::fread(f, showProgress = FALSE,
                                                           na.strings = c("", "NA")))
    else read_dta(f),
    error = function(e) {
      if (ext == "dta") read_dta(f, encoding = "latin1") else stop(e)
    }
  )
  names(out) <- tolower(names(out))
  out
}

get1 <- function(df, cands) {
  hit <- intersect(cands, names(df))
  if (length(hit) == 0) return(rep(NA, nrow(df)))   # vecteur NA de bonne longueur
  df[[hit[1]]]
}

extraire_annee <- function(an) {
  d <- file.path(base_path, paste("ERFS", an))
  if (!dir.exists(d)) return(NULL)
  fs <- list.files(d, pattern = "\\.(dta|sas7bdat|csv)$", full.names = TRUE)
  fi <- fs[grepl("indiv|irf", basename(fs), ignore.case = TRUE)]
  if (length(fi) == 0) return(NULL)

  # Lire et fusionner les sous-fichiers individus par noindiv
  dfs <- lapply(fi, read1)
  # clé individu
  cle_noi <- "noindiv"
  dfs <- lapply(dfs, function(x) {
    if (!"noindiv" %in% names(x)) {
      alt <- grep("^noindiv$", names(x), value = TRUE)
      if (length(alt)) names(x)[names(x) == alt[1]] <- "noindiv"
    }
    x
  })
  keep <- vapply(dfs, function(x) "noindiv" %in% names(x), logical(1))
  dfs <- dfs[keep]
  if (length(dfs) == 0) return(NULL)
  ind <- reduce(dfs, function(x, y) {
    com <- setdiff(intersect(names(x), names(y)), "noindiv")
    if (length(com)) y <- y[, setdiff(names(y), com), drop = FALSE]
    left_join(x, y, by = "noindiv")
  })

  ident_col <- grep("^ident", names(ind), value = TRUE)[1]

  tibble(
    annee         = an,
    ident         = as.character(ind[[ident_col]]),
    noindiv       = as.character(ind$noindiv),
    noi           = as.character(get1(ind, "noi")),
    noicon        = as.character(get1(ind, c("noicon", "noiconmen"))),
    lpr           = suppressWarnings(as.integer(as.character(get1(ind, c("lpr", "lprm"))))),
    acteu_raw     = suppressWarnings(as.integer(as.character(get1(ind, "acteu")))),
    salaires_i    = suppressWarnings(as.numeric(get1(ind, "salaires_i"))),
    tppred_raw    = suppressWarnings(as.integer(as.character(get1(ind, "tppred")))),
    txtp_raw      = suppressWarnings(as.numeric(get1(ind, c("txtppred", "txtpp")))),
    sousempl_raw  = suppressWarnings(as.integer(as.character(get1(ind, c("sousemplr", "sousempl"))))),
    age           = suppressWarnings(as.numeric(get1(ind, "age"))),
    sexe          = suppressWarnings(as.integer(as.character(get1(ind, "sexe"))))
  ) |>
    mutate(
      emploi        = as.integer(acteu_raw == 1),               # acteu : 1 = en emploi (BIT)
      temps_partiel = as.integer(tppred_raw == 2),              # tppred : 1 complet, 2 partiel
      txtp          = ifelse(!is.na(txtp_raw) & txtp_raw > 0, txtp_raw, NA_real_),
      sousemploi    = as.integer(sousempl_raw == 1)
    ) |>
    select(annee, ident, noindiv, noi, noicon, lpr, emploi,
           salaires_i, temps_partiel, txtp, sousemploi, age, sexe)
}

# ── Test ou run complet ───────────────────────────────────────────────────────
annees <- if (exists("ANNEES_TEST")) ANNEES_TEST else 2005:2024

res <- list()
for (an in annees) {
  x <- tryCatch(extraire_annee(an), error = function(e) {
    message("  ", an, " : ERREUR ", conditionMessage(e)); NULL
  })
  if (!is.null(x)) {
    res[[as.character(an)]] <- x
    message(sprintf("  %d : n=%d | emploi=%.0f%% | sal>0 chez emploi=%.0f%% | tp dispo=%.0f%%",
                    an, nrow(x),
                    100 * mean(x$emploi, na.rm = TRUE),
                    100 * mean(x$salaires_i[x$emploi == 1] > 0, na.rm = TRUE),
                    100 * mean(!is.na(x$temps_partiel[x$emploi == 1]))))
  }
}
trav_indiv <- bind_rows(res)

if (!exists("ANNEES_TEST")) {
  saveRDS(trav_indiv, file.path(path_fig, "travailleurs_indiv.rds"))
  message(sprintf("travailleurs_indiv.rds : %d lignes, années %s",
                  nrow(trav_indiv), paste(range(trav_indiv$annee), collapse = "-")))
}
