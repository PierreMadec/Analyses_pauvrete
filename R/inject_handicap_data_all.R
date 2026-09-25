# ==============================================================================
# inject_handicap_data_all.R
#
# Ajoute la colonne `handicap_ind` (indicateur individuel de situation de
# handicap / invalidité, cf. extract_handicap.R) à figure/data_all.rds.
#
# data_all ne conserve pas d'identifiant individuel : l'attache se fait par
# POSITION, année par année. `construire_base_individus()` produit les lignes
# dans l'ordre du fichier individus filtré par `!is.na(nivviem) & !is.na(wprm)`.
# On reproduit exactement ce pipeline et on VÉRIFIE l'alignement sur age_num et
# nivviem (taux de correspondance == 1 exigé) avant d'attacher la colonne.
#
# Années couvertes : 2021-2023 (ADMHANDR/AAH/INVALID). Autres années : NA.
# ==============================================================================

suppressMessages({library(haven); library(dplyr)})

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
rds_path  <- "figure/data_all.rds"

read1 <- function(f) {
  ext <- tools::file_ext(f)
  out <- tryCatch(
    if (ext == "sas7bdat") read_sas(f) else read_dta(f),
    error = function(e) if (ext == "dta") read_dta(f, encoding = "latin1") else stop(e)
  )
  names(out) <- tolower(names(out))
  out[, !duplicated(names(out))]
}

reconstruire_annee <- function(an) {
  dossier <- file.path(base_path, paste("ERFS", an))
  fs <- list.files(dossier, pattern = "\\.(dta|sas7bdat)$", full.names = TRUE)
  fm <- fs[grepl("menage|mrf", basename(fs), ignore.case = TRUE)][1]
  fi <- fs[grepl("irf|indiv", basename(fs), ignore.case = TRUE)][1]
  men <- read1(fm); ind <- read1(fi)

  cle <- grep("^ident", names(men), value = TRUE)[1]
  if ("champ_calcul" %in% names(men)) men <- men[as.integer(men$champ_calcul) == 1L, ]
  cle_ind <- grep(paste0("^", cle), names(ind), value = TRUE)[1]
  if (!is.na(cle_ind) && cle_ind != cle) names(ind)[names(ind) == cle_ind] <- cle

  g <- function(v) if (v %in% names(ind))
    suppressWarnings(as.integer(as.character(ind[[v]]))) else rep(NA_integer_, nrow(ind))
  admhandr <- g("admhandr"); aah <- g("aah"); invalid <- g("invalid")
  interroge <- !(is.na(admhandr) & is.na(aah) & is.na(invalid))
  hand <- (admhandr == 1) | (aah == 1) | (invalid == 1)
  hand <- ifelse(is.na(hand), FALSE, hand)

  tibble(
    age_chk      = suppressWarnings(as.numeric(as.character(ind$age))),
    nivviem_chk  = men$nivviem[match(as.character(ind[[cle]]), as.character(men[[cle]]))],
    wprm_chk     = men$wprm[match(as.character(ind[[cle]]),    as.character(men[[cle]]))],
    handicap_ind = ifelse(interroge, hand, NA)
  ) |>
    filter(!is.na(nivviem_chk), !is.na(wprm_chk))
}

data_all <- readRDS(rds_path)
stopifnot(!"handicap_ind" %in% names(data_all))
data_all$handicap_ind <- NA

for (an in 2021:2023) {
  idx <- which(data_all$annee == an)
  rec <- reconstruire_annee(an)
  if (length(idx) != nrow(rec))
    stop(sprintf("[%d] nrow mismatch : data_all=%d, reconstruit=%d", an, length(idx), nrow(rec)))

  ok_age <- mean(data_all$age_num[idx] == rec$age_chk, na.rm = TRUE)
  ok_niv <- mean(abs(data_all$nivviem[idx] - rec$nivviem_chk) < 1e-6, na.rm = TRUE)
  message(sprintf("  [%d] n=%d  match age=%.4f  match nivviem=%.4f", an, nrow(rec), ok_age, ok_niv))
  if (ok_age < 0.9999 || ok_niv < 0.9999)
    stop(sprintf("[%d] alignement positionnel non fiable — abandon", an))

  data_all$handicap_ind[idx] <- rec$handicap_ind
}

data_all$handicap_ind <- as.logical(data_all$handicap_ind)

message(sprintf("\nhandicap_ind ajoutée : %d TRUE / %d FALSE / %d NA (total %d lignes)",
                sum(data_all$handicap_ind %in% TRUE),
                sum(data_all$handicap_ind %in% FALSE),
                sum(is.na(data_all$handicap_ind)),
                nrow(data_all)))

saveRDS(data_all, rds_path)
message("figure/data_all.rds ré-écrit avec la colonne handicap_ind.")
