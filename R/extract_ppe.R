# ==============================================================================
# extract_ppe.R
#
# La Prime pour l'emploi (PPE) était un crédit d'impôt : elle n'est pas chargée
# dans data_all (qui ne retient que prestations et impôts CSG/IRPP). On l'extrait
# ici directement des fichiers ménage ERFS source, pour 2005-2015 (dernière année
# de la PPE avant son remplacement par la prime d'activité en 2016).
#
# Produit : figure/ppe_2005_2015.rds  -> tibble(annee, ident, ppe)  [montant annuel ménage]
#
# Clé de jointure : ident<aa> (cohérente avec data_all$ident).
# ==============================================================================

suppressMessages({library(haven); library(dplyr)})

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
path_fig  <- "figure"
if (!dir.exists(path_fig)) dir.create(path_fig)

read1 <- function(f) {
  ext <- tools::file_ext(f)
  if (ext == "sas7bdat") read_sas(f) else read_dta(f)
}

out <- list()
for (an in 2005:2015) {
  d <- file.path(base_path, paste("ERFS", an))
  if (!dir.exists(d)) { message("Année ", an, " absente, ignorée."); next }
  fs <- list.files(d, pattern = "\\.(dta|sas7bdat)$", full.names = TRUE)
  fm <- fs[grepl("menage|mrf", basename(fs), ignore.case = TRUE)]

  got <- NULL
  for (f in fm) {
    cn <- tryCatch(tolower(names(read1(f)[0, ])), error = function(e) character())
    if ("ppe" %in% cn) {
      m <- read1(f); names(m) <- tolower(names(m))
      key <- grep("^ident", names(m), value = TRUE)[1]
      got <- tibble(annee = an,
                    ident = as.character(m[[key]]),
                    ppe   = as.numeric(m$ppe))
      break
    }
  }
  if (!is.null(got)) {
    out[[as.character(an)]] <- got
    message(sprintf("  %d : ppe extrait (n=%d, moy.>0=%d €)",
                    an, nrow(got), round(mean(got$ppe[got$ppe > 0], na.rm = TRUE))))
  } else {
    message("  ", an, " : ppe NON trouvée")
  }
}

ppe_all <- bind_rows(out)
saveRDS(ppe_all, file.path(path_fig, "ppe_2005_2015.rds"))
message(sprintf("ppe_2005_2015.rds : %d lignes, années %s",
                nrow(ppe_all), paste(range(ppe_all$annee), collapse = "-")))
