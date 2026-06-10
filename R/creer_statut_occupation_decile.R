library(haven)
library(tidyverse)
library(openxlsx)

message("=== Répartition des statuts d'occupation par décile de niveau de vie ===\n")

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
annees    <- setdiff(2005:2023, c(2013, 2014, 2017))

# Codage "so" (1-6) et "logt" (1-6) : même nomenclature
recode_so <- function(v) {
  val <- suppressWarnings(as.integer(as.character(v)))
  case_when(
    val %in% 1:3 ~ "Propriétaire",
    val == 4     ~ "Locataire HLM",
    val == 5     ~ "Locataire non-HLM",
    val == 6     ~ "Logé gratuitement",
    TRUE         ~ NA_character_
  )
}

resultats <- list()

for (annee in annees) {
  dossier <- file.path(base_path, paste("ERFS", annee))
  if (!dir.exists(dossier)) {
    message(paste("⚠️", annee, ": dossier manquant"))
    next
  }

  an_short <- substr(as.character(annee), 3, 4)
  cle      <- paste0("ident", an_short)

  message(paste("📊", annee, "..."))

  # ── Charger menage (nivviem + wprm) ──────────────────────────────
  f_men <- list.files(dossier,
                      pattern = "menage.*\\.(sas7bdat|dta)$",
                      full.names = TRUE)[1]
  if (is.na(f_men)) { message("  ⚠️  pas de fichier menage"); next }

  men <- if (grepl("\\.dta$", f_men)) read_dta(f_men) else read_sas(f_men)
  names(men) <- tolower(names(men))

  vars_men <- intersect(c(cle, "wprm", "nivviem", "logt"), names(men))
  men <- men[, vars_men, drop = FALSE]

  # ── Statut d'occupation ───────────────────────────────────────────
  if ("logt" %in% names(men)) {
    # logt directement dans menage (2015+)
    df <- men %>%
      mutate(statut = recode_so(logt)) %>%
      select(all_of(cle), wprm, nivviem, statut)
    message("    source: logt (menage)")

  } else {
    # so dans fpr_mrf
    f_mrf <- list.files(dossier,
                        pattern = "mrf.*\\.(sas7bdat|dta)$",
                        full.names = TRUE)[1]
    if (is.na(f_mrf)) { message("  ⚠️  pas de fichier mrf et pas de logt"); next }

    mrf <- if (grepl("\\.dta$", f_mrf)) read_dta(f_mrf) else read_sas(f_mrf)
    names(mrf) <- tolower(names(mrf))

    if (!("so" %in% names(mrf))) { message("  ⚠️  variable so absente dans mrf"); next }

    mrf <- mrf %>%
      select(all_of(cle), so) %>%
      mutate(statut = recode_so(so)) %>%
      select(all_of(cle), statut)

    df <- men %>%
      select(all_of(cle), wprm, nivviem) %>%
      left_join(mrf, by = cle)
    message("    source: so (mrf)")
  }

  # ── Préparer et calculer ──────────────────────────────────────────
  df <- df %>%
    mutate(
      wprm    = as.numeric(wprm),
      nivviem = as.numeric(nivviem)
    ) %>%
    filter(!is.na(statut), !is.na(nivviem), !is.na(wprm), wprm > 0)

  n_valid <- nrow(df)
  pct_valid <- round(100 * n_valid / (n_valid + sum(is.na(df$statut))), 1)
  message(paste("    ménages valides:", n_valid))

  # Quintiles de niveau de vie (pondérés par wprm)
  df <- df %>%
    arrange(nivviem) %>%
    mutate(
      cum_w   = cumsum(wprm),
      total_w = sum(wprm),
      quintile = ceiling(cum_w / total_w * 5)
    ) %>%
    mutate(quintile = pmin(quintile, 5))

  # Tableau croisé en % (pondéré)
  tab <- df %>%
    group_by(quintile, statut) %>%
    summarise(w = sum(wprm), .groups = "drop") %>%
    group_by(quintile) %>%
    mutate(pct = 100 * w / sum(w)) %>%
    ungroup() %>%
    select(quintile, statut, pct) %>%
    pivot_wider(names_from = statut, values_from = pct, values_fill = 0) %>%
    arrange(quintile)

  resultats[[as.character(annee)]] <- tab
  message(paste("    ✓ OK"))
}

if (length(resultats) == 0) stop("Aucune donnée traitée.")

message(paste("\nAnnées ok:", paste(names(resultats), collapse = ", ")))

# ─────────────────────────────────────────────────────────────────────
# Export Excel — un seul onglet, une ligne par (année × quintile)
# ─────────────────────────────────────────────────────────────────────
message("\nCréation du fichier Excel...")

tableau_final <- bind_rows(
  lapply(names(resultats), function(a) {
    resultats[[a]] %>% mutate(Année = as.integer(a), .before = quintile)
  })
) %>%
  mutate(
    Total    = rowSums(across(-c(Année, quintile))),
    quintile = paste0("Q", quintile)
  ) %>%
  rename(Quintile = quintile)

wb <- createWorkbook()
sht <- "Données"
addWorksheet(wb, sht)

st_titre  <- createStyle(fontSize = 12, textDecoration = "bold",
                         fgFill = "#1F4E78", fontColour = "white",
                         halign = "center", valign = "center")
st_header <- createStyle(textDecoration = "bold", fgFill = "#4472C4",
                         fontColour = "white", halign = "center",
                         border = "Bottom", borderColour = "#1F4E78")
st_annee  <- createStyle(textDecoration = "bold", halign = "center")
st_quin   <- createStyle(halign = "center")
st_pct    <- createStyle(numFmt = "0.0", halign = "center")

n_col <- ncol(tableau_final)
n_lig <- nrow(tableau_final)

mergeCells(wb, sht, rows = 1, cols = 1:n_col)
writeData(wb, sht, "Statut d'occupation par quintile de niveau de vie — 2005-2023",
          startRow = 1, startCol = 1)
addStyle(wb, sht, st_titre,  rows = 1,            cols = 1:n_col, gridExpand = TRUE)
setRowHeights(wb, sht, rows = 1, heights = 28)

writeData(wb, sht, tableau_final, startRow = 2, startCol = 1)
addStyle(wb, sht, st_header, rows = 2,            cols = 1:n_col, gridExpand = TRUE)
addStyle(wb, sht, st_annee,  rows = 3:(n_lig + 2), cols = 1,     gridExpand = TRUE)
addStyle(wb, sht, st_quin,   rows = 3:(n_lig + 2), cols = 2,     gridExpand = TRUE)
addStyle(wb, sht, st_pct,    rows = 3:(n_lig + 2), cols = 3:n_col, gridExpand = TRUE)

setColWidths(wb, sht, cols = 1,      widths = 8)
setColWidths(wb, sht, cols = 2,      widths = 10)
setColWidths(wb, sht, cols = 3:n_col, widths = 18)

output_file <- "statut_occupation_par_quintile.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)
message(paste("✅ Fichier créé :", output_file))
