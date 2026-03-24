library(tidyverse)
library(haven)
library(glue)
library(ggiraph)

# ==============================================================================
# ÉTAPE 1 : Taux de consommation par COICOP 2 digits dans BDF 2017
# ==============================================================================
message("=== Étape 1 : BDF 2017 — Taux de consommation par strate (COICOP 2 digits) ===")

bdf_path <- file.path("BDF 2017", "Csv")
loc <- locale(decimal_mark = ".", grouping_mark = "")

# --- 1a. Lecture des fichiers ---
bdf_menage <- read_delim(
  file.path(bdf_path, "MENAGE.csv"), delim = ";", locale = loc,
  col_select = c(IDENT_MEN, TYPMEN5, DNIVIE1, TUU, COEFFUC, REVDISP),
  show_col_types = FALSE
)

bdf_depmen <- read_delim(
  file.path(bdf_path, "DEPMEN.csv"), delim = ";", locale = loc,
  col_select = c(IDENT_MEN, Stalog),
  show_col_types = FALSE
)

bdf_conso <- read_delim(
  file.path(bdf_path, "C05.csv"), delim = ";", locale = loc,
  show_col_types = FALSE
)

bdf <- bdf_menage |>
  left_join(bdf_depmen, by = "IDENT_MEN") |>
  left_join(bdf_conso, by = "IDENT_MEN")

message(glue("  {nrow(bdf)} ménages chargés"))

# --- 1b. Agrégation COICOP 2 digits (C01 à C12) ---
# Colonnes COICOP détaillées dans C05.csv : C01111 ... C14xxx
cols_conso <- names(bdf)[grepl("^C[0-9]{5}$", names(bdf))]

coicop_divisions <- sprintf("C%02d", 1:12)  # C01 à C12

# Pour chaque division, sommer les colonnes qui commencent par ce préfixe
for (div in coicop_divisions) {
  cols_div <- cols_conso[grepl(paste0("^", div), cols_conso)]
  if (length(cols_div) > 0) {
    bdf[[div]] <- rowSums(bdf[, cols_div], na.rm = TRUE)
  } else {
    bdf[[div]] <- 0
  }
}

message(glue("  COICOP 2 digits créés : {paste(coicop_divisions, collapse=', ')}"))

# --- 1c. Variables de stratification ---
bdf <- bdf |>
  mutate(
    quintile = case_when(
      DNIVIE1 %in% 1:2  ~ 1L,
      DNIVIE1 %in% 3:4  ~ 2L,
      DNIVIE1 %in% 5:6  ~ 3L,
      DNIVIE1 %in% 7:8  ~ 4L,
      DNIVIE1 %in% 9:10 ~ 5L
    ),
    typmen  = as.integer(TYPMEN5),
    tuu3    = case_when(
      TUU == 0             ~ 1L,  # Rural
      TUU >= 1 & TUU <= 7  ~ 2L,  # Urbain hors Paris
      TUU == 8             ~ 3L   # Paris
    ),
    statut3 = case_when(
      Stalog %in% 1:3 ~ 1L,  # Propriétaire
      Stalog %in% 4:5 ~ 2L,  # Locataire
      Stalog == 6     ~ 3L   # Logé gratuitement
    )
  )

# --- 1d. Filtrage ---
bdf <- bdf |> filter(REVDISP > 0)
message(glue("  {nrow(bdf)} ménages avec REVDISP > 0"))

# --- 1e. Taux de consommation par strate (RATIO DES MOYENNES pondérées) ---
# tx = Σ(conso × poids) / Σ(revdisp × poids)

# Strate complète (225 cellules) : quintile × typmen × tuu3 × statut3
calc_taux <- function(data, group_vars) {
  data |>
    filter(!is.na(quintile), !is.na(typmen), !is.na(tuu3)) |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n_menages = n(),
      across(all_of(coicop_divisions),
             ~ sum(.x * pondmen, na.rm = TRUE) / sum(REVDISP * pondmen, na.rm = TRUE),
             .names = "tx_{.col}"),
      .groups = "drop"
    )
}

taux_225 <- calc_taux(
  bdf |> filter(!is.na(statut3)),
  c("quintile", "typmen", "tuu3", "statut3")
)

taux_75 <- calc_taux(bdf, c("quintile", "typmen", "tuu3"))

message(glue("  Strate 225 : {nrow(taux_225)} cellules ({sum(taux_225$n_menages < 10)} avec < 10 ménages)"))
message(glue("  Strate 75  : {nrow(taux_75)} cellules ({sum(taux_75$n_menages < 10)} avec < 10 ménages)"))

# Vérification globale
revdisp_total <- sum(bdf$REVDISP * bdf$pondmen, na.rm = TRUE)
message("\n  Taux de consommation moyens (ratio des moyennes, ensemble) :")
for (div in coicop_divisions) {
  tx <- sum(bdf[[div]] * bdf$pondmen, na.rm = TRUE) / revdisp_total
  message(glue("    {div} : {round(100 * tx, 1)} %"))
}
tx_total <- sum(rowSums(bdf[, coicop_divisions]) * bdf$pondmen, na.rm = TRUE) / revdisp_total
message(glue("    TOTAL : {round(100 * tx_total, 1)} %"))

# ==============================================================================
# ÉTAPE 2 : Table IPC par COICOP (base 2015 = 100), moyennes annuelles
# ==============================================================================
message("\n=== Étape 2 : IPC par COICOP 2 digits (INSEE, base 2015=100) ===")

# Source : INSEE BDM, indices annuels des prix à la consommation
# Ensemble des ménages, France
ipc <- tribble(
  ~coicop, ~`2017`, ~`2018`, ~`2019`, ~`2020`, ~`2021`, ~`2022`, ~`2023`,
  "C01",   101.65,  103.65,  106.19,  108.33,  109.00,  116.93,  131.38,
  "C02",   102.10,  109.90,  117.22,  126.07,  130.27,  131.99,  142.14,
  "C03",   100.25,  100.41,  100.16,   99.71,   99.87,  102.56,  105.29,
  "C04",   101.48,  103.90,  105.46,  105.24,  109.17,  119.34,  125.78,
  "C05",    99.64,   99.88,  100.05,  100.67,  102.11,  107.92,  114.95,
  "C06",    98.65,   98.26,   97.18,   96.67,   95.19,   94.54,   94.13,
  "C07",   102.01,  106.32,  107.43,  105.24,  109.95,  120.79,  125.94,
  "C08",    96.49,   94.53,   92.06,   91.96,   93.85,   93.97,   90.46,
  "C09",   101.28,  102.06,  102.57,  103.29,  104.42,  106.27,  107.42,
  "C10",   102.50,  103.52,  105.33,  107.42,  109.76,  112.28,  115.99,
  "C11",   103.37,  105.60,  107.10,  108.04,  108.95,  114.02,  119.78,
  "C12",   102.15,  103.31,  104.50,  105.68,  106.79,  110.67,  115.06
)

# Pivot long et calcul du ratio par rapport à 2018 (année de base ERFS)
ipc_long <- ipc |>
  pivot_longer(-coicop, names_to = "annee", values_to = "indice") |>
  mutate(annee = as.integer(annee))

ipc_base <- ipc_long |>
  left_join(
    ipc_long |> filter(annee == 2018) |> select(coicop, indice_base = indice),
    by = "coicop"
  ) |>
  mutate(ratio_ipc = indice / indice_base) |>
  select(coicop, annee, ratio_ipc)

message("  IPC chargé pour C01-C12, 2017-2023 (base = 2018)")
message("  Ratio IPC 2023/2018 :")
for (div in coicop_divisions) {
  r <- ipc_base |> filter(coicop == div, annee == 2023) |> pull(ratio_ipc)
  message(glue("    {div} : {round(r, 3)}"))
}

# ==============================================================================
# ÉTAPE 3 : Imputation dans ERFS 2018-2023 (base = ERFS 2018)
# ==============================================================================
message("\n=== Étape 3 : Imputation dans ERFS 2018-2023 (base ERFS 2018) ===")

# Colonnes de taux
tx_cols <- paste0("tx_", coicop_divisions)

# --- Fonction pour extraire et harmoniser les variables de strate ERFS ---
construire_strates_erfs <- function(df, annee) {
  noms <- tolower(names(df))
  names(df) <- noms

  # --- Identifiant ménage ---
  ident_col <- grep(paste0("^ident", substr(annee, 3, 4), "$|^ident$"), noms, value = TRUE)
  if (length(ident_col) == 0) ident_col <- grep("^ident", noms, value = TRUE)[1]
  ident_val <- as.character(df[[ident_col]])

  # --- nivviem, revdispm, nb_uci, wprm ---
  nivviem  <- as.numeric(df$nivviem)
  wprm     <- as.numeric(df$wprm)
  revdispm <- as.numeric(df$revdispm)
  nb_uci   <- as.numeric(df$nb_uci)

  # --- Quintile de niveau de vie ---
  breaks <- Hmisc::wtd.quantile(nivviem[nivviem > 0 & !is.na(nivviem)],
                                  weights = wprm[nivviem > 0 & !is.na(nivviem)],
                                  probs = seq(0, 1, 0.2))
  quintile_val <- as.integer(cut(nivviem, breaks = breaks, include.lowest = TRUE, labels = FALSE))

  # --- typmen5 ---
  # 2017-2020 : typmen7 (valeurs 1-4, 5, 6, 9 → recodé en 5 modalités)
  # 2021-2023 : typlog5 (valeurs 1-5, directement utilisable)
  if ("typlog5" %in% noms) {
    typmen5_val <- as.integer(as.numeric(df$typlog5))
  } else if ("typmen5" %in% noms) {
    typmen5_val <- as.integer(as.numeric(df$typmen5))
  } else if ("typmen7" %in% noms) {
    tm7 <- as.integer(as.numeric(df$typmen7))
    typmen5_val <- ifelse(tm7 %in% c(5, 6, 9), 5L, tm7)
  } else {
    warning(glue("  {annee} : ni typmen5, ni typmen7, ni typlog5 trouvé !"))
    typmen5_val <- NA_integer_
  }

  # --- tuu3 ---
  # 2017 : tuu2010r.x (suffixe du merge fpr_menage/fpr_mrf)
  # 2018-2020 : tuu2010r (valeurs 1-5)
  # 2021-2023 : tuu2020 (valeurs 0-8)
  if ("tuu2010r.x" %in% noms) {
    tuu_raw <- as.integer(as.numeric(df[["tuu2010r.x"]]))
    tuu3_val <- case_when(
      tuu_raw == 1       ~ 1L,
      tuu_raw %in% 2:4   ~ 2L,
      tuu_raw == 5       ~ 3L
    )
  } else if ("tuu2010r" %in% noms) {
    tuu_raw <- as.integer(as.numeric(df$tuu2010r))
    tuu3_val <- case_when(
      tuu_raw == 1       ~ 1L,
      tuu_raw %in% 2:4   ~ 2L,
      tuu_raw == 5       ~ 3L
    )
  } else if ("tuu2020" %in% noms) {
    tuu_raw <- as.integer(as.numeric(df$tuu2020))
    # tuu2020 : 0=rural, 1-7=urbain, 8=Paris
    tuu3_val <- case_when(
      tuu_raw == 0       ~ 1L,
      tuu_raw %in% 1:7   ~ 2L,
      tuu_raw == 8       ~ 3L
    )
  } else {
    warning(glue("  {annee} : aucune variable tuu trouvée !"))
    tuu3_val <- NA_integer_
  }

  # --- statut3 (statut d'occupation) ---
  # 2017 : so (21% coverage seulement)
  # 2018+ : logt (1-6)
  if ("logt" %in% noms) {
    logt_raw <- as.integer(as.numeric(df$logt))
    statut3_val <- case_when(
      logt_raw %in% 1:3 ~ 1L,  # Propriétaire
      logt_raw %in% 4:5 ~ 2L,  # Locataire
      logt_raw == 6     ~ 3L   # Logé gratuitement
    )
  } else if ("so" %in% noms) {
    so_raw <- suppressWarnings(as.integer(as.numeric(df$so)))
    statut3_val <- case_when(
      so_raw %in% 1:3 ~ 1L,
      so_raw %in% 4:5 ~ 2L,
      so_raw == 6     ~ 3L
    )
  } else {
    statut3_val <- NA_integer_
  }

  tibble(
    ident    = ident_val,
    annee    = annee,
    wprm     = wprm,
    nivviem  = nivviem,
    revdispm = revdispm,
    nb_uci   = nb_uci,
    quintile = quintile_val,
    typmen   = typmen5_val,
    tuu3     = tuu3_val,
    statut3  = statut3_val
  )
}

# --- Boucle sur les années ---
resultats_annuels <- list()

for (annee in 2018:2023) {
  message(glue("\n  --- ERFS {annee} ---"))
  nom <- paste0("menage", annee)
  if (!nom %in% names(erfs)) {
    message(glue("    ERFS {annee} non disponible, skip"))
    next
  }

  df <- erfs[[nom]]
  names(df) <- tolower(names(df))

  # Filtrage champ standard ERFS
  if ("champ_calcul" %in% names(df)) {
    df <- df[as.integer(df$champ_calcul) == 1L, ]
  }

  # Construire les strates
  erfs_annee <- construire_strates_erfs(df, annee)

  # Filtrer nivviem <= 0
  n_avant <- nrow(erfs_annee)
  erfs_annee <- erfs_annee |> filter(nivviem > 0)
  message(glue("    {nrow(erfs_annee)} ménages (nivviem > 0, {n_avant - nrow(erfs_annee)} exclus)"))

  # SO coverage
  n_so <- sum(!is.na(erfs_annee$statut3))
  pct_so <- round(100 * n_so / nrow(erfs_annee), 1)
  message(glue("    SO/logt disponible : {n_so} / {nrow(erfs_annee)} ({pct_so} %)"))

  # --- Jointure avec taux BDF (strate 225 puis fallback 75) ---
  erfs_annee <- erfs_annee |>
    left_join(taux_225, by = c("quintile", "typmen", "tuu3", "statut3"),
              suffix = c("", "_225"))

  idx_na <- is.na(erfs_annee$tx_C01)
  if (sum(idx_na) > 0) {
    message(glue("    {sum(idx_na)} non appariés strate 225 → fallback 75"))

    cols_to_drop <- c("n_menages", tx_cols)
    cols_to_drop <- cols_to_drop[cols_to_drop %in% names(erfs_annee)]

    fallback <- erfs_annee |>
      filter(idx_na) |>
      select(-all_of(cols_to_drop)) |>
      left_join(taux_75, by = c("quintile", "typmen", "tuu3"),
                suffix = c("", "_75"))

    erfs_annee <- bind_rows(
      erfs_annee |> filter(!idx_na),
      fallback
    )
  }

  n_apparies <- sum(!is.na(erfs_annee$tx_C01))
  message(glue("    Appariement final : {n_apparies} / {nrow(erfs_annee)} ({round(100*n_apparies/nrow(erfs_annee),1)} %)"))

  # --- Indexation IPC et calcul des consommations ---
  # conso_CXX_t = tx_CXX × nivviem × (IPC_CXX_t / IPC_CXX_2018)
  # En 2018 (année de base), ratio_ipc = 1, donc conso = tx × nivviem
  for (div in coicop_divisions) {
    ratio <- ipc_base |>
      filter(coicop == div, annee == !!annee) |>
      pull(ratio_ipc)
    if (length(ratio) == 0) ratio <- 1

    tx_col <- paste0("tx_", div)
    conso_col <- paste0("conso_", div)
    erfs_annee[[conso_col]] <- erfs_annee[[tx_col]] * erfs_annee$nivviem * ratio
  }

  # --- Agrégation en grandes catégories ---
  erfs_annee <- erfs_annee |>
    mutate(
      conso_alimentation = conso_C01 + conso_C02,
      conso_logement     = conso_C04,
      conso_energie      = 0,  # sera recalculé ci-dessous
      conso_transport    = conso_C07,
      conso_sante        = conso_C06,
      conso_total        = rowSums(pick(starts_with("conso_C")), na.rm = TRUE),
      # Consommation contrainte = alimentation + logement + énergie (partie de C04 + C07 carburants)
      # Pour l'instant : contrainte = C01 + C02 + C04 (inclut énergie logement)
      conso_contrainte   = conso_C01 + conso_C02 + conso_C04,
      reste_a_vivre      = nivviem - conso_contrainte
    )

  # --- Statistiques résumées ---
  stats <- erfs_annee |>
    filter(!is.na(reste_a_vivre)) |>
    summarise(
      nivvie_moy       = weighted.mean(nivviem, wprm),
      conso_total_moy  = weighted.mean(conso_total, wprm),
      conso_contr_moy  = weighted.mean(conso_contrainte, wprm),
      rav_moy          = weighted.mean(reste_a_vivre, wprm),
      part_contrainte  = 100 * weighted.mean(conso_contrainte, wprm) / weighted.mean(nivviem, wprm),
      part_conso_total = 100 * weighted.mean(conso_total, wprm) / weighted.mean(nivviem, wprm)
    )

  message(glue("    Niveau de vie annuel moyen : {format(round(stats$nivvie_moy), big.mark=' ')} €"))
  message(glue("    Conso totale moyenne       : {format(round(stats$conso_total_moy), big.mark=' ')} € ({round(stats$part_conso_total,1)} % du niv. vie)"))
  message(glue("    Conso contrainte moyenne   : {format(round(stats$conso_contr_moy), big.mark=' ')} € ({round(stats$part_contrainte,1)} % du niv. vie)"))
  message(glue("    Reste à vivre moyen        : {format(round(stats$rav_moy), big.mark=' ')} €"))

  # Détail par COICOP
  message("    Détail par COICOP :")
  for (div in coicop_divisions) {
    conso_col <- paste0("conso_", div)
    moy <- weighted.mean(erfs_annee[[conso_col]], erfs_annee$wprm, na.rm = TRUE)
    pct <- 100 * moy / stats$nivvie_moy
    message(glue("      {div} : {format(round(moy), big.mark=' ')} € ({round(pct,1)} %)"))
  }

  resultats_annuels[[as.character(annee)]] <- erfs_annee
}

# ==============================================================================
# ÉTAPE 4 : Synthèse 2018-2023
# ==============================================================================
message("\n=== Étape 4 : Synthèse 2018-2023 ===")

synthese <- map_dfr(names(resultats_annuels), function(a) {
  df <- resultats_annuels[[a]]
  if (is.null(df)) return(NULL)
  df |>
    filter(!is.na(reste_a_vivre)) |>
    summarise(
      annee = as.integer(a),
      nivvie_moy      = weighted.mean(nivviem, wprm),
      conso_total_moy = weighted.mean(conso_total, wprm),
      conso_contr_moy = weighted.mean(conso_contrainte, wprm),
      rav_moy         = weighted.mean(reste_a_vivre, wprm),
      across(all_of(paste0("conso_", coicop_divisions)),
             ~ weighted.mean(.x, wprm, na.rm = TRUE),
             .names = "{.col}_moy")
    )
})

message("\n  Tableau synthétique :")
message(sprintf("  %s | %10s | %10s | %10s | %10s | %7s",
                "Année", "Niv. vie", "Conso tot", "Contrainte", "RAV", "% contr"))
message(paste(rep("-", 80), collapse = ""))
for (i in seq_len(nrow(synthese))) {
  r <- synthese[i, ]
  pct <- round(100 * r$conso_contr_moy / r$nivvie_moy, 1)
  message(sprintf("  %d  | %10s | %10s | %10s | %10s | %6.1f %%",
                  r$annee,
                  format(round(r$nivvie_moy), big.mark = " "),
                  format(round(r$conso_total_moy), big.mark = " "),
                  format(round(r$conso_contr_moy), big.mark = " "),
                  format(round(r$rav_moy), big.mark = " "),
                  pct))
}

message("\n=== reste_a_vivre.R terminé — lancer graphs_reste_a_vivre.R pour les graphiques ===")
