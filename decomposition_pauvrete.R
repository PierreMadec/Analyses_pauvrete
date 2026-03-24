library(tidyverse)
library(ggiraph)

# ==============================================================================
# Paramètres macro : IPC, RSA socle, SMIC net
# ==============================================================================
params_macro <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
ipc       <- setNames(params_macro$ipc,       as.character(params_macro$annee))
rsa_socle <- setNames(params_macro$rsa_socle,  as.character(params_macro$annee))
smic_net  <- setNames(params_macro$smic_net,   as.character(params_macro$annee))

# ==============================================================================
# Fonctions utilitaires
# ==============================================================================

mediane_ponderee <- function(x, w) {
  tibble(x = x, w = w) |>
    filter(!is.na(x), !is.na(w), w > 0) |>
    arrange(x) |>
    mutate(cum_w = cumsum(w)) |>
    filter(cum_w >= sum(w) / 2) |>
    slice(1) |>
    pull(x)
}

get_first_col <- function(df, candidats) {
  col <- intersect(candidats, names(df))
  if (length(col) > 0) return(df[[col[1]]])
  NULL
}

recode_activite <- function(raw, nomenclature = "acteu6") {
  val <- suppressWarnings(as.integer(as.character(raw)))
  if (nomenclature == "acteu3") return(val)
  case_when(val == 1 ~ 1L, val == 3 ~ 2L, val %in% c(4, 5, 6) ~ 3L, TRUE ~ NA_integer_)
}

recode_statut_occ <- function(raw, nomenclature = "so") {
  val <- suppressWarnings(as.integer(as.numeric(raw)))
  if (nomenclature == "duhab") {
    return(case_when(
      val == 1          ~ "Proprietaire",
      val %in% c(2,3,4) ~ "Locataire",
      val %in% c(5,6)   ~ "Loge gratuitement",
      TRUE ~ NA_character_
    ))
  }
  case_when(
    val %in% 1:3 ~ "Proprietaire",
    val %in% 4:5 ~ "Locataire",
    val == 6     ~ "Loge gratuitement",
    TRUE ~ NA_character_
  )
}

recode_typmen <- function(df) {
  if ("typmen5"  %in% names(df)) return(suppressWarnings(as.integer(as.character(df$typmen5))))
  if ("typlog5"  %in% names(df)) return(suppressWarnings(as.integer(as.character(df$typlog5))))
  if ("typmen7"  %in% names(df)) {
    val <- suppressWarnings(as.integer(as.character(df$typmen7)))
    return(ifelse(val %in% c(5, 6, 9), 5L, val))
  }
  if ("typmen21" %in% names(df)) {
    val <- suppressWarnings(as.integer(as.character(df$typmen21)))
    return(case_when(
      val %in% 11:12 ~ 1L, val %in% 21:23 ~ 2L,
      val %in% 31:33 ~ 3L, val %in% 41:44 ~ 4L, TRUE ~ 5L
    ))
  }
  rep(NA_integer_, nrow(df))
}

detecter_et_recoder_activite <- function(df, candidats_acteu3, candidats_acteu6) {
  col3 <- get_first_col(df, candidats_acteu3)
  if (!is.null(col3)) return(recode_activite(col3, "acteu3"))
  col6 <- get_first_col(df, candidats_acteu6)
  if (!is.null(col6)) return(recode_activite(col6, "acteu6"))
  rep(NA_integer_, nrow(df))
}

recode_pcs <- function(df) {
  # Harmonisation PCS : pcs1q (2023+, PCS 2020), puis csp (2015-2020, code 2 chiffres
  # dont le premier = CS1), puis css2 (fallback sparse)
  for (col in c("css2", "pcs1", "pcs1q", "csp")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
    df[[col]] <- as.character(df[[col]])
  }
  # Helper : premier caractère → catégorie
  cs1_to_cat <- function(x) {
    case_when(
      substr(x, 1, 1) == "1" ~ "Agriculteurs exploitants",
      substr(x, 1, 1) == "2" ~ "Artisans, commercants, chefs d'entreprise",
      substr(x, 1, 1) == "3" ~ "Cadres et prof. intellectuelles sup.",
      substr(x, 1, 1) == "4" ~ "Professions intermediaires",
      substr(x, 1, 1) == "5" ~ "Employes",
      substr(x, 1, 1) == "6" ~ "Ouvriers",
      TRUE ~ NA_character_
    )
  }
  case_when(
    # Priorité 1 : pcs1q (ERFS 2023+, PCS 2020)
    !is.na(df$pcs1q) & df$pcs1q != "" ~ cs1_to_cat(df$pcs1q),
    # Priorité 2 : csp (ERFS 2015-2020 dans IRF, ~40 % des obs)
    !is.na(df$csp)   & df$csp   != "" ~ cs1_to_cat(df$csp),
    # Priorité 3 : css2 (fallback, très peu d'obs)
    !is.na(df$css2)  & df$css2  != "" ~ cs1_to_cat(df$css2),
    TRUE ~ NA_character_
  )
}

# ==============================================================================
# Fonction principale : construire la base individus enrichie pour une année
#
# Architecture : une ligne par individu.
#   - Variables ménage (nivviem, prestations, typmen, acteu_pr/cj, etc.)
#     jointes sur l'identifiant ménage (ident{YY}).
#   - Variables individuelles (age, sexe, diplome, acteu_ind, contrat, PCS)
#     lues directement dans la base individus.
#
# Pondération :
#   Chaque individu hérite du poids wprm du ménage. La somme des wprm
#   individuels est proportionnelle au nombre d'individus par ménage,
#   ce qui rapproche le calcul du taux de pauvreté de la définition
#   individuelle officielle (proportion d'individus dans un ménage pauvre),
#   par opposition à la proportion de ménages pauvres.
#
# Repondération (effet structure) :
#   Les multiplicateurs de repondération sont calculés au niveau ménage
#   (distinct(ident, .keep_all=TRUE)) et joints sur les individus.
#   Tous les membres d'un même ménage reçoivent le même multiplicateur.
# ==============================================================================

construire_base_individus <- function(annee) {
  an       <- as.character(annee)
  an_short <- substr(an, 3, 4)
  cle      <- paste0("ident", an_short)
  nom_men  <- paste0("menage",    annee)
  nom_ind  <- paste0("individus", annee)

  if (!nom_men %in% names(erfs) || !nom_ind %in% names(erfs)) {
    message(sprintf("  [%d] menage ou individus manquant — annee ignoree", annee))
    return(NULL)
  }

  men <- erfs[[nom_men]]
  ind <- erfs[[nom_ind]]
  names(men) <- tolower(names(men))
  men <- men[, !duplicated(names(men))]
  names(ind) <- tolower(names(ind))
  ind <- ind[, !duplicated(names(ind))]

  # --- Champ standard ERFS ---
  if ("champ_calcul" %in% names(men)) {
    men <- men[as.integer(men$champ_calcul) == 1L, ]
  }

  # --- Normaliser la clé de jointure dans ind (ex : ident05a -> ident05) ---
  cle_ind <- grep(paste0("^", cle), names(ind), value = TRUE)[1]
  if (!is.na(cle_ind) && cle_ind != cle) {
    ind <- ind |> rename(!!cle := !!cle_ind)
  }
  if (!cle %in% names(ind)) {
    message(sprintf("  [%d] cle '%s' absente dans individus — annee ignoree", annee, cle))
    return(NULL)
  }

  # --------------------------------------------------------------------------
  # Listes de variables ménage à récupérer
  # --------------------------------------------------------------------------
  vars_prest <- c(
    "prest_logement", "prest_fam_petite_enfance", "prest_fam_autres",
    "prest_precarite_hand", "prest_precarite_vieil",
    "prest_precarite_rsa", "prest_precarite_rmi", "prest_precarite_api_rmi",
    "m_rsa_actm", "ppa", "prest_aide_solidarite", "cej"
  )
  vars_impots <- c(
    "irpp",
    "csgsaldm", "csgchodm", "csgrstdm", "csgpidm",
    "csgragdm", "csgricdm", "csgrncdm",
    "csgpatm", "csgvalm", "csgimpm",
    "csg_crds_nondeductible_salaires", "csg_crds_nondeductible_chomage",
    "csg_crds_nondeductible_retraites", "csg_crds_nondeductible_invalid",
    "csg_crds_nondeductible_rag", "csg_crds_nondeductible_rnc",
    "csg_crds_nondeductible_ric",
    "crds_psoc", "crds_logt", "crds_minim",
    "impot_plus_value", "rev_financier_prelev_lib_imputes"
  )
  vars_men_sel <- unique(c(
    cle, "wprm", "nivviem", "revdispm", "nb_uci",
    intersect(vars_prest,                          names(men)),
    intersect(vars_impots,                         names(men)),
    intersect(c("typmen5","typlog5","typmen7","typmen21"), names(men)),
    intersect(c("acteuprl","acteu6prm","acteu6pr"), names(men)),
    intersect(c("acteuprlcj","acteu6prmcj","acteu6cj"), names(men)),
    intersect(c("nbenfa18","nbenflog","nbenflogchamp"), names(men)),
    intersect(c("logt","so"),                      names(men))
  ))

  # --------------------------------------------------------------------------
  # Listes de variables individus à récupérer
  # --------------------------------------------------------------------------
  vars_ind_cand <- c(
    cle, "lpr", "lprm",
    "acteu",                              # activite individuelle (3 cat.)
    "age", "sexe", "immi",
    "dip5", "dip3",                       # diplome
    "duhab",                              # statut occ. (2013-2014)
    "cdd", "cdi", "interim", "contra",   # type de contrat (contra: 2015-2020, cdd/cdi/interim: 2023+)
    "tpstrav", "sousemplr",              # temps partiel / sous-emploi (2015+)
    "nbhab_usual_emp1",                  # heures habituelles (2015+)
    "css2", "pcs1", "pcs1q", "csp"      # PCS (csp: 2015-2020 dans IRF, pcs1q: 2023+)
  )
  vars_ind_sel <- intersect(vars_ind_cand, names(ind))

  # Supprimer de ind les colonnes qui seront récupérées du ménage
  cols_doublon <- setdiff(intersect(names(ind), vars_men_sel), cle)
  if (length(cols_doublon) > 0) ind <- ind |> select(-all_of(cols_doublon))

  # --------------------------------------------------------------------------
  # Jointure individus ← ménage
  # --------------------------------------------------------------------------
  df <- ind |>
    select(all_of(intersect(vars_ind_sel, names(ind)))) |>
    left_join(men |> select(all_of(vars_men_sel)), by = cle)

  # Exclure les individus sans correspondance ménage (hors champ)
  df <- df |> filter(!is.na(nivviem), !is.na(wprm))
  if (nrow(df) == 0) return(NULL)

  # --------------------------------------------------------------------------
  # Variables MÉNAGE harmonisées
  # --------------------------------------------------------------------------

  # Prestations
  prest_ok      <- intersect(vars_prest, names(df))
  df$prest_logement <- if ("prest_logement" %in% names(df))
    suppressWarnings(as.numeric(df$prest_logement)) else 0
  df$total_prestations <- rowSums(
    df[, prest_ok, drop = FALSE] |> mutate(across(everything(), as.numeric)),
    na.rm = TRUE
  )
  for (.v in c("ppa", "m_rsa_actm",
               "prest_fam_petite_enfance", "prest_fam_autres",
               "prest_precarite_rsa", "prest_precarite_rmi",
               "prest_precarite_api_rmi")) {
    df[[.v]] <- if (.v %in% names(df)) suppressWarnings(as.numeric(df[[.v]])) else 0
  }

  # Impôts
  impots_ok <- intersect(vars_impots, names(df))
  df$total_impots <- rowSums(
    df[, impots_ok, drop = FALSE] |> mutate(across(everything(), as.numeric)),
    na.rm = TRUE
  )

  # Revenus avant redistribution
  df$rev_avant_redist <- df$revdispm - df$total_prestations + df$total_impots
  df$nivviem_hors_apl <- (df$revdispm - df$prest_logement) / df$nb_uci
  df$nivvie_avant     <- df$rev_avant_redist / df$nb_uci

  # Type de ménage
  df$typmen <- factor(
    recode_typmen(df), levels = 1:5,
    labels = c("Personne seule", "Couple sans enfant",
               "Couple avec enfant(s)", "Famille monoparentale", "Autre")
  )

  # Activité PR (ménage)
  df$acteu_pr <- factor(
    detecter_et_recoder_activite(
      df, candidats_acteu3 = "acteuprl",
      candidats_acteu6 = c("acteu6prm", "acteu6pr")
    ),
    levels = 1:3, labels = c("Emploi", "Chomage", "Inactif")
  )

  # Activité CJ (ménage) — utilisée pour l'OB global
  df$acteu_cj <- factor(
    detecter_et_recoder_activite(
      df, candidats_acteu3 = "acteuprlcj",
      candidats_acteu6 = c("acteu6prmcj", "acteu6cj")
    ),
    levels = 1:3, labels = c("Emploi", "Chomage", "Inactif")
  )

  # Bi-activité (ménage) — groupe de repondération pour la décomposition séquentielle
  # Structure : typmen × biactivite (capture mieux les dynamiques d'emploi de couple
  # que le seul acteu_pr, car on distingue bi-actifs, mono-actifs et sans emploi)
  df$biactivite <- factor(
    case_when(
      as.integer(df$acteu_pr) == 1L & df$acteu_cj == "Emploi"                    ~ "Bi-actif",
      as.integer(df$acteu_pr) == 1L & (is.na(df$acteu_cj) | df$acteu_cj != "Emploi") ~ "Mono-actif",
      TRUE                                                                          ~ "Sans emploi"
    ),
    levels = c("Bi-actif", "Mono-actif", "Sans emploi")
  )

  # Groupe de repondération : typmen × biactivite (calculé au niveau ménage)
  df$groupe <- interaction(df$typmen, df$biactivite, drop = TRUE, sep = " / ")

  # Nombre d'enfants (variable ménage catégorisée)
  nbenf_col <- get_first_col(df, c("nbenfa18", "nbenflog", "nbenflogchamp"))
  nbenf_raw <- if (!is.null(nbenf_col))
    suppressWarnings(as.numeric(as.character(nbenf_col)))
  else rep(NA_real_, nrow(df))
  df$nb_enfants <- factor(
    case_when(
      nbenf_raw == 0 ~ "0 enfant",  nbenf_raw == 1 ~ "1 enfant",
      nbenf_raw == 2 ~ "2 enfants", nbenf_raw >= 3 ~ "3+ enfants",
      TRUE ~ NA_character_
    ),
    levels = c("0 enfant", "1 enfant", "2 enfants", "3+ enfants")
  )

  # Type de ménage croisé avec nombre d'enfants (pour OB sans colinéarité)
  df$typmen2 <- factor(case_when(
    df$typmen == "Personne seule"         ~ "Personne seule",
    df$typmen == "Couple sans enfant"     ~ "Couple sans enfant",
    df$typmen == "Couple avec enfant(s)" & nbenf_raw == 1 ~ "Couple 1 enfant",
    df$typmen == "Couple avec enfant(s)" & nbenf_raw == 2 ~ "Couple 2 enfants",
    df$typmen == "Couple avec enfant(s)" & nbenf_raw >= 3 ~ "Couple 3+ enfants",
    df$typmen == "Famille monoparentale" & nbenf_raw == 1 ~ "Mono 1 enfant",
    df$typmen == "Famille monoparentale" & nbenf_raw >= 2 ~ "Mono 2+ enfants",
    TRUE ~ "Autre"
  ), levels = c("Personne seule", "Couple sans enfant",
                "Couple 1 enfant", "Couple 2 enfants", "Couple 3+ enfants",
                "Mono 1 enfant", "Mono 2+ enfants", "Autre"))

  # Statut d'occupation du logement (ménage)
  # 2013-2014 : DUHAB dans individus, diffusé à tout le ménage via PR (lpr==1)
  df$statut_occ <- NA_character_
  if (annee >= 2015) {
    raw_occ <- get_first_col(df, c("logt", "so"))
    if (!is.null(raw_occ)) df$statut_occ <- recode_statut_occ(raw_occ, "so")
  } else if (annee %in% 2013:2014) {
    lpr_col <- get_first_col(df, c("lpr", "lprm"))
    if (!is.null(lpr_col) && "duhab" %in% names(df)) {
      lpr_int <- suppressWarnings(as.integer(as.character(lpr_col)))
      pr_duhab <- df[lpr_int == 1L & !is.na(lpr_int), ] |>
        select(!!cle, duhab_pr = duhab)
      df <- df |>
        left_join(pr_duhab, by = cle) |>
        mutate(statut_occ = recode_statut_occ(duhab_pr, "duhab")) |>
        select(-duhab_pr)
    }
    if (all(is.na(df$statut_occ))) {
      raw_occ <- get_first_col(df, "so")
      if (!is.null(raw_occ)) df$statut_occ <- recode_statut_occ(raw_occ, "so")
    }
  } else {
    raw_occ <- get_first_col(df, "so")
    if (!is.null(raw_occ)) df$statut_occ <- recode_statut_occ(raw_occ, "so")
  }

  # --------------------------------------------------------------------------
  # Variables INDIVIDUELLES harmonisées
  # --------------------------------------------------------------------------

  # Lien à la personne de référence (1 = PR, 2 = conjoint, 3 = autre)
  lpr_col <- get_first_col(df, c("lpr", "lprm"))
  df$lpr <- if (!is.null(lpr_col))
    suppressWarnings(as.integer(as.character(lpr_col))) else NA_integer_

  # Activité individuelle (acteu : 1=Emploi, 2=Chômage, 3=Inactif)
  raw_acteu_ind <- get_first_col(df, "acteu")
  df$acteu_ind <- factor(
    if (!is.null(raw_acteu_ind)) recode_activite(raw_acteu_ind, "acteu3")
    else rep(NA_integer_, nrow(df)),
    levels = 1:3, labels = c("Emploi", "Chomage", "Inactif")
  )

  # Âge
  age_raw    <- if ("age" %in% names(df))
    suppressWarnings(as.numeric(as.character(df$age))) else NA_real_
  df$age_num <- age_raw
  df$age_cat <- cut(
    age_raw,
    breaks = c(-Inf, 18, 25, 35, 45, 55, 65, 75, Inf),
    labels = c("< 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
    right  = FALSE
  )

  # Sexe
  df$sexe_cat <- if ("sexe" %in% names(df)) {
    factor(suppressWarnings(as.integer(as.character(df$sexe))),
           levels = 1:2, labels = c("Homme", "Femme"))
  } else NA_character_

  # Immigration
  df$immi_cat <- if ("immi" %in% names(df)) {
    factor(ifelse(
      suppressWarnings(as.integer(as.character(df$immi))) == 1,
      "Immigre", "Non immigre"
    ))
  } else NA_character_

  # Diplôme
  if ("dip5" %in% names(df)) {
    df$diplome <- factor(
      suppressWarnings(as.integer(as.character(df$dip5))), levels = 1:5,
      labels = c("Sans diplome", "CEP/BEPC/Brevet", "CAP/BEP", "Bac", "Superieur au bac")
    )
  } else if ("dip3" %in% names(df)) {
    df$diplome <- factor(
      suppressWarnings(as.integer(as.character(df$dip3))), levels = 1:3,
      labels = c("Inferieur au bac", "Bac", "Superieur au bac")
    )
  } else {
    df$diplome <- NA_character_
  }

  # Type de contrat : cdd/cdi/interim (2023) ou contra (2015-2020 dans IRF)
  for (.col in c("cdd", "cdi", "interim", "contra")) {
    df[[.col]] <- if (.col %in% names(df))
      suppressWarnings(as.numeric(df[[.col]])) else NA_real_
  }
  df$type_contrat <- case_when(
    # Méthode 1 : variables cdd/cdi/interim (ERFS 2023+)
    !is.na(df$cdi)     & df$cdi     == 1 ~ "CDI",
    !is.na(df$cdd)     & df$cdd     == 1 ~ "CDD",
    !is.na(df$interim) & df$interim == 1 ~ "Interim",
    # Méthode 2 : variable contra (ERFS 2015-2020, fichier IRF)
    !is.na(df$contra)  & df$contra  == 1 ~ "CDI",
    !is.na(df$contra)  & df$contra  == 2 ~ "CDD",
    !is.na(df$contra)  & df$contra  == 3 ~ "Interim",
    TRUE ~ NA_character_
  )

  # Temps partiel / sous-emploi (2015+)
  df$tpstrav   <- if ("tpstrav"   %in% names(df))
    suppressWarnings(as.numeric(df$tpstrav))   else NA_real_
  df$sousemplr <- if ("sousemplr" %in% names(df))
    suppressWarnings(as.numeric(df$sousemplr)) else NA_real_

  # PCS harmonisée 2015-2023
  df$pcs_cat <- recode_pcs(df)

  # --------------------------------------------------------------------------
  df$annee <- annee
  df$ident  <- as.character(df[[cle]])   # clé ménage conservée pour les opérations ménage

  df |> select(
    annee, ident, lpr, wprm,
    # Niveau de vie ménage
    nivviem, revdispm, nb_uci,
    prest_logement, total_prestations, total_impots,
    rev_avant_redist, nivvie_avant, nivviem_hors_apl,
    # Composantes de prestations détaillées
    ppa, m_rsa_actm,
    prest_fam_petite_enfance, prest_fam_autres,
    prest_precarite_rsa, prest_precarite_rmi, prest_precarite_api_rmi,
    # Variables ménage harmonisées
    typmen, typmen2, acteu_pr, acteu_cj, biactivite, nb_enfants, statut_occ, groupe,
    # Variables individuelles harmonisées
    acteu_ind, age_num, age_cat, sexe_cat, immi_cat, diplome,
    type_contrat, tpstrav, sousemplr, pcs_cat
  )
}

# ==============================================================================
# Construire la base individus pour toutes les années disponibles
# ==============================================================================
annees <- as.integer(names(ipc))
annees <- annees[
  paste0("menage",    annees) %in% names(erfs) &
  paste0("individus", annees) %in% names(erfs)
]

message("Construction de data_all (base individus enrichie)...")
data_all <- map_dfr(annees, construire_base_individus)
message(sprintf("  data_all : %s individus, %d annees",
                format(nrow(data_all), big.mark = " "), n_distinct(data_all$annee)))

# ==============================================================================
# Seuils de référence (calculés sur la distribution individuelle, 2005)
# ==============================================================================
seuil_2005       <- 0.6 * mediane_ponderee(
  data_all$nivviem[data_all$annee == 2005],
  data_all$wprm[data_all$annee == 2005]
)
seuil_avant_2005 <- 0.6 * mediane_ponderee(
  data_all$nivvie_avant[data_all$annee == 2005],
  data_all$wprm[data_all$annee == 2005]
)

# ==============================================================================
# Structures de référence 2005 pour le reweighting (niveau MÉNAGE)
#
# La repondération doit être calculée sur la distribution des MÉNAGES
# (et non des individus) car les groupes (typmen × biactivite) sont des
# caractéristiques de ménage. On déduplique à une obs par ménage via
# distinct(ident, .keep_all=TRUE) avant de calculer les parts de référence.
# ==============================================================================
struct_ref <- data_all |>
  filter(annee == 2005, !is.na(groupe)) |>
  distinct(ident, .keep_all = TRUE) |>
  group_by(groupe) |>
  summarise(poids_ref = sum(wprm), .groups = "drop") |>
  mutate(part_ref = poids_ref / sum(poids_ref))

struct_ref_typmen <- data_all |>
  filter(annee == 2005, !is.na(typmen)) |>
  distinct(ident, .keep_all = TRUE) |>
  group_by(typmen) |>
  summarise(poids_ref = sum(wprm), .groups = "drop") |>
  mutate(part_ref_typmen = poids_ref / sum(poids_ref)) |>
  select(-poids_ref)

# ==============================================================================
# Décomposition séquentielle
#
# Même logique télescopique qu'auparavant, mais sur la base individuelle :
# - Le seuil et les taux sont calculés sur la distribution individuelle
# - La repondération est calculée au niveau ménage (distinct(ident)) puis
#   propagée à tous les individus du ménage via un left_join sur ident
# ==============================================================================
resultats <- data_all |>
  group_by(annee) |>
  group_modify(function(df, grp) {
    an <- as.character(grp$annee)

    seuil_obs        <- 0.6 * mediane_ponderee(df$nivviem,      df$wprm)
    seuil_gele       <- seuil_2005 * ipc[an] / ipc["2005"]
    seuil_avant_gele <- seuil_avant_2005 * ipc[an] / ipc["2005"]

    # --- Taux sur distribution individuelle ---
    taux_obs   <- sum(df$wprm[df$nivviem < seuil_obs],  na.rm = TRUE) / sum(df$wprm)
    taux_gele  <- sum(df$wprm[df$nivviem < seuil_gele], na.rm = TRUE) / sum(df$wprm)
    taux_avant <- sum(df$wprm[df$nivvie_avant <
                       0.6 * mediane_ponderee(df$nivvie_avant, df$wprm)],
                      na.rm = TRUE) / sum(df$wprm)
    taux_avant_gele <- sum(df$wprm[df$nivvie_avant < seuil_avant_gele],
                           na.rm = TRUE) / sum(df$wprm)

    # --- Repondération jointe (typmen × biactivite) ---
    # Multiplicateurs calculés au niveau ménage, joints sur individus
    mult_grp <- df |>
      distinct(ident, .keep_all = TRUE) |>
      filter(!is.na(groupe)) |>
      group_by(groupe) |>
      mutate(
        poids_grp = sum(wprm),
        part_grp  = poids_grp / sum(df$wprm[!is.na(df$groupe)])
      ) |>
      ungroup() |>
      left_join(struct_ref |> select(groupe, part_ref), by = "groupe") |>
      mutate(
        part_ref     = replace_na(part_ref, 0),
        ratio_repond = ifelse(part_grp > 0, part_ref / part_grp, 1)
      ) |>
      select(ident, ratio_repond)

    df_r <- df |>
      left_join(mult_grp, by = "ident") |>
      mutate(ratio_repond = replace_na(ratio_repond, 1),
             wprm_r       = wprm * ratio_repond)

    taux_repond_gele <- sum(df_r$wprm_r[df_r$nivviem < seuil_gele], na.rm = TRUE) /
                        sum(df_r$wprm_r)

    # --- Repondération typmen seul ---
    mult_typ <- df |>
      distinct(ident, .keep_all = TRUE) |>
      filter(!is.na(typmen)) |>
      group_by(typmen) |>
      mutate(part_grp = sum(wprm) / sum(df$wprm[!is.na(df$typmen)])) |>
      ungroup() |>
      left_join(struct_ref_typmen, by = "typmen") |>
      mutate(
        part_ref_typmen = replace_na(part_ref_typmen, 0),
        ratio_typ       = ifelse(part_grp > 0, part_ref_typmen / part_grp, 1)
      ) |>
      select(ident, ratio_typ)

    df_t <- df |>
      left_join(mult_typ, by = "ident") |>
      mutate(ratio_typ = replace_na(ratio_typ, 1),
             wprm_t    = wprm * ratio_typ)

    taux_repond_typmen_gele <- sum(df_t$wprm_t[df_t$nivviem < seuil_gele], na.rm = TRUE) /
                               sum(df_t$wprm_t)

    # --- Avant redistribution, seuil gelé, structure 2005 ---
    taux_avant_gele_repond <- sum(
      df_r$wprm_r[df_r$nivvie_avant < seuil_avant_gele], na.rm = TRUE
    ) / sum(df_r$wprm_r)

    tibble(
      taux_obs               = taux_obs               * 100,
      taux_seuil_gele        = taux_gele              * 100,
      taux_avant_redist      = taux_avant             * 100,
      taux_avant_gele        = taux_avant_gele        * 100,
      taux_avant_gele_repond = taux_avant_gele_repond * 100,
      taux_repond_gele       = taux_repond_gele       * 100,
      taux_repond_typmen_gele = taux_repond_typmen_gele * 100,
      seuil_obs        = seuil_obs,
      seuil_gele       = seuil_gele,
      seuil_avant_gele = seuil_avant_gele
    )
  }) |>
  ungroup()

# --- Décomposition télescopique exacte (résidu = 0 par construction) ---
ref <- resultats |> filter(annee == 2005)

resultats <- resultats |>
  mutate(
    delta_total            = taux_obs - ref$taux_obs,
    effet_seuil            = taux_obs - taux_seuil_gele,
    effet_structure        = taux_seuil_gele - taux_repond_gele,
    effet_structure_typmen = taux_seuil_gele - taux_repond_typmen_gele,
    effet_structure_acteu  = taux_repond_typmen_gele - taux_repond_gele,
    efficacite_redist      = taux_avant_gele_repond - taux_repond_gele,
    efficacite_ref         = ref$taux_avant_gele_repond - ref$taux_repond_gele,
    effet_redistribution   = -(efficacite_redist - efficacite_ref),
    effet_rev_primaires    = taux_avant_gele_repond - ref$taux_avant_gele_repond,
    residu = delta_total - effet_seuil - effet_structure -
             effet_rev_primaires - effet_redistribution
  )

tableau <- resultats |>
  select(annee, taux_obs, delta_total,
         effet_seuil, effet_structure_typmen, effet_structure_acteu,
         effet_rev_primaires, effet_redistribution, residu) |>
  mutate(across(where(is.numeric) & !matches("annee"), ~ round(., 2)))
print(tableau, n = Inf)

# ==============================================================================
# Seuils annuels (réutilisés dans les analyses en aval)
# ==============================================================================
seuils_annuels <- resultats |>
  select(annee, seuil_std = seuil_obs) |>
  left_join(
    data_all |>
      group_by(annee) |>
      summarise(seuil_hapl = 0.6 * mediane_ponderee(nivviem_hors_apl, wprm),
                .groups = "drop"),
    by = "annee"
  )

# ==============================================================================
# Données g3 : RSA / SMIC / Revenu médian (base 100)
# ==============================================================================
g3_data <- tibble(
  annee = as.integer(names(rsa_socle)),
  RSA   = rsa_socle,
  SMIC  = smic_net
) |>
  left_join(
    resultats |> mutate(median_mensuel = seuil_obs / 0.6 / 12) |>
      select(annee, median_mensuel),
    by = "annee"
  ) |>
  rename(`Revenu median` = median_mensuel) |>
  pivot_longer(-annee, names_to = "serie", values_to = "euros") |>
  group_by(serie) |>
  mutate(base100 = euros / euros[annee == 2005] * 100) |>
  ungroup()

# ==============================================================================
# Coût d'éradication et d'intensité de la pauvreté
# NB : sum(nb_uci * wprm) pondère par le nombre d'UC du ménage ; ici on
# travaille au niveau individuel, donc on déduplique pour éviter les doubles
# comptes sur les variables monétaires de ménage.
# ==============================================================================
cout_pauvrete <- data_all |>
  distinct(annee, ident, .keep_all = TRUE) |>     # une obs par ménage
  group_by(annee) |>
  summarise(
    mediane  = mediane_ponderee(nivviem, wprm),
    seuil_50 = 0.5 * mediane,
    seuil_60 = 0.6 * mediane,
    ecart_50 = sum(pmax(seuil_50 - nivviem, 0) * nb_uci * wprm, na.rm = TRUE),
    ecart_60 = sum(pmax(seuil_60 - nivviem, 0) * nb_uci * wprm, na.rm = TRUE),
    nb_menages_pauvres_50 = sum(wprm * (nivviem < seuil_50), na.rm = TRUE),
    nb_menages_pauvres_60 = sum(wprm * (nivviem < seuil_60), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    an              = as.character(annee),
    cout_50_courant = ecart_50 / 1e9,
    cout_60_courant = ecart_60 / 1e9,
    cout_50_2023    = ecart_50 * ipc["2023"] / ipc[an] / 1e9,
    cout_60_2023    = ecart_60 * ipc["2023"] / ipc[an] / 1e9,
    cout_par_menage_50 = ecart_50 / nb_menages_pauvres_50,
    cout_par_menage_60 = ecart_60 / nb_menages_pauvres_60
  ) |>
  select(annee, seuil_50, seuil_60,
         cout_50_courant, cout_60_courant, cout_50_2023, cout_60_2023,
         nb_menages_pauvres_50, nb_menages_pauvres_60,
         cout_par_menage_50, cout_par_menage_60)

saveRDS(cout_pauvrete, file.path("graphiques", "cout_pauvrete.rds"))

cout_intensite <- data_all |>
  distinct(annee, ident, .keep_all = TRUE) |>
  group_by(annee) |>
  summarise(
    mediane      = mediane_ponderee(nivviem, wprm),
    seuil_50     = 0.5 * mediane,
    seuil_60     = 0.6 * mediane,
    intensite_50 = weighted.mean(pmax(seuil_50 - nivviem, 0) / seuil_50,
                                 wprm * (nivviem < seuil_50), na.rm = TRUE) * 100,
    intensite_60 = weighted.mean(pmax(seuil_60 - nivviem, 0) / seuil_60,
                                 wprm * (nivviem < seuil_60), na.rm = TRUE) * 100,
    uc_pauvres_50 = sum(nb_uci * wprm * (nivviem < seuil_50), na.rm = TRUE),
    uc_pauvres_60 = sum(nb_uci * wprm * (nivviem < seuil_60), na.rm = TRUE),
    cout_1pt_50   = 0.01 * seuil_50 * uc_pauvres_50,
    cout_1pt_60   = 0.01 * seuil_60 * uc_pauvres_60,
    .groups = "drop"
  ) |>
  mutate(
    an                  = as.character(annee),
    cout_1pt_50_courant = cout_1pt_50 / 1e9,
    cout_1pt_60_courant = cout_1pt_60 / 1e9,
    cout_1pt_50_2023    = cout_1pt_50 * ipc["2023"] / ipc[an] / 1e9,
    cout_1pt_60_2023    = cout_1pt_60 * ipc["2023"] / ipc[an] / 1e9
  ) |>
  select(annee, intensite_50, intensite_60,
         cout_1pt_50_courant, cout_1pt_60_courant,
         cout_1pt_50_2023, cout_1pt_60_2023)

# ==============================================================================
# Décomposition Oaxaca-Blinder — Méthode Fairlie (2005)
#
# ARCHITECTURE :
#   - Niveau d'analyse : INDIVIDUEL (acteu_ind = activité propre de l'individu)
#   - Le seuil est calculé sur la distribution individuelle de l'année courante
#   - Les variables individuelles (age, sexe, diplome, acteu_ind) sont issues
#     directement de la base individus ; les variables ménage (typmen, acteu_cj,
#     nb_enfants) proviennent de la jointure avec la table ménage.
#   - La décomposition est appliquée à chaque paire d'années consécutives
#     pour produire une chronique annuelle des effets composition/coefficients.
#
# CHOIX METHODOLOGIQUES :
#   1. On retient les individus avec acteu_ind non NA (adultes avec statut
#      d'activité renseigné, exclut les enfants <15 ans dont acteu = NA).
#   2. acteu_cj = activité du conjoint (NA si sans conjoint) — absorbe
#      l'information sur la bi-activité du ménage dans la formule logit.
#   3. Les poids sont normalisés (moyenne=1) pour la stabilité numérique.
#   4. Décomposition de référence (2017 vs 2023) conservée pour les graphiques.
# ==============================================================================

vars_ob    <- c("age_cat", "sexe_cat", "immi_cat", "diplome",
                "typmen", "acteu_ind", "acteu_cj")
formule_ob <- pauvre ~ age_cat + sexe_cat + immi_cat + diplome +
                       typmen + acteu_ind + acteu_cj

predire_logit <- function(X_mat, beta) {
  cols <- intersect(colnames(X_mat), names(beta))
  eta  <- X_mat[, cols, drop = FALSE] %*% beta[cols]
  plogis(as.numeric(eta))
}

#' Préparer les données OB pour une année
#' @param data   data_all ou sous-ensemble
#' @param an     année cible
#' @param formule formule logit (détermine les variables requises)
#' @param filtre  expression de filtre optionnel (ex. typmen == "Famille monoparentale")
preparer_ob <- function(data, an, formule = formule_ob, filtre = NULL) {
  filtre_q <- rlang::enquo(filtre)
  seuil_an <- 0.6 * mediane_ponderee(
    data$nivviem[data$annee == an], data$wprm[data$annee == an]
  )
  vars_req <- all.vars(formule)[-1]
  d <- data |>
    filter(annee == an, !is.na(acteu_ind)) |>
    mutate(pauvre = as.integer(nivviem < seuil_an))
  if (!rlang::quo_is_null(filtre_q)) d <- d |> filter(!!filtre_q)
  vars_ok <- intersect(vars_req, names(d))
  d <- d |> filter(if_all(all_of(vars_ok), ~ !is.na(.)))
  # S'assurer que toutes les variables catégorielles sont bien des facteurs

  # (immi_cat, diplome, typmen peuvent être character si la source manque)
  for (v in vars_ok) {
    if (is.character(d[[v]])) d[[v]] <- factor(d[[v]])
  }
  d |> mutate(wprm = wprm / mean(wprm))
}

#' Décomposition Fairlie (logit) pour une paire d'années
#' Contrastes treatment (défaut R). Le détail par modalité utilise directement
#' les colonnes du model matrix (Xbar * beta * AME).
#' @return liste : taux, effets globaux, detail_modalite, detail_par_variable
decomposer_ob <- function(df_deb, df_fin, formule = formule_ob) {

  # --- 1. Harmoniser les niveaux des facteurs ---
  vars_facteur <- intersect(all.vars(formule)[-1], intersect(names(df_deb), names(df_fin)))
  for (v in vars_facteur) {
    if (is.character(df_deb[[v]])) df_deb[[v]] <- factor(df_deb[[v]])
    if (is.character(df_fin[[v]])) df_fin[[v]] <- factor(df_fin[[v]])
    if (is.factor(df_deb[[v]]) && is.factor(df_fin[[v]])) {
      niveaux_communs <- union(levels(df_deb[[v]]), levels(df_fin[[v]]))
      df_deb[[v]] <- factor(df_deb[[v]], levels = niveaux_communs)
      df_fin[[v]] <- factor(df_fin[[v]], levels = niveaux_communs)
    }
  }
  stopifnot(all(df_deb$wprm > 0), all(df_fin$wprm > 0))

  # --- 2. Logits pondérés ---
  logit_deb <- glm(formule, data = df_deb, family = binomial, weights = wprm,
                   control = glm.control(maxit = 50))
  logit_fin <- glm(formule, data = df_fin, family = binomial, weights = wprm,
                   control = glm.control(maxit = 50))

  X_deb    <- model.matrix(formule, data = df_deb)
  X_fin    <- model.matrix(formule, data = df_fin)
  beta_deb <- coef(logit_deb)
  beta_fin <- coef(logit_fin)

  # --- 3. Décomposition agrégée (Fairlie) ---
  taux_deb      <- weighted.mean(df_deb$pauvre, df_deb$wprm)
  taux_fin      <- weighted.mean(df_fin$pauvre, df_fin$wprm)
  pred_deb_deb  <- predire_logit(X_deb, beta_deb)
  pred_cf       <- predire_logit(X_fin, beta_deb)
  taux_pred_deb <- weighted.mean(pred_deb_deb, df_deb$wprm)
  taux_cf       <- weighted.mean(pred_cf, df_fin$wprm)

  delta_total        <- taux_fin - taux_deb
  effet_composition  <- taux_cf - taux_pred_deb
  effet_coefficients <- delta_total - effet_composition

  # --- 4. Détail par modalité ---
  # On calcule d'abord la version treatment (colonnes du model matrix),
  # puis on applique la normalisation de Yun (2005) pour rendre les
  # contributions par modalité invariantes au choix de la catégorie de référence.

  # AME moyens pondérés
  ame_poids <- pred_deb_deb * (1 - pred_deb_deb) * df_deb$wprm
  ame_denom <- sum(df_deb$wprm)
  mean_ame_deb <- sum(ame_poids) / ame_denom

  pred_fin_fin  <- predire_logit(X_fin, beta_fin)
  ame_fin_poids <- pred_fin_fin * (1 - pred_fin_fin) * df_fin$wprm
  ame_fin_denom <- sum(df_fin$wprm)
  mean_ame_fin  <- sum(ame_fin_poids) / ame_fin_denom

  # Mapping variable R -> label lisible
  var_labels <- c(
    age_cat = "Age", sexe_cat = "Sexe", immi_cat = "Immigration",
    diplome = "Diplome", nb_enfants = "Nb enfants", typmen = "Type de menage",
    acteu_ind = "Activite individuelle", acteu_cj = "Activite CJ"
  )

  detail_list <- list()

  # Identifier les variables facteur dans la formule
  vars_formule <- all.vars(formule)[-1]
  factor_vars_in_model <- character(0)

  for (v in vars_formule) {
    if (v %in% names(df_deb) && is.factor(df_deb[[v]])) {
      factor_vars_in_model <- c(factor_vars_in_model, v)
      levs <- levels(df_deb[[v]])
      k    <- length(levs)
      lbl  <- if (v %in% names(var_labels)) var_labels[[v]] else v

      # Coefficients treatment : b_ref = 0 (1er niveau), b_2..b_k
      col_names <- paste0(v, levs[-1])
      b_treat_deb <- setNames(rep(0, k), levs)
      b_treat_fin <- setNames(rep(0, k), levs)
      for (cn in col_names) {
        lev_name <- sub(paste0("^", v), "", cn)
        if (cn %in% names(beta_deb) && !is.na(beta_deb[cn]))
          b_treat_deb[lev_name] <- beta_deb[cn]
        if (cn %in% names(beta_fin) && !is.na(beta_fin[cn]))
          b_treat_fin[lev_name] <- beta_fin[cn]
      }

      # Normalisation de Yun (2005) : conversion treatment -> déviation
      # d_j = b_j - mean(b_1,...,b_k) avec b_1 = 0
      gmean_deb <- mean(b_treat_deb)
      gmean_fin <- mean(b_treat_fin)
      d_deb <- b_treat_deb - gmean_deb
      d_fin <- b_treat_fin - gmean_fin

      # Proportions pondérées de chaque niveau
      prop_deb <- map_dbl(levs, ~ weighted.mean(df_deb[[v]] == .x, df_deb$wprm))
      prop_fin <- map_dbl(levs, ~ weighted.mean(df_fin[[v]] == .x, df_fin$wprm))

      for (j in seq_len(k)) {
        detail_list[[length(detail_list) + 1]] <- tibble(
          colonne      = paste0(v, levs[j]),
          variable     = lbl,
          modalite     = levs[j],
          delta_X      = prop_fin[j] - prop_deb[j],
          ame          = d_deb[j] * mean_ame_deb,
          composition  = (prop_fin[j] - prop_deb[j]) * d_deb[j] * mean_ame_deb,
          delta_beta   = d_fin[j] - d_deb[j],
          coefficients = prop_fin[j] * (d_fin[j] - d_deb[j]) * mean_ame_fin
        )
      }
    }
  }

  # Variables non-facteur (numériques) — inchangé
  Xbar_deb <- colSums(X_deb * df_deb$wprm) / sum(df_deb$wprm)
  Xbar_fin <- colSums(X_fin * df_fin$wprm) / sum(df_fin$wprm)
  non_factor_vars <- setdiff(vars_formule, factor_vars_in_model)
  for (v in non_factor_vars) {
    if (v %in% colnames(X_deb) && v %in% colnames(X_fin) &&
        v %in% names(beta_deb) && v %in% names(beta_fin)) {
      Xbar_deb_v <- Xbar_deb[v]
      Xbar_fin_v <- Xbar_fin[v]
      lbl <- if (v %in% names(var_labels)) var_labels[[v]] else v
      detail_list[[length(detail_list) + 1]] <- tibble(
        colonne      = v,
        variable     = lbl,
        modalite     = "",
        delta_X      = Xbar_fin_v - Xbar_deb_v,
        ame          = beta_deb[v] * mean_ame_deb,
        composition  = (Xbar_fin_v - Xbar_deb_v) * beta_deb[v] * mean_ame_deb,
        delta_beta   = beta_fin[v] - beta_deb[v],
        coefficients = Xbar_fin_v * (beta_fin[v] - beta_deb[v]) * mean_ame_fin
      )
    }
  }

  detail_modalite <- bind_rows(detail_list)

  # Agrégation par variable
  detail_par_variable <- detail_modalite |>
    group_by(variable) |>
    summarise(composition  = sum(composition),
              coefficients = sum(coefficients),
              .groups = "drop") |>
    mutate(total = composition + coefficients)

  list(taux_deb = taux_deb, taux_fin = taux_fin,
       delta_total = delta_total,
       effet_composition = effet_composition, effet_coefficients = effet_coefficients,
       detail_modalite = detail_modalite, detail_par_variable = detail_par_variable)
}

# ==============================================================================
# OB — Chronique annuelle (toutes paires consécutives)
# Produit ob_annuel : une ligne par paire (annee_deb, annee_fin)
# ==============================================================================
message("OB annuel : lancement sur toutes les paires consecutives...")

ob_annuel <- map_dfr(seq_len(length(annees) - 1), function(i) {
  an_deb <- annees[i]
  an_fin <- annees[i + 1]
  message(sprintf("  OB %d -> %d", an_deb, an_fin))

  df_deb <- tryCatch(preparer_ob(data_all, an_deb), error = function(e) NULL)
  df_fin <- tryCatch(preparer_ob(data_all, an_fin), error = function(e) NULL)
  if (is.null(df_deb) || nrow(df_deb) < 100 ||
      is.null(df_fin) || nrow(df_fin) < 100) return(NULL)

  res <- tryCatch(decomposer_ob(df_deb, df_fin), error = function(e) {
    message(sprintf("    ERREUR OB %d->%d : %s", an_deb, an_fin, e$message))
    NULL
  })
  if (is.null(res)) return(NULL)

  tibble(
    annee_deb          = an_deb,
    annee_fin          = an_fin,
    taux_deb           = res$taux_deb           * 100,
    taux_fin           = res$taux_fin           * 100,
    delta_total        = res$delta_total        * 100,
    effet_composition  = res$effet_composition  * 100,
    effet_coefficients = res$effet_coefficients * 100
  )
})

# ==============================================================================
# OB — Décomposition de référence (2017 vs 2023) pour les graphiques existants
# ==============================================================================
ob_annee_debut <- 2017
ob_annee_fin   <- 2023

df_ob_debut <- preparer_ob(data_all, ob_annee_debut)
df_ob_fin   <- preparer_ob(data_all, ob_annee_fin)
ob_ref      <- decomposer_ob(df_ob_debut, df_ob_fin)

detail_modalite     <- ob_ref$detail_modalite
detail_par_variable <- ob_ref$detail_par_variable

cat(sprintf("\n=== OB reference %d vs %d ===\n", ob_annee_debut, ob_annee_fin))
cat(sprintf("  Taux %d : %.2f %%\n", ob_annee_debut, ob_ref$taux_deb * 100))
cat(sprintf("  Taux %d : %.2f %%\n", ob_annee_fin,   ob_ref$taux_fin * 100))
cat(sprintf("  Effet composition  : %+.2f pts\n", ob_ref$effet_composition  * 100))
cat(sprintf("  Effet coefficients : %+.2f pts\n", ob_ref$effet_coefficients * 100))

# ==============================================================================
# Familles monoparentales : décomposition des ressources (mono_data)
# On déduplique à une obs par ménage pour les variables monétaires.
# ==============================================================================
vars_fam <- c("prest_fam_petite_enfance", "prest_fam_autres")
vars_rsa  <- c("prest_precarite_rsa", "prest_precarite_rmi", "prest_precarite_api_rmi")

mono_data <- data_all |>
  filter(typmen == "Famille monoparentale") |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(nivviem < seuil_std) |>
  distinct(annee, ident, .keep_all = TRUE) |>      # une obs par ménage
  group_by(annee, seuil_std) |>
  summarise(
    nivvie_apres     = weighted.mean(nivviem,      wprm) / 12,
    nivvie_avant_moy = weighted.mean(nivvie_avant, wprm) / 12,
    prest_fam   = weighted.mean(
      rowSums(across(any_of(vars_fam),  \(x) replace_na(x, 0))) / nb_uci / 12, wprm),
    prest_rsa   = weighted.mean(
      rowSums(across(any_of(vars_rsa),  \(x) replace_na(x, 0))) / nb_uci / 12, wprm),
    prest_apl   = weighted.mean(replace_na(prest_logement, 0) / nb_uci / 12, wprm),
    ppa_rsa_act = weighted.mean(
      rowSums(across(any_of(c("ppa", "m_rsa_actm")), \(x) replace_na(x, 0))) / nb_uci / 12,
      wprm),
    prest_autres = weighted.mean(
      (total_prestations
       - replace_na(prest_logement, 0)
       - rowSums(across(any_of(vars_fam),             \(x) replace_na(x, 0)))
       - rowSums(across(any_of(vars_rsa),             \(x) replace_na(x, 0)))
       - rowSums(across(any_of(c("ppa","m_rsa_actm")), \(x) replace_na(x, 0)))
      ) / nb_uci / 12, wprm),
    total_prest_moy = weighted.mean(total_prestations / nb_uci / 12, wprm),
    total_impots_moy = weighted.mean(total_impots / nb_uci / 12, wprm),
    n_pond = sum(wprm),
    .groups = "drop"
  ) |>
  mutate(seuil_mensuel = seuil_std / 12)

cat(sprintf("mono_data : ok — %d annees\n", nrow(mono_data)))

# ==============================================================================
# Monoparentales : taux de pauvreté par statut d'activité INDIVIDUEL (PR)
# On filtre lpr == 1 pour ne retenir que la personne de référence, dont
# acteu_ind est l'équivalent de l'ancien acteu_pr au niveau ménage.
# ==============================================================================
mono_taux_activite <- data_all |>
  filter(typmen == "Famille monoparentale", lpr == 1, !is.na(acteu_ind)) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = as.integer(nivviem < seuil_std)) |>
  group_by(annee, acteu_ind) |>
  summarise(
    taux   = 100 * weighted.mean(pauvre, wprm),
    n_pond = sum(wprm),
    .groups = "drop"
  ) |>
  mutate(
    tooltip = paste0(acteu_ind, "\n", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(acteu_ind, "_mono_activ_", annee)
  )

cat(sprintf("mono_taux_activite : ok — %d lignes\n", nrow(mono_taux_activite)))

# ==============================================================================
# Taux de pauvreté par statut d'occupation × APL
# On déduplique à une obs par ménage (statut_occ est une variable ménage).
# ==============================================================================
diag_couverture <- data_all |>
  distinct(annee, ident, .keep_all = TRUE) |>
  group_by(annee) |>
  summarise(
    n_total    = n(),
    pct_statut = round(100 * sum(!is.na(statut_occ)) / n_total, 1),
    .groups = "drop"
  )
cat("\n--- Diagnostic couverture statut_occ ---\n")
print(diag_couverture, n = Inf)
annees_occ_ok <- diag_couverture |> filter(pct_statut >= 80) |> pull(annee)

taux_occ_apl <- data_all |>
  distinct(annee, ident, .keep_all = TRUE) |>
  filter(annee %in% annees_occ_ok, !is.na(statut_occ),
         statut_occ != "Loge gratuitement") |>
  left_join(seuils_annuels, by = "annee") |>
  group_by(annee, statut_occ) |>
  summarise(
    taux_avec_apl = 100 * sum(wprm[nivviem < first(seuil_std)]) / sum(wprm),
    taux_hors_apl = 100 * sum(wprm[nivviem_hors_apl < first(seuil_hapl)]) / sum(wprm),
    .groups = "drop"
  ) |>
  pivot_longer(c(taux_avec_apl, taux_hors_apl),
               names_to = "mesure_apl", values_to = "taux") |>
  mutate(
    label   = paste0(statut_occ, " - ",
                     ifelse(mesure_apl == "taux_avec_apl", "avec APL", "hors APL")),
    tooltip = paste0(label, "\n", annee, " : ", round(taux, 1), " %"),
    data_id = paste0(label, "_", annee)
  )

# ==============================================================================
# Pauvreté par statut d'activité INDIVIDUEL (remplace l'ancienne version ménage)
#
# Définition : taux de pauvreté = proportion d'individus d'activité X dont le
# niveau de vie du ménage est inférieur au seuil. Chaque individu est pondéré
# par wprm (poids du ménage), sans dédupliquer.
# ==============================================================================
taux_activite <- data_all |>
  filter(!is.na(acteu_ind), annee >= 2010) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  group_by(annee, acteu_ind) |>
  summarise(
    taux   = 100 * sum(wprm[nivviem < first(seuil_std)]) / sum(wprm),
    n_pond = sum(wprm),
    .groups = "drop"
  ) |>
  mutate(
    tooltip = paste0(acteu_ind, "\n", annee, " : ", round(taux, 1), " %\n(",
                     format(round(n_pond / 1e6, 1), big.mark = " "), " M individus)"),
    data_id = paste0(acteu_ind, "_", annee)
  )

part_actifs_pauvres <- data_all |>
  filter(!is.na(acteu_ind), annee >= 2010) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(nivviem < seuil_std) |>
  group_by(annee, acteu_ind) |>
  summarise(poids = sum(wprm), .groups = "drop") |>
  group_by(annee) |>
  mutate(
    part    = 100 * poids / sum(poids),
    tooltip = paste0(acteu_ind, "\n", annee, " : ", round(part, 1), " %"),
    data_id = paste0(acteu_ind, "_compo_", annee)
  ) |>
  ungroup()

# ==============================================================================
# Travailleurs pauvres
#
# Définition : individu en emploi (acteu_ind == "Emploi") dont le ménage est
# pauvre (nivviem < seuil). Plus précis que l'ancienne définition ménage
# (acteu_pr == "Emploi") qui attribuait le statut de travailleur à tous les
# membres du ménage dont le PR était en emploi.
#
# Pour les analyses de ressources (tp_data) et de composition par ménage
# (tp_typmen), on déduplique à une obs par ménage afin d'éviter de compter
# plusieurs fois les variables monétaires du ménage (nivvie_avant, prestations).
# Pour le type de contrat (tp_contrat) et la PCS (tp_pcs), on conserve le
# niveau individuel car ces variables sont propres à chaque travailleur.
# ==============================================================================
tp_base <- data_all |>
  filter(acteu_ind == "Emploi", annee >= 2010) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std)

# Ressources des travailleurs pauvres (ménage)
tp_data <- tp_base |>
  filter(pauvre) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  group_by(annee) |>
  summarise(
    nivvie_apres     = weighted.mean(nivviem,      wprm) / 12,
    nivvie_avant_moy = weighted.mean(nivvie_avant, wprm) / 12,
    ppa_rsa_act = weighted.mean(
      rowSums(across(any_of(c("ppa","m_rsa_actm")), \(x) replace_na(x,0))) / nb_uci / 12,
      wprm),
    prest_apl   = weighted.mean(replace_na(prest_logement, 0) / nb_uci / 12, wprm),
    prest_fam   = weighted.mean(
      rowSums(across(any_of(vars_fam), \(x) replace_na(x,0))) / nb_uci / 12, wprm),
    prest_rsa   = weighted.mean(
      rowSums(across(any_of(vars_rsa), \(x) replace_na(x,0))) / nb_uci / 12, wprm),
    prest_autres = weighted.mean(
      (total_prestations
       - rowSums(across(any_of(c("ppa","m_rsa_actm")), \(x) replace_na(x,0)))
       - replace_na(prest_logement, 0)
       - rowSums(across(any_of(vars_fam), \(x) replace_na(x,0)))
       - rowSums(across(any_of(vars_rsa), \(x) replace_na(x,0)))
      ) / nb_uci / 12, wprm),
    total_prest_moy = weighted.mean(total_prestations / nb_uci / 12, wprm),
    total_impots_moy = weighted.mean(total_impots / nb_uci / 12, wprm),
    seuil_mensuel = first(seuil_std) / 12,
    n_pond        = sum(wprm),
    .groups = "drop"
  )
cat(sprintf("tp_data : ok — %d annees\n", nrow(tp_data)))

# Dates clés : 2015, 2019, 2023 (variables cdd/cdi/interim disponibles à partir de 2015)
dates_cles_contrat <- c(2015, 2019, 2023)

# Composition par type de ménage (ménage dédupliqué) — pauvres vs non pauvres
tp_typmen <- tp_base |>
  filter(annee %in% dates_cles_contrat) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
  group_by(annee, pauvre, groupe, typmen) |>
  summarise(poids = sum(wprm), .groups = "drop") |>
  group_by(annee, pauvre) |>
  mutate(
    part    = 100 * poids / sum(poids),
    tooltip = paste0(typmen, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
    data_id = paste0(typmen, "_", pauvre, "_", annee)
  ) |>
  ungroup()

# Type de contrat (individuel — variables directement dans data_all)
tp_contrat <- tp_base |>
  filter(!is.na(type_contrat), annee %in% dates_cles_contrat) |>
  mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
  group_by(annee, pauvre, groupe, type_contrat) |>
  summarise(poids = sum(wprm, na.rm = TRUE), .groups = "drop") |>
  group_by(annee, pauvre) |>
  mutate(
    part    = 100 * poids / sum(poids),
    tooltip = paste0(type_contrat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
    data_id = paste0(type_contrat, "_", pauvre, "_", annee)
  ) |>
  ungroup()
cat(sprintf("tp_contrat : ok — %d annees\n", n_distinct(tp_contrat$annee)))

# PCS (individuel)
# Dates clés : 2015, 2019, 2023 (cohérent avec type de contrat)
if (any(!is.na(data_all$pcs_cat))) {
  tp_pcs <- tp_base |>
    filter(!is.na(pcs_cat), annee %in% dates_cles_contrat, lpr == 1) |>
    mutate(groupe = ifelse(pauvre, "Travailleurs pauvres", "Travailleurs non pauvres")) |>
    group_by(annee, pauvre, groupe, pcs_cat) |>
    summarise(poids = sum(wprm, na.rm = TRUE), .groups = "drop") |>
    group_by(annee, pauvre) |>
    mutate(
      part    = 100 * poids / sum(poids),
      tooltip = paste0(pcs_cat, " — ", groupe, "\n", annee, " : ", round(part, 1), " %"),
      data_id = paste0(pcs_cat, "_", pauvre, "_", annee)
    ) |>
    ungroup()
  cat(sprintf("tp_pcs : ok — %d annees\n", n_distinct(tp_pcs$annee)))
} else {
  tp_pcs <- NULL
  cat("AVERTISSEMENT : pcs_cat absente dans data_all — tp_pcs non cree\n")
}

cat("\n=== decomposition_pauvrete.R termine ===\n")
