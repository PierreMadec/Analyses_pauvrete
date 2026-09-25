# ==============================================================================
# extract_dipl_pcs.R
#
# Extrait et HARMONISE le diplôme et la catégorie socioprofessionnelle de la
# personne de référence, pour les deux périodes de la décomposition Oaxaca.
#
# Sources et raccord :
#   - 2010-2012 : fichier « irf » (partie enquête Emploi) — variables `ddipl`
#     (diplôme, 6 postes) et `cse` (CS de l'emploi actuel, 1er chiffre).
#     ⚠ NE PAS utiliser `cspm` : c'est une CS de ménage (11 % d'agriculteurs).
#   - 2022-2024 : fichier « indiv » — variables `dip5` et `pcs1`.
#
# Harmonisation du diplôme en 5 postes. `dip5` est exactement `dip7` regroupé
# ({1,2}->1, 3->2, 4->3, 5->4, {6,7}->5) et `ddipl` a précisément ces 5 postes
# (il n'a pas de modalité 2) :
#     ddipl 1 <-> dip5 1  Supérieur
#     ddipl 3 <-> dip5 2  Bac+2
#     ddipl 4 <-> dip5 3  Bac
#     ddipl 5 <-> dip5 4  CAP-BEP
#     ddipl 6,7 <-> dip5 5  BEPC ou aucun
#
# PCS retenue au 1er chiffre seulement : c'est le seul niveau insensible à la
# révision de nomenclature PCS 2020.
#
# Contrôles passés (cf. conversation) : distributions plausibles aux deux dates,
# stabilité intra-période (< 1 point), et surtout AUCUNE rupture au raccord
# 2020/2021 sur la série annuelle complète 2010-2024 (+1,6 point de « Supérieur »
# entre 2020 et 2021, contre +1,5 et +1,9 les deux années précédentes).
#
# Produit : figure/dipl_pcs.rds — (annee, ident, dipl_h, pcs_h) pour les PR.
# ==============================================================================

suppressMessages({library(haven); library(data.table); library(dplyr)})

base_path <- "/Users/pierremadec/Documents/ERFS_backup"
path_fig  <- "figure"

lire <- function(f) {
  if (grepl("dta$", f))
    as.data.frame(tryCatch(read_dta(f), error = function(e) read_dta(f, encoding = "latin1")))
  else if (grepl("csv$", f))
    as.data.frame(fread(f, showProgress = FALSE, na.strings = c("", "NA")))
  else
    as.data.frame(read_sas(f))
}

fichier <- function(an, motif) {
  fs <- list.files(file.path(base_path, sprintf("ERFS %d", an)),
                   pattern = motif, full.names = TRUE, ignore.case = TRUE)
  if (length(fs)) fs[1] else NA_character_
}

LD <- c("Supérieur", "Bac+2", "Bac", "CAP-BEP", "BEPC ou aucun")
LP <- c("Agriculteurs", "Artisans, commerçants", "Cadres",
        "Prof. intermédiaires", "Employés", "Ouvriers")

# ── t1 : 2010-2012, fichier irf ──────────────────────────────────────────────
t1 <- rbindlist(lapply(2010:2012, function(an) {
  f <- fichier(an, "irf.*[.](dta|sas7bdat)$"); if (is.na(f)) return(NULL)
  d <- lire(f); names(d) <- tolower(names(d))
  x <- as.character(d$ddipl); p <- substr(as.character(d$cse), 1, 1)
  data.table(
    annee   = an,
    noindiv = as.character(d$noindiv),
    dipl_h  = fcase(x == "1", LD[1], x == "3", LD[2], x == "4", LD[3],
                    x == "5", LD[4], x %in% c("6", "7"), LD[5]),
    pcs_h   = fcase(p == "1", LP[1], p == "2", LP[2], p == "3", LP[3],
                    p == "4", LP[4], p == "5", LP[5], p == "6", LP[6]))
}), fill = TRUE)

# ── t2 : 2022-2024, fichier indiv ────────────────────────────────────────────
t2 <- rbindlist(lapply(2022:2024, function(an) {
  f <- fichier(an, "indiv.*[.](dta|csv)$"); if (is.na(f)) return(NULL)
  d <- lire(f); names(d) <- tolower(names(d))
  x <- as.character(d$dip5); p <- substr(as.character(d$pcs1), 1, 1)
  data.table(
    annee   = an,
    noindiv = as.character(d$noindiv),
    dipl_h  = fcase(x == "1", LD[1], x == "2", LD[2], x == "3", LD[3],
                    x == "4", LD[4], x == "5", LD[5]),
    pcs_h   = fcase(p == "1", LP[1], p == "2", LP[2], p == "3", LP[3],
                    p == "4", LP[4], p == "5", LP[5], p == "6", LP[6]))
}), fill = TRUE)

h <- unique(rbind(t1, t2), by = c("annee", "noindiv"))

# ── Clé (annee, ident) des personnes de référence ────────────────────────────
trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
map  <- trav |>
  filter(lpr == 1) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  transmute(annee, ident = as.character(ident), noindiv = as.character(noindiv))

res <- map |>
  left_join(as.data.frame(h), by = c("annee", "noindiv")) |>
  mutate(dipl_h = factor(dipl_h, levels = LD),
         pcs_h  = factor(pcs_h,  levels = LP)) |>
  select(annee, ident, dipl_h, pcs_h) |>
  filter(!is.na(dipl_h) | !is.na(pcs_h))

saveRDS(res, file.path(path_fig, "dipl_pcs.rds"))
cat(sprintf("dipl_pcs.rds : %s lignes, années %s\n",
            format(nrow(res), big.mark = " "),
            paste(range(res$annee), collapse = "-")))
print(res |> count(annee, dipl_h) |> group_by(annee) |>
        mutate(pct = round(100 * n / sum(n))) |> select(-n) |>
        tidyr::pivot_wider(names_from = annee, values_from = pct) |> as.data.frame())
