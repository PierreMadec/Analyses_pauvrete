# ==============================================================================
# retraites_impot_revenu.R
#
# Part des ménages comptant au moins un retraité assujettis à l'impôt sur le
# revenu, ERFS 2023.
#
# Champ : ménages (le foyer fiscal n'existe pas dans l'ERFS, le ménage sert
# d'approximation, convention déjà retenue dans ce projet, cf. extract_ppe.R).
#
# Définitions :
#   - "ménage de retraité" : au moins un membre du ménage perçoit une pension
#     de retraite au sens de l'enquête Emploi (RETRAITE == 1 pour au moins un
#     individu du ménage). Champ EEC = personnes de 50 à 89 ans -> un ménage
#     dont le seul retraité potentiel est hors de cette tranche n'est pas vu.
#   - "assujetti à l'impôt sur le revenu" : IRPP net du ménage strictement
#     positif (IRPP <= 0 = ménage non imposable, y compris les ménages
#     bénéficiaires nets d'un crédit d'impôt remboursable, IRPP < 0).
#
# Poids : wprm (poids ménage, une ligne par ménage — pas de double-compte des
# couples de retraités, à la différence d'une pondération par personne).
#
# Produit :
#   - figure/retraites_imposables_2023.rds : listes par tranche d'âge / décile
#   - retraites_impot_revenu.png            : graphique par âge de la PR
#   - retraites_impot_revenu_decile.png     : graphique par décile de niveau de vie
# ==============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

base_path <- "/Users/pierremadec/Documents/ERFS_backup/ERFS 2023"

indiv <- read_dta(file.path(base_path, "fpr_indiv_2023.dta")) |>
  select(ident23, noi, champ_calcul, wprm, AGE, RETRAITE) |>
  filter(champ_calcul == 1)                # champ_calcul == 1 : "dans le champ de diffusion de l'ERFS"

menage <- read_dta(file.path(base_path, "fpr_menage_2023.dta")) |>
  select(ident23, champ_calcul, NOIPRL, AGEPRL, IRPP, nivviem, wprm) |>
  filter(champ_calcul == 1) |>
  select(-champ_calcul)

# ── Déciles de niveau de vie pondérés (convention standard : population, en
#    personnes), calculés sur l'ensemble des individus dans le champ ──────────
pop_indiv <- indiv |> inner_join(menage |> select(ident23, nivviem), by = "ident23")
wquant <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) * p)[1]]
}
breaks_decile <- c(-Inf, sapply(1:9 / 10, wquant, x = pop_indiv$nivviem, w = pop_indiv$wprm), Inf)

# ── Ménage comptant au moins un membre retraité ────────────────────────────────
has_retraite <- indiv |>
  group_by(ident23) |>
  summarise(retraite = any(RETRAITE == 1, na.rm = TRUE), .groups = "drop")

d <- menage |>
  inner_join(has_retraite, by = "ident23") |>
  filter(retraite) |>                     # au moins un retraité au sens EEC (50-89 ans) dans le ménage
  mutate(
    imposable = IRPP > 0,
    decile = cut(nivviem, breaks = breaks_decile, labels = 1:10, include.lowest = TRUE),
    tranche_age = cut(
      AGEPRL,
      breaks = c(-Inf, 59, 69, 79, 89, Inf),
      labels = c("Moins de 60 ans", "60-69 ans", "70-79 ans", "80-89 ans", "90 ans et plus")
    )
  )

# ── Part imposable, ensemble des ménages de retraités ─────────────────────────
ens <- d |>
  summarise(
    n_men      = sum(wprm),
    part_impos = 100 * weighted.mean(imposable, wprm)
  )
message(sprintf(
  "Ménages comptant au moins un retraité (50-89 ans, EEC) : %.2f millions, %.1f %% assujettis à l'IR",
  ens$n_men / 1e6, ens$part_impos
))

# ── Par tranche d'âge de la PR (à titre descriptif — la PR n'est pas toujours
#    le membre retraité du ménage) ─────────────────────────────────────────────
par_age <- d |>
  group_by(tranche_age) |>
  summarise(
    n_men      = sum(wprm),
    part_impos = 100 * weighted.mean(imposable, wprm),
    .groups = "drop"
  )
print(par_age)

# ── Par décile de niveau de vie du ménage ──────────────────────────────────────
par_decile <- d |>
  group_by(decile) |>
  summarise(
    n_men      = sum(wprm),
    part_impos = 100 * weighted.mean(imposable, wprm),
    .groups = "drop"
  )
print(par_decile)

path_fig <- "../figure"
if (!dir.exists(path_fig)) dir.create(path_fig)
saveRDS(list(ensemble = ens, par_age = par_age, par_decile = par_decile),
        file.path(path_fig, "retraites_imposables_2023.rds"))

# ── Graphique ──────────────────────────────────────────────────────────────────
theme_cnle <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(colour = "grey30", size = 11),
      plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0),
      axis.title       = element_blank(),
      axis.text.y      = element_text(size = 11)
    )
}

bluish <- "#2674DD"
redish_line <- "#D4308D"

p <- ggplot(par_age, aes(x = tranche_age, y = part_impos)) +
  geom_col(fill = bluish, width = 0.6) +
  geom_text(aes(label = paste0(gsub("\\.", ",", round(part_impos, 1)), " %")),
            vjust = -0.6, size = 4.2, fontface = "bold") +
  geom_hline(yintercept = ens$part_impos, linetype = "dashed", colour = redish_line, linewidth = 0.6) +
  annotate("text", x = 0.62, y = ens$part_impos, hjust = 0, vjust = -0.6,
           label = paste0("Ensemble des ménages avec retraité : ", gsub("\\.", ",", round(ens$part_impos, 1)), " %"),
           colour = redish_line, size = 3.6, fontface = "italic") +
  scale_y_continuous(limits = c(0, max(par_age$part_impos) * 1.2), labels = label_number(suffix = " %", decimal.mark = ",")) +
  labs(
    title = "Part des ménages comptant un retraité assujettis à l'impôt sur le revenu, par âge",
    subtitle = "France, 2023 — ménages comptant au moins un retraité (au sens de l'enquête Emploi), par âge de la personne de référence",
    caption = "Champ : ménages comptant au moins un membre de 50 à 89 ans percevant une pension de retraite (RETRAITE = 1, enquête Emploi).\nÂge = personne de référence du ménage (pas nécessairement le membre retraité).\nAssujetti = IRPP net du ménage strictement positif (l'ERFS n'a pas de foyer fiscal, ménage = approximation). Pondération par ménage (wprm).\nSource : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE)."
  ) +
  theme_cnle()

ggsave("retraites_impot_revenu.png", p, width = 9, height = 5.5, dpi = 200, bg = "white")
message("Graphique enregistré : retraites_impot_revenu.png")

p_decile <- ggplot(par_decile, aes(x = decile, y = part_impos)) +
  geom_col(fill = bluish, width = 0.65) +
  geom_text(aes(label = paste0(gsub("\\.", ",", round(part_impos, 1)), " %")),
            vjust = -0.6, size = 4, fontface = "bold") +
  geom_hline(yintercept = ens$part_impos, linetype = "dashed", colour = redish_line, linewidth = 0.6) +
  annotate("text", x = 0.7, y = ens$part_impos, hjust = 0, vjust = -0.6,
           label = paste0("Ensemble des ménages avec retraité : ", gsub("\\.", ",", round(ens$part_impos, 1)), " %"),
           colour = redish_line, size = 3.5, fontface = "italic") +
  scale_y_continuous(limits = c(0, 100), labels = label_number(suffix = " %", decimal.mark = ",")) +
  labs(
    title = "Part des ménages comptant un retraité assujettis à l'impôt sur le revenu, par décile de niveau de vie",
    subtitle = "France, 2023 — déciles de niveau de vie pondérés (population), calculés sur l'ensemble des individus",
    x = "Décile de niveau de vie du ménage (D1 = 10 % les plus modestes)",
    caption = "Champ : ménages comptant au moins un membre de 50 à 89 ans percevant une pension de retraite (RETRAITE = 1, enquête Emploi), champ_calcul = 1.\nAssujetti = IRPP net du ménage strictement positif (l'ERFS n'a pas de foyer fiscal, ménage = approximation). Pondération par ménage (wprm).\nDéciles de niveau de vie : convention standard, calculés sur la population d'individus (pas sur les ménages).\nSource : Insee, ERFS 2023 ; calculs Pierre Madec (OFCE)."
  ) +
  theme_cnle() +
  theme(axis.title.x = element_text(size = 10, colour = "grey30"))

ggsave("retraites_impot_revenu_decile.png", p_decile, width = 9.5, height = 5.5, dpi = 200, bg = "white")
message("Graphique enregistré : retraites_impot_revenu_decile.png")
