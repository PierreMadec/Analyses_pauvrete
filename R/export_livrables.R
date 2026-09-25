# ==============================================================================
# export_livrables.R
#
# Exporte, pour chacune des 10 figures de l'article, les données sous-jacentes
# (CSV, pour reconstruction dans Excel) et une image statique (PNG, pour le
# document Word). Ne modifie aucun fichier existant du pipeline.
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(ggiraph)
})

out_data <- "livrables/data"
out_img  <- "livrables/images"
dir.create(out_data, showWarnings = FALSE, recursive = TRUE)
dir.create(out_img, showWarnings = FALSE, recursive = TRUE)

save_png <- function(p, name, width = 9, height = 5.5) {
  ggsave(file.path(out_img, paste0(name, ".png")), p, width = width, height = height, dpi = 150, bg = "white")
}

# ── 1. fig-emploi-pauvrete ──────────────────────────────────────────────────
p1 <- readRDS("figure/g0_emploi_pauvrete.rds")
d1 <- p1$data |> select(annee, `Taux d'emploi (%)` = taux_empl, `Taux de pauvreté (%)` = taux_pauv)
write_csv(d1, file.path(out_data, "01_emploi_pauvrete.csv"))
save_png(p1, "01_emploi_pauvrete")

# ── 2. fig-rel-anc-trav ─────────────────────────────────────────────────────
p2 <- readRDS("figure/pauvrete_rel_anc_trav.rds")
d2 <- p2$data |> select(annee, mesure, `Taux de pauvreté (%)` = taux) |>
  pivot_wider(names_from = mesure, values_from = `Taux de pauvreté (%)`)
write_csv(d2, file.path(out_data, "02_relatif_ancre.csv"))
save_png(p2, "02_relatif_ancre")

# ── 3. fig-tp3-taux-config ──────────────────────────────────────────────────
p3 <- readRDS("figure/tp3_taux_config.rds")
d3 <- p3$data |> select(annee, config, `Taux de pauvreté (%)` = taux, n_obs) |>
  pivot_wider(names_from = config, values_from = `Taux de pauvreté (%)`, id_cols = annee)
write_csv(d3, file.path(out_data, "03_taux_par_config.csv"))
save_png(p3, "03_taux_par_config")

# ── 4. fig-tp-diag ───────────────────────────────────────────────────────────
p4 <- readRDS("figure/tp_diag_config.rds")
d4 <- p4$data |> select(config, cause, `Part des travailleurs pauvres (%)` = pct) |>
  pivot_wider(names_from = cause, values_from = `Part des travailleurs pauvres (%)`)
write_csv(d4, file.path(out_data, "04_diagnostic_causes.csv"))
save_png(p4, "04_diagnostic_causes", height = 6.5)

# ── 5. fig-salaire-distrib (courbe de densité reconstruite) ────────────────
p5 <- readRDS("figure/tp_salaire_horaire_distrib.rds")
b5 <- ggplot_build(p5)
grp_labels <- c("1" = "Travailleur pauvre", "2" = "Travailleur non pauvre")
d5 <- b5$data[[1]] |>
  mutate(statut = grp_labels[as.character(group)]) |>
  select(statut, `Ratio salaire horaire / Smic` = x, Densité = density) |>
  arrange(statut, `Ratio salaire horaire / Smic`) |>
  pivot_wider(names_from = statut, values_from = Densité, id_cols = `Ratio salaire horaire / Smic`)
write_csv(d5, file.path(out_data, "05_distribution_salaire_horaire.csv"))
save_png(p5, "05_distribution_salaire_horaire")

# ── 6. fig-tp-cf ─────────────────────────────────────────────────────────────
p6 <- readRDS("figure/tp_contrefactuel.rds")
d6 <- p6$data |> mutate(levier = gsub("\n", " ", levier)) |>
  select(config, levier, `Sortis du seuil (%)` = pct) |>
  pivot_wider(names_from = levier, values_from = `Sortis du seuil (%)`)
write_csv(d6, file.path(out_data, "06_contrefactuels.csv"))
save_png(p6, "06_contrefactuels")

# ── 7. fig-oaxaca-barre ──────────────────────────────────────────────────────
p7 <- readRDS("figure/oaxaca_tp_barre.rds")
d7 <- p7$data |> mutate(effet = gsub("\n", " ", effet)) |>
  select(effet, `Contribution (pts de %)` = valeur)
write_csv(d7, file.path(out_data, "07_oaxaca_composition_coefficients.csv"))
save_png(p7, "07_oaxaca_composition_coefficients")

# ── 8. fig-cohortes-jeunes ───────────────────────────────────────────────────
p8 <- readRDS("figure/cohortes_jeunes.rds")
d8 <- p8$data |> mutate(tx = tx * 100, label_coh = gsub("\n", " ", label_coh)) |>
  select(Cohorte = label_coh, `Taux de pauvreté laborieuse à 25-34 ans (%)` = tx, n)
write_csv(d8, file.path(out_data, "08_cohortes.csv"))
save_png(p8, "08_cohortes")

# ── 9. fig-soutien-amortisseur ───────────────────────────────────────────────
p9 <- readRDS("figure/soutien_effet_amortisseur.rds")
d9 <- p9$data |> select(annee, scenario, `Taux de pauvreté laborieuse (%)` = taux) |>
  pivot_wider(names_from = scenario, values_from = `Taux de pauvreté laborieuse (%)`)
write_csv(d9, file.path(out_data, "09_effet_amortisseur.csv"))
save_png(p9, "09_effet_amortisseur")

# ── 10. fig-decompo-transferts-config ────────────────────────────────────────
p10 <- readRDS("figure/decompo_transferts_config.rds")
d10 <- p10$data |> mutate(effet = effet * 100, groupe_lab = gsub("\n", " ", groupe_lab)) |>
  select(Configuration = groupe_lab, Poste = poste_lab, `Effet amortisseur (pts)` = effet) |>
  pivot_wider(names_from = Poste, values_from = `Effet amortisseur (pts)`)
write_csv(d10, file.path(out_data, "10_decompo_transferts.csv"))
save_png(p10, "10_decompo_transferts", height = 6)

cat("Export termine :", length(list.files(out_data)), "CSV,", length(list.files(out_img)), "PNG\n")
