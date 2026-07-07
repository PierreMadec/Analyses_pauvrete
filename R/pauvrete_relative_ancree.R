# ==============================================================================
# pauvrete_relative_ancree.R
#
# Colonne vertébrale de l'article : décomposer le "paradoxe" emploi/pauvreté
# en distinguant
#   - le seuil RELATIF (60 % de la médiane courante) -> mesure officielle
#   - le seuil ANCRÉ   (seuil 2005 porté par l'inflation, pouvoir d'achat constant)
#
# Résultat clé : à pouvoir d'achat constant, la pauvreté BAISSE (surtout en fin
# de période), alors que la pauvreté relative MONTE. Le "paradoxe" est d'abord
# un effet de seuil relatif : la médiane progresse plus vite que le bas de la
# distribution, qui décroche du milieu sans s'appauvrir en niveau.
#
# Figures produites (figure/) :
#   pauvrete_rel_anc_global   — pop. totale : pauvreté relative vs ancrée
#   pauvrete_rel_anc_trav     — travailleurs (PR emploi 18-64) : relative vs ancrée
#   decrochage_median         — médiane vs 1er décile de niveau de vie (indices réels)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}

path_fig <- "figure"
if (!dir.exists(path_fig)) dir.create(path_fig)

# Médiane pondérée
wmed <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}
# Quantile pondéré
wquant <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) * p)[1]]
}

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title.x     = element_blank()
    )
}

pal_ra <- c("Seuil relatif (médiane courante)" = "#e31a1c",
            "Seuil ancré (pouvoir d'achat 2005)" = "#1f78b4")

# ── Seuil ancré = seuil relatif 2005 porté par l'inflation (IPC) ──────────────
pm  <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
ipc <- setNames(pm$ipc, as.character(pm$annee))
seuil_rel_2005 <- seuils_annuels$seuil_std[seuils_annuels$annee == 2005]

seuils <- seuils_annuels |>
  select(annee, seuil_std) |>
  mutate(seuil_ancre = seuil_rel_2005 * ipc[as.character(annee)] / ipc["2005"])

dd <- data_all |> left_join(seuils, by = "annee")

# ==============================================================================
# Fonction générique : taux relatif + ancré pour un sous-champ
# ==============================================================================
taux_rel_anc <- function(df, champ_lab) {
  df |>
    group_by(annee) |>
    summarise(
      `Seuil relatif (médiane courante)`   = 100 * weighted.mean(nivviem < seuil_std,   wprm),
      `Seuil ancré (pouvoir d'achat 2005)` = 100 * weighted.mean(nivviem < seuil_ancre, wprm),
      .groups = "drop"
    ) |>
    pivot_longer(-annee, names_to = "mesure", values_to = "taux") |>
    mutate(
      mesure  = factor(mesure, levels = names(pal_ra)),
      tooltip = paste0(mesure, "\n", annee, " : ", round(taux, 1), " %\n", champ_lab),
      data_id = paste0(gsub("[^a-z]", "", tolower(mesure)), "_", annee)
    )
}

faire_fig <- function(dat, titre_y, caption) {
  ggplot(dat, aes(x = annee, y = taux, colour = mesure, group = mesure)) +
    geom_line_interactive(linewidth = 1.1) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.4) +
    scale_colour_manual(values = pal_ra) +
    scale_x_continuous(breaks = seq(2005, 2023, 2)) +
    scale_y_continuous(labels = label_number(suffix = " %")) +
    labs(y = titre_y, caption = caption) +
    theme_erfs()
}

cap_global <- paste0(
  "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
  "Seuil relatif : 60 % de la médiane de niveau de vie de l'année. ",
  "Seuil ancré : seuil 2005 porté par l'inflation (pouvoir d'achat constant)."
)

# ── Figure 1 : population totale ──────────────────────────────────────────────
dat_glob <- taux_rel_anc(dd, "Ensemble de la population")
g_global <- faire_fig(dat_glob, "Taux de pauvreté (%)", cap_global)
saveRDS(g_global, file.path(path_fig, "pauvrete_rel_anc_global.rds"))
cat("pauvrete_rel_anc_global : ok\n")

# ── Figure 2 : travailleurs (PR en emploi, 18-64) ─────────────────────────────
dat_trav <- dd |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64) |>
  taux_rel_anc("Travailleurs (PR en emploi, 18-64 ans)")
g_trav <- faire_fig(
  dat_trav, "Taux de pauvreté laborieuse (%)",
  paste0(cap_global, "\nChamp : personnes de référence en emploi, 18-64 ans.")
)
saveRDS(g_trav, file.path(path_fig, "pauvrete_rel_anc_trav.rds"))
cat("pauvrete_rel_anc_trav : ok\n")

# ==============================================================================
# Figure 3 : le décrochage — médiane vs 1er décile de niveau de vie
# Indices base 100 en 2005, en euros constants (déflatés par l'IPC).
# Si la médiane progresse plus vite que le 1er décile -> le bas décroche.
# ==============================================================================
defl <- function(x, an) x / (ipc[as.character(an)] / ipc["2005"])

distrib <- dd |>
  group_by(annee) |>
  summarise(
    mediane = wmed(nivviem, wprm),
    d1      = wquant(nivviem, wprm, 0.10),
    .groups = "drop"
  ) |>
  mutate(
    mediane_reel = defl(mediane, annee),
    d1_reel      = defl(d1, annee),
    `Médiane (D5)`        = 100 * mediane_reel / mediane_reel[annee == 2005],
    `1er décile (D1)`     = 100 * d1_reel / d1_reel[annee == 2005]
  ) |>
  select(annee, `Médiane (D5)`, `1er décile (D1)`) |>
  pivot_longer(-annee, names_to = "serie", values_to = "indice") |>
  mutate(
    serie   = factor(serie, levels = c("Médiane (D5)", "1er décile (D1)")),
    tooltip = paste0(serie, "\n", annee, " : ", round(indice, 1), " (base 100 = 2005)"),
    data_id = paste0(gsub("[^a-z]", "", tolower(serie)), "_", annee)
  )

g_decroch <- ggplot(distrib, aes(x = annee, y = indice, colour = serie, group = serie)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = "grey70") +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.4) +
  scale_colour_manual(values = c("Médiane (D5)" = "#33a02c", "1er décile (D1)" = "#e31a1c")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  labs(
    y = "Niveau de vie réel (indice base 100 = 2005)",
    caption = paste0(
      "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
      "Niveaux de vie déflatés par l'IPC. Lecture : un écart croissant entre les ",
      "deux courbes traduit un décrochage du bas de la distribution par rapport au milieu."
    )
  ) +
  theme_erfs()
saveRDS(g_decroch, file.path(path_fig, "decrochage_median.rds"))
cat("decrochage_median : ok\n")

cat("\n=== pauvrete_relative_ancree.R terminé ===\n")
