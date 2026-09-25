# ==============================================================================
# piste7_pcs_horaire.R
#
# Teste directement l'hypothèse avancée en Partie 2 (fig-salaire-evol) : le
# recul de la part des travailleurs pauvres au-dessus du SMIC horaire
# (51 % en 2013 -> 34 % en 2023) serait-il lié à une recomposition sectorielle
# de l'emploi vers des métiers moins qualifiés (PCS "Ouvriers"/"Employés") ?
#
# Méthode : décomposition shift-share à 2 groupes (Ouvriers/Employés vs autres
# PCS) du taux "salaire horaire >= SMIC" parmi les travailleurs pauvres, entre
# 2013-2015 et 2021-2023 (moyennes triennales, plus robustes que les deux
# années isolées 2013/2023 utilisées pour le chiffre agrégé) :
#
#   Delta = Somme_j (s2_j - s1_j) * rbar_j   [effet composition]
#         + Somme_j sbar_j * (r2_j - r1_j)   [effet "within" / rendement]
#
# où s_j = part du groupe PCS j parmi les travailleurs pauvres, r_j = part du
# groupe j au-dessus du SMIC horaire, rbar_j et sbar_j les moyennes des deux
# périodes (pas de terme d'interaction résiduel).
#
# Champ identique à piste1_salaire_horaire.R : PR en emploi, 18-64 ans, avec
# quotité de temps partiel connue (disponible à partir de 2013).
#
# Figures produites (figure/) :
#   tp_pcs_shiftshare   — décomposition composition / within du recul du taux
#                          au-dessus du SMIC horaire
#   tp_pcs_compo_evol   — évolution de la part Ouvriers/Employés parmi les
#                          travailleurs pauvres, 2013-2023
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
ti <- readRDS("figure/travailleurs_indiv.rds")

path_fig <- "figure"

# ── Base identique à piste1_salaire_horaire.R ─────────────────────────────────
smic_net <- tibble(
  annee = 2013:2023,
  smic_net_annuel = c(13191, 13320, 13464, 13596, 13884, 14424,
                      14976, 15288, 15876, 17472, 18534)
) |> mutate(smic_net_horaire = smic_net_annuel / 1820)

conv_txtp <- c("1" = 0.25, "2" = 0.50, "3" = 0.625, "4" = 0.75, "5" = 0.875)

base <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(pauvre = nivviem < seuil_std)

ti_pr <- ti |>
  filter(lpr == 1) |>
  arrange(annee, ident, noi) |>
  distinct(annee, ident, .keep_all = TRUE) |>
  mutate(
    quotite = case_when(
      temps_partiel == 0                               ~ 1.0,
      temps_partiel == 1 & !is.na(txtp) & txtp %in% 1:5 ~ conv_txtp[as.character(txtp)],
      TRUE ~ NA_real_
    )
  )

tp_indiv <- base |>
  left_join(ti_pr |> select(annee, ident, salaires_i, temps_partiel, quotite),
            by = c("annee", "ident")) |>
  filter(!is.na(quotite), quotite > 0, salaires_i > 0, annee >= 2013) |>
  left_join(smic_net, by = "annee") |>
  mutate(
    heures_ann  = 1820 * quotite,
    sal_horaire = salaires_i / heures_ann,
    ratio_smic  = sal_horaire / smic_net_horaire,
    dessus_smic = ratio_smic >= 1.0
  )

# ── Champ : travailleurs pauvres avec PCS connue ──────────────────────────────
pauvres <- tp_indiv |>
  filter(pauvre) |>
  mutate(pcs_grp = case_when(
    pcs_cat %in% c("Ouvriers", "Employes") ~ "Ouvriers/Employés",
    !is.na(pcs_cat)                        ~ "Autres PCS",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(pcs_grp))

# ==============================================================================
# 1. Décomposition shift-share : composition PCS vs rendement à PCS donnée
# ==============================================================================

p1 <- pauvres |> filter(annee %in% 2013:2015)
p2 <- pauvres |> filter(annee %in% 2021:2023)

compo <- function(df) df |> group_by(pcs_grp) |> summarise(s = sum(wprm), .groups = "drop") |>
  mutate(s = s / sum(s))
taux <- function(df) df |> group_by(pcs_grp) |> summarise(r = weighted.mean(dessus_smic, wprm), .groups = "drop")

tab <- compo(p1) |> rename(s1 = s) |>
  inner_join(compo(p2) |> rename(s2 = s), by = "pcs_grp") |>
  inner_join(taux(p1)  |> rename(r1 = r), by = "pcs_grp") |>
  inner_join(taux(p2)  |> rename(r2 = r), by = "pcs_grp") |>
  mutate(
    rbar  = (r1 + r2) / 2,
    sbar  = (s1 + s2) / 2,
    compo_j  = (s2 - s1) * rbar,
    within_j = sbar * (r2 - r1)
  )

gap_obs      <- weighted.mean(p2$dessus_smic, p2$wprm) - weighted.mean(p1$dessus_smic, p1$wprm)
effet_compo  <- sum(tab$compo_j)
effet_within <- sum(tab$within_j)

cat(sprintf(
  "\nRecul du taux au-dessus du SMIC horaire, 2013-2015 -> 2021-2023 : %+.1f pts\n  dont composition PCS : %+.1f pts\n  dont within (à PCS donnée) : %+.1f pts\n",
  gap_obs * 100, effet_compo * 100, effet_within * 100
))
cat("Part Ouvriers/Employés parmi les travailleurs pauvres :\n")
print(tab |> select(pcs_grp, s1, s2) |> mutate(across(c(s1, s2), ~round(.x * 100, 1))))

# ── Figure A : décomposition composition / within ─────────────────────────────
decomp_pcs <- tibble(
  effet   = factor(
    c("Composition PCS\n(recomposition sectorielle)",
      "Rendement à PCS donnée\n(within)"),
    levels = c("Composition PCS\n(recomposition sectorielle)",
               "Rendement à PCS donnée\n(within)")
  ),
  valeur  = c(effet_compo, effet_within) * 100,
  sens    = if_else(c(effet_compo, effet_within) * 100 >= 0, "Hausse", "Baisse"),
  tooltip = paste0(
    c("Effet composition PCS", "Effet within (à PCS donnée)"),
    " : ", sprintf("%+.2f", c(effet_compo, effet_within) * 100), " pts de %"
  ),
  data_id = c("pcs_compo", "pcs_within")
)

g_pcs_ss <- ggplot(decomp_pcs, aes(x = effet, y = valeur, fill = sens)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = data_id), width = 0.55) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  annotate("text", x = Inf, y = min(decomp_pcs$valeur) * 0.9,
           label = sprintf("Recul total : %+.1f pts", gap_obs * 100),
           hjust = 1.05, size = 3.5, colour = "grey30") +
  scale_fill_manual(values = c("Hausse" = "#1f78b4", "Baisse" = "#e31a1c"), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    x = NULL,
    y = "Contribution au recul de la part au-dessus du SMIC horaire (pts de %)",
    caption = paste0(
      "Source : INSEE, ERFS, calculs de l'auteur. 2013-2015 vs 2021-2023.\n",
      "Champ : PR en emploi pauvres, 18-64 ans, avec quotité et PCS connues.\n",
      "PCS = Ouvriers + Employés vs autres catégories socioprofessionnelles."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0)
  )

saveRDS(g_pcs_ss, file.path(path_fig, "tp_pcs_shiftshare.rds"))
cat("tp_pcs_shiftshare : ok\n")

# ==============================================================================
# 2. Évolution de la composition PCS parmi les travailleurs pauvres
# ==============================================================================

compo_evol <- pauvres |>
  group_by(annee, pcs_grp) |>
  summarise(w = sum(wprm), .groups = "drop") |>
  group_by(annee) |>
  mutate(part = 100 * w / sum(w)) |>
  ungroup() |>
  filter(pcs_grp == "Ouvriers/Employés") |>
  mutate(
    tooltip = paste0(annee, " : ", round(part, 1), " % d'ouvriers/employés"),
    data_id = paste0("pcs_", annee)
  )

g_pcs_evol <- ggplot(compo_evol, aes(x = annee, y = part)) +
  geom_line_interactive(colour = "#6a3d9a", linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id),
                         colour = "#6a3d9a", size = 2.5) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 100)) +
  labs(
    x = NULL, y = "Part d'ouvriers/employés parmi les\ntravailleurs pauvres (%)",
    caption = paste0(
      "Source : INSEE, ERFS 2013-2023, calculs de l'auteur.\n",
      "Champ : PR en emploi pauvres, 18-64 ans, avec quotité et PCS connues."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(g_pcs_evol, file.path(path_fig, "tp_pcs_compo_evol.rds"))
cat("tp_pcs_compo_evol : ok\n")

cat("\n=== piste7_pcs_horaire.R terminé ===\n")
