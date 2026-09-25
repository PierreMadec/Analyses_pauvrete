# ==============================================================================
# decompo_emploi_pauvrete.R
#
# Décomposition comptable (shift-share) de la variation du taux de pauvreté
# monétaire des 18-64 ans entre 2016 et 2024, au seuil relatif.
#
# P = s_emploi . p_emploi + s_horsemploi . p_horsemploi
# ΔP = Σ Δs.p0   (effet structure = hausse du taux d'emploi)
#    + Σ s0.Δp   (effet taux, décliné en "en emploi" / "sans emploi")
#    + Σ Δs.Δp   (interaction)
#
# Figure produite : figure/decompo_emploi_pauvrete.rds
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
path_fig <- "figure"

A0 <- 2016L; A1 <- 2024L

pop <- data_all |>
  filter(age_num >= 18, age_num <= 64, wprm > 0, !is.na(acteu_ind),
         annee %in% c(A0, A1)) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(grp    = ifelse(acteu_ind == "Emploi", "En emploi", "Sans emploi"),
         pauvre = nivviem < seuil_std)

agg <- pop |>
  group_by(annee, grp) |>
  summarise(w = sum(wprm), p = weighted.mean(pauvre, wprm, na.rm = TRUE), .groups = "drop_last") |>
  mutate(s = w / sum(w)) |>
  ungroup()

t0 <- agg |> filter(annee == A0)
t1 <- agg |> filter(annee == A1)
m  <- full_join(t0 |> select(grp, s0 = s, p0 = p),
                t1 |> select(grp, s1 = s, p1 = p), by = "grp")

P0 <- sum(m$s0 * m$p0); P1 <- sum(m$s1 * m$p1)
eff_struct <- sum((m$s1 - m$s0) * m$p0)
eff_taux_e <- with(m[m$grp == "En emploi", ],  s0 * (p1 - p0))
eff_taux_s <- with(m[m$grp == "Sans emploi", ], s0 * (p1 - p0))
inter      <- sum((m$s1 - m$s0) * (m$p1 - m$p0))

d <- tibble(
  poste = factor(c("Hausse du taux d'emploi",
                   "Pauvreté des personnes en emploi",
                   "Pauvreté des personnes sans emploi",
                   "Interaction",
                   "Variation observée"),
                 levels = rev(c("Hausse du taux d'emploi",
                                "Pauvreté des personnes en emploi",
                                "Pauvreté des personnes sans emploi",
                                "Interaction",
                                "Variation observée"))),
  val = 100 * c(eff_struct, eff_taux_e, eff_taux_s, inter, P1 - P0),
  type = c("contribution", "contribution", "contribution", "contribution", "total")
)

pal <- c("contribution" = "#2674DD", "total" = "#8D30D4")

g <- ggplot(d, aes(y = poste, x = val, fill = type,
                   tooltip = sprintf("%s : %+.1f point", poste, val),
                   data_id = poste)) +
  geom_col_interactive(width = 0.6) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", val),
                hjust = ifelse(val >= 0, -0.2, 1.2)),
            size = 3.2, colour = "grey20") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.1, style_positive = "plus"),
                     expand = expansion(mult = 0.18)) +
  labs(x = "contribution à la variation du taux de pauvreté des 18-64 ans (points)",
       y = NULL,
       caption = paste0("Source : INSEE, ERFS 2016 et 2024, calculs de l'auteur.\n",
                        "Champ : personnes de 18 à 64 ans, France métropolitaine. Seuil relatif.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g, file.path(path_fig, "decompo_emploi_pauvrete.rds"))

cat(sprintf("taux de pauvreté 18-64 : %.1f (%d) -> %.1f (%d)\n", 100*P0, A0, 100*P1, A1))
cat(sprintf("  part en emploi : %.1f -> %.1f\n", 100*m$s0[m$grp=="En emploi"], 100*m$s1[m$grp=="En emploi"]))
cat(sprintf("  p(en emploi)   : %.1f -> %.1f\n", 100*m$p0[m$grp=="En emploi"], 100*m$p1[m$grp=="En emploi"]))
cat(sprintf("  p(sans emploi) : %.1f -> %.1f\n", 100*m$p0[m$grp=="Sans emploi"], 100*m$p1[m$grp=="Sans emploi"]))
print(d)
