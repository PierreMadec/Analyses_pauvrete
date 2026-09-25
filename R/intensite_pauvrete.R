# ==============================================================================
# intensite_pauvrete.R
#
# Intensité de la pauvreté laborieuse : écart médian au seuil, exprimé en % du
# seuil, calculé aux deux seuils (relatif et ancré 2005) sur les personnes de
# référence en emploi de 18 à 64 ans.
#
# Le taux dit combien de personnes passent sous le seuil ; l'intensité dit à
# quelle distance. Les deux se lisent ensemble : au seuil ancré, le taux recule
# depuis 2005 alors que l'intensité progresse.
#
# Figure produite : figure/intensite_tp.rds
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
path_fig <- "figure"

pm  <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
ipc <- setNames(pm$ipc, as.character(pm$annee))

s <- seuils_annuels |>
  mutate(seuil_ancre = seuils_annuels$seuil_std[seuils_annuels$annee == 2005] *
           ipc[as.character(annee)] / ipc["2005"])

# Médiane pondérée
wmed <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

tr <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64, wprm > 0) |>
  left_join(s |> select(annee, seuil_std, seuil_ancre), by = "annee")

intens <- tr |>
  group_by(annee) |>
  summarise(
    `Seuil relatif` = 100 * (1 - wmed(nivviem[nivviem < seuil_std],
                                      wprm[nivviem < seuil_std]) / first(seuil_std)),
    `Seuil ancré (pouvoir d'achat 2005)` =
      100 * (1 - wmed(nivviem[nivviem < seuil_ancre],
                      wprm[nivviem < seuil_ancre]) / first(seuil_ancre)),
    .groups = "drop") |>
  pivot_longer(-annee, names_to = "seuil", values_to = "intensite") |>
  mutate(seuil = factor(seuil, levels = c("Seuil relatif",
                                          "Seuil ancré (pouvoir d'achat 2005)")),
         tooltip = sprintf("%s\n%d : %.1f %% du seuil", seuil, annee, intensite),
         data_id = paste0(gsub("[^a-z]", "", tolower(seuil)), "_", annee))

g <- ggplot(intens, aes(x = annee, y = intensite, colour = seuil, group = seuil)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = c("Seuil relatif" = "#2674DD",
                                 "Seuil ancré (pouvoir d'achat 2005)" = "#8D30D4")) +
  scale_x_continuous(breaks = seq(2005, 2024, 2)) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1),
                     limits = c(15, 25)) +
  labs(x = NULL,
       y = "écart médian au seuil (% du seuil)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2024, calculs de l'auteur.\n",
         "Champ : personnes de référence en emploi de 18 à 64 ans, France métropolitaine.\n",
         "Intensité = écart entre le seuil et le niveau de vie médian des travailleurs pauvres, en % du seuil.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
        legend.position = "bottom", legend.title = element_blank())

saveRDS(g, file.path(path_fig, "intensite_tp.rds"))

cat("\n--- intensité de la pauvreté laborieuse (% du seuil) ---\n")
print(as.data.frame(intens |> select(annee, seuil, intensite) |>
        pivot_wider(names_from = seuil, values_from = intensite) |>
        mutate(across(-annee, ~ round(.x, 1)))), row.names = FALSE)
cat("\n=== intensite_pauvrete.R terminé ===\n")
