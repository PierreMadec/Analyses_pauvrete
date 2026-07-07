# ==============================================================================
# decompo_revenu_tp.R
#
# Deux décompositions de revenu :
#
# D. Recul de la pauvreté laborieuse ANCRÉE par couche de revenu.
#    Taux de pauvreté (seuil ancré) des PR en emploi calculé sur :
#      - les revenus de marché seuls (rev_avant_redist),
#      - + l'enveloppe de soutien aux bas revenus d'activité (PPE+RSA act.+PPA),
#      - le revenu disponible observé.
#    Les écarts entre couches mesurent la contribution de chaque bloc au recul.
#
# E. Décrochage relatif : pourquoi le niveau de vie médian progresse-t-il plus
#    vite que celui des travailleurs modestes ? Décomposition de la croissance
#    réelle du revenu du ménage médian par source (salaires / autres revenus
#    primaires / transferts nets), comparée à celle des travailleurs pauvres.
#
# Figures produites (figure/) :
#   decompo_ancree_couches — pauvreté ancrée par couche de revenu (2005-2023)
#   decrochage_sources     — croissance réelle du revenu médian par source vs bottom
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

path_fig <- "figure"
trav <- readRDS(file.path(path_fig, "travailleurs_indiv.rds"))
d    <- readRDS(file.path(path_fig, "data_all.rds"))
s    <- readRDS(file.path(path_fig, "seuils_annuels.rds"))
ppe  <- readRDS(file.path(path_fig, "ppe_2005_2015.rds"))
pm   <- read_csv("data/parametres_macro.csv", show_col_types = FALSE)
ipc  <- setNames(pm$ipc, as.character(pm$annee))

seuil_rel_2005 <- s$seuil_std[s$annee == 2005]
s <- s |> mutate(seuil_ancre = seuil_rel_2005 * ipc[as.character(annee)] / ipc["2005"])
defl <- function(x, an) x / (ipc[as.character(an)] / ipc["2005"])

# Salaire total du ménage (somme des salaires individuels)
sal_hh <- trav |> group_by(annee, ident) |>
  summarise(sal_hh = sum(salaires_i, na.rm = TRUE), .groups = "drop")

hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  left_join(ppe, by = c("annee", "ident")) |>
  left_join(sal_hh, by = c("annee", "ident")) |>
  mutate(across(c(ppe, m_rsa_actm, ppa), ~ coalesce(as.numeric(.), 0)),
         sal_hh = coalesce(sal_hh, 0),
         env = ppe + m_rsa_actm + ppa) |>
  left_join(s, by = "annee")

theme_erfs <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, colour = "grey50", hjust = 0),
          legend.position = "bottom", legend.title = element_blank())
}

# ==============================================================================
# D. Pauvreté laborieuse ancrée par couche de revenu
# ==============================================================================
base_D <- trav |> filter(lpr == 1, emploi == 1, age >= 18, age <= 64) |>
  select(annee, ident) |> inner_join(hh, by = c("annee", "ident"))

pal_D <- c("Revenus d'activité et de marché seuls"        = "#e31a1c",
           "+ soutien aux bas revenus d'activité"          = "#ff7f00",
           "Revenu disponible (après redistribution)"      = "#1f78b4")

D <- base_D |> group_by(annee) |>
  summarise(
    `Revenus d'activité et de marché seuls`   = 100 * weighted.mean(rev_avant_redist / nb_uci < seuil_ancre, wprm),
    `+ soutien aux bas revenus d'activité`     = 100 * weighted.mean((rev_avant_redist + env) / nb_uci < seuil_ancre, wprm),
    `Revenu disponible (après redistribution)` = 100 * weighted.mean(nivviem < seuil_ancre, wprm),
    .groups = "drop") |>
  pivot_longer(-annee, names_to = "couche", values_to = "taux") |>
  mutate(couche = factor(couche, levels = names(pal_D)),
         tooltip = paste0(couche, "\n", annee, " : ", round(taux, 1), " %"),
         data_id = paste0(gsub("[^a-z]", "", tolower(couche)), "_", annee))

g_D <- ggplot(D, aes(x = annee, y = taux, colour = couche, group = couche)) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = pal_D) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(x = NULL, y = "Taux de pauvreté laborieuse ancré (%)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Seuil ancré (pouvoir d'achat 2005). L'écart entre deux courbes mesure la contribution ",
         "du bloc de revenu intercalé. Champ : PR en emploi, 18-64 ans.")) +
  theme_erfs()
saveRDS(g_D, file.path(path_fig, "decompo_ancree_couches.rds"))
cat("decompo_ancree_couches : ok\n")

# ==============================================================================
# E. Décrochage : croissance réelle du revenu médian par source
# ==============================================================================
comp_hh <- d |> distinct(annee, ident, .keep_all = TRUE) |>
  left_join(sal_hh, by = c("annee", "ident")) |>
  left_join(s, by = "annee") |>
  mutate(sal_hh = coalesce(sal_hh, 0),
         labor       = sal_hh / nb_uci,
         autres_prim = (rev_avant_redist - sal_hh) / nb_uci,
         transf      = (revdispm - rev_avant_redist) / nb_uci)

# Ménage médian : tranche D45-D55 du niveau de vie chaque année
med <- comp_hh |> group_by(annee) |>
  filter(nivviem >= quantile(nivviem, .45, na.rm = TRUE),
         nivviem <= quantile(nivviem, .55, na.rm = TRUE)) |>
  summarise(`Salaires` = mean(labor), `Autres revenus primaires` = mean(autres_prim),
            .groups = "drop") |>
  mutate(across(-annee, ~ defl(.x, annee)))

E <- med |>
  pivot_longer(-annee, names_to = "source", values_to = "val") |>
  group_by(source) |>
  mutate(indice = 100 * val / val[annee == 2005]) |>
  ungroup() |>
  mutate(source = factor(source, levels = c("Salaires", "Autres revenus primaires")),
         tooltip = paste0(source, "\n", annee, " : ", round(indice), " (base 100 = 2005)\n",
                          round(val), " €/UC (réel)"),
         data_id = paste0(gsub("[^a-z]", "", tolower(source)), "_", annee))

g_E <- ggplot(E, aes(x = annee, y = indice, colour = source, group = source)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = "grey70") +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.2) +
  scale_colour_manual(values = c("Salaires" = "#1f78b4",
                                 "Autres revenus primaires" = "#33a02c")) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  labs(x = NULL, y = "Revenu réel du ménage médian (indice base 100 = 2005)",
       caption = paste0(
         "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
         "Ménage médian = tranche D45-D55 du niveau de vie. Autres revenus primaires : ",
         "pensions de retraite, revenus du capital et des indépendants. Déflaté par l'IPC.")) +
  theme_erfs()
saveRDS(g_E, file.path(path_fig, "decrochage_sources.rds"))
cat("decrochage_sources : ok\n")

# Contrôles
cat("\n--- D : pauvreté ancrée par couche ---\n")
print(as.data.frame(D |> filter(annee %in% c(2005, 2023)) |>
        select(annee, couche, taux) |> mutate(taux = round(taux, 1))), row.names = FALSE)
cat("\n--- E : croissance réelle 2005->2023 du revenu médian par source ---\n")
print(as.data.frame(E |> filter(annee == 2023) |> select(source, indice) |>
        mutate(croissance_pct = round(indice - 100))), row.names = FALSE)
cat("\n=== decompo_revenu_tp.R terminé ===\n")
