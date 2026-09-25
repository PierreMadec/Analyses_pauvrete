# ==============================================================================
# ecart_seuil_config.R
#
# Profondeur de la pauvreté laborieuse : distribution de l'écart au seuil de
# pauvreté, par configuration de ménage (PR en emploi pauvres, 2022-2024).
#
# Écart au seuil = (seuil_std - nivviem) / seuil_std, réparti en trois bandes :
#   moins de 10 %  |  10 à 30 %  |  plus de 30 %  sous le seuil.
#
# Descriptif pur : aucune maquette, aucun archétype — uniquement data_all.
#
# Figure produite : figure/ecart_seuil_config.rds
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

if (!exists("data_all") || !exists("seuils_annuels")) {
  data_all       <<- readRDS("figure/data_all.rds")
  seuils_annuels <<- readRDS("figure/seuils_annuels.rds")
}
path_fig <- "figure"

lab_levels <- c("Personne seule", "Famille monoparentale",
                "Couple monoactif sans enfant", "Couple monoactif avec enfant(s)",
                "Couple biactif avec enfant(s)")

tp <- data_all |>
  filter(lpr == 1, acteu_ind == "Emploi", age_num >= 18, age_num <= 64,
         wprm > 0, annee %in% 2022:2024) |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  mutate(
    config = case_when(
      typmen == "Personne seule"                                   ~ "Personne seule",
      typmen == "Famille monoparentale"                            ~ "Famille monoparentale",
      typmen == "Couple sans enfant"    & biactivite != "Bi-actif" ~ "Couple monoactif sans enfant",
      typmen == "Couple avec enfant(s)" & biactivite != "Bi-actif" ~ "Couple monoactif avec enfant(s)",
      typmen == "Couple avec enfant(s)" & biactivite == "Bi-actif" ~ "Couple biactif avec enfant(s)",
      TRUE ~ NA_character_)
  ) |>
  filter(nivviem < seuil_std, !is.na(config)) |>
  mutate(
    ecart = 100 * (seuil_std - nivviem) / seuil_std,
    bande = cut(ecart, c(-Inf, 10, 30, Inf),
                labels = c("moins de 10 %", "10 à 30 %", "plus de 30 %")),
    config = factor(config, levels = lab_levels)
  )

# ── Parts par bande et écart médian, par configuration ───────────────────────
parts <- tp |>
  group_by(config, bande) |>
  summarise(w = sum(wprm), .groups = "drop_last") |>
  mutate(pct = 100 * w / sum(w)) |>
  ungroup()

# médiane pondérée (les médianes brutes surpondèrent les petits ménages)
wmed <- function(x, w) {
  i <- !is.na(x) & !is.na(w); x <- x[i]; w <- w[i]
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

med <- tp |>
  group_by(config) |>
  summarise(ecart_med = wmed(ecart, wprm),
            eur_uc    = wmed((seuil_std - nivviem) / 12, wprm),
            .groups = "drop")

# ordre : de la pauvreté la moins profonde (haut) à la plus profonde (bas)
ord <- med |> arrange(ecart_med) |> pull(config) |> as.character()
parts <- parts |> mutate(config = factor(as.character(config), levels = rev(ord)))
med   <- med   |> mutate(config = factor(as.character(config), levels = rev(ord)))

pal_bande <- c("moins de 10 %" = "#86C6FF",   # B2
               "10 à 30 %"     = "#2674DD",   # B5
               "plus de 30 %"  = "#042F80")   # B8

parts <- parts |>
  mutate(tooltip = sprintf("%s\n%s sous le seuil : %.0f %%", config, bande, pct))

g <- ggplot(parts, aes(y = config, x = pct, fill = bande,
                       tooltip = tooltip, data_id = paste(config, bande))) +
  geom_col_interactive(width = 0.66, position = position_stack(reverse = TRUE)) +
  geom_text(data = med,
            aes(y = config, x = 102, label = sprintf("écart médian : %.0f %%", ecart_med)),
            inherit.aes = FALSE, hjust = 0, size = 3.1, colour = "grey30") +
  scale_fill_manual(values = pal_bande, name = "niveau de vie sous le seuil de :",
                    guide = guide_legend(reverse = FALSE)) +
  scale_x_continuous(labels = label_number(suffix = " %"),
                     breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, 0.28))) +
  labs(
    x = "part des travailleurs pauvres de la configuration",
    y = NULL,
    caption = paste0("Source : INSEE, ERFS 2022-2024, calculs de l'auteur.\n",
                     "Champ : PR en emploi 18-64 ans, ménage pauvre. ",
                     "Écart = (seuil - niveau de vie) / seuil.")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0))

saveRDS(g, file.path(path_fig, "ecart_seuil_config.rds"))

cat("ecart_seuil_config : ok\n")
print(as.data.frame(
  med |> left_join(
    parts |> select(config, bande, pct) |>
      pivot_wider(names_from = bande, values_from = pct),
    by = "config")), digits = 3)
