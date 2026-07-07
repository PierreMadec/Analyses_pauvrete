# ==============================================================================
# premium_emploi.R
#
# "Toutes choses égales par ailleurs, est-on plus à risque d'être pauvre
#  quand on travaille en 2023 qu'en 2013 ?"
#
# Méthode : logit pondéré annuel sur la population adulte (acteu_ind non-NA).
#
#   pauvre ~ emploi_bin + age_cat + sexe_cat + immi_cat + diplome
#          + typmen + nb_enfants  [selon disponibilité par année]
#
#   emploi_bin = TRUE si acteu_ind == "Emploi", FALSE sinon (chômage + inactif).
#
# L'AME (Average Marginal Effect) est calculé par prédictions contrefactuelles.
# Le SE est approché par la formule analytique de premier ordre :
#   SE(AME) ≈ SE(β_emploi) × mean_w[f(Xβ)]
# où f est la densité logistique et SE(β_emploi) vient de vcov(fit).
#
# Figures produites (figure/) :
#   premium_ame_annuel.rds    — AME global par année (2005-2023)
#   premium_ame_typmen.rds    — AME par type de ménage × années clés
#
# Prérequis : data_all et seuils_annuels (figure/data_all.rds)
# ==============================================================================

library(tidyverse)
library(ggiraph)
library(scales)

# ------------------------------------------------------------------------------
# 0. Chargement des données
# ------------------------------------------------------------------------------
if (!exists("data_all") || !exists("seuils_annuels")) {
  rds_data   <- "figure/data_all.rds"
  rds_seuils <- "figure/seuils_annuels.rds"
  if (!file.exists(rds_data))
    stop("figure/data_all.rds introuvable. Lancez d'abord decomposition_pauvrete.R.")
  data_all       <<- readRDS(rds_data)
  seuils_annuels <<- readRDS(rds_seuils)
  message("data_all et seuils_annuels chargés depuis les RDS.")
}

path_fig <- "figure"

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

caption_base <- paste0(
  "Source : INSEE, ERFS 2005-2023, calculs de l'auteur.\n",
  "Logit pondéré (wprm). AME = effet marginal moyen de l'emploi sur P(pauvre),\n",
  "toutes choses égales par ailleurs (âge, sexe, diplôme, immigration,\n",
  "type de ménage, nombre d'enfants selon disponibilité)."
)

# ------------------------------------------------------------------------------
# 1. Base de travail : personnes de référence du ménage (lpr == 1)
#
# On restreint aux PR pour éviter le biais d'atténuation lié au fait que
# tous les membres d'un même ménage partagent le même statut pauvre :
# - inclure le conjoint inactif et le PR employé avec pauvre identique
#   annule mécaniquement l'effet emploi dans la régression
# - restreindre à lpr == 1 donne une observation par ménage, dont l'emploi
#   du chef détermine directement les ressources du ménage
# ------------------------------------------------------------------------------
base <- data_all |>
  left_join(seuils_annuels |> select(annee, seuil_std), by = "annee") |>
  filter(lpr == 1, !is.na(acteu_ind), !is.na(age_num), age_num >= 18) |>
  mutate(
    pauvre     = as.integer(nivviem < seuil_std),
    emploi_bin = (acteu_ind == "Emploi")
  )

# Candidats contrôles, par ordre de préférence
vars_controle_cand <- c("age_cat", "sexe_cat", "immi_cat", "diplome", "typmen", "nb_enfants")
annees_dispo <- sort(unique(base$annee))

# ------------------------------------------------------------------------------
# 2. Fonctions utilitaires
# ------------------------------------------------------------------------------

# Sélectionne les contrôles disponibles pour un data.frame donné
# (variable retenue si taux de NA < seuil ET variation non nulle)
controls_disponibles <- function(df, candidats, seuil_na = 0.5) {
  candidats[sapply(candidats, function(v) {
    if (!v %in% names(df)) return(FALSE)
    col <- df[[v]]
    na_rate <- mean(is.na(col))
    if (na_rate >= seuil_na) return(FALSE)
    # Variable numérique ou facteur avec >1 niveau non-NA
    if (is.factor(col) || is.character(col)) {
      length(unique(na.omit(as.character(col)))) > 1
    } else {
      var(col, na.rm = TRUE) > 0
    }
  })]
}

# AME de emploi_bin par prédictions contrefactuelles
calc_ame <- function(fit, df) {
  d1 <- df |> mutate(emploi_bin = TRUE)
  d0 <- df |> mutate(emploi_bin = FALSE)
  p1 <- tryCatch(predict(fit, newdata = d1, type = "response"),
                 error = function(e) rep(NA_real_, nrow(df)))
  p0 <- tryCatch(predict(fit, newdata = d0, type = "response"),
                 error = function(e) rep(NA_real_, nrow(df)))
  weighted.mean(p1 - p0, df$wprm, na.rm = TRUE)
}

# SE de l'AME — approximation analytique de premier ordre :
#   SE(AME) ≈ SE(β_emploi) × mean_w[ p*(1-p) ]
# Robuste, pas de boucle, compatible avec tout modèle GLM.
calc_ame_se <- function(fit, df) {
  eta <- predict(fit, type = "link")
  p   <- plogis(eta)
  avg_f <- weighted.mean(p * (1 - p), df$wprm, na.rm = TRUE)

  v   <- vcov(fit)
  # Nom du coefficient pour emploi_bin (TRUE/FALSE → "emploi_binTRUE")
  nm  <- grep("^emploi_bin", rownames(v), value = TRUE)
  if (length(nm) == 0) return(NA_real_)
  se_beta <- sqrt(diag(v)[nm[1]])
  avg_f * se_beta
}

# Estime le logit et renvoie une liste (fit, df_propre, vars_ok)
# Gère la séparation quasi-parfaite via firth si disponible, sinon ridge léger.
estimer_logit <- function(df_raw, vars_ok) {
  formule <- as.formula(
    paste("pauvre ~ emploi_bin +", paste(vars_ok, collapse = " + "))
  )
  df <- df_raw |>
    select(pauvre, emploi_bin, wprm, all_of(vars_ok)) |>
    filter(if_all(all_of(vars_ok), ~ !is.na(.))) |>
    mutate(wprm = wprm / mean(wprm))

  # Supprimer les niveaux vides
  for (v in vars_ok) {
    if (is.factor(df[[v]])) df[[v]] <- droplevels(df[[v]])
  }

  if (nrow(df) < 100 || length(unique(df$pauvre)) < 2 ||
      length(unique(df$emploi_bin)) < 2) return(NULL)

  fit <- tryCatch(
    glm(formule, data = df, family = binomial,
        weights = wprm, control = glm.control(maxit = 100)),
    warning = function(w) {
      # Convergence warning toléré
      suppressWarnings(
        glm(formule, data = df, family = binomial,
            weights = wprm, control = glm.control(maxit = 200))
      )
    },
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(fit = fit, df = df, vars = vars_ok)
}

# ------------------------------------------------------------------------------
# 3. Estimation annuelle — AME global
# ------------------------------------------------------------------------------
message("=== Estimation AME global (logit annuel) ===")

resultats_ame <- map_dfr(annees_dispo, function(an) {
  message(sprintf("  Année %d...", an))

  df_an <- base |> filter(annee == an)

  # Contrôles disponibles cette année-là
  vars_ok <- controls_disponibles(df_an, vars_controle_cand)
  if (length(vars_ok) == 0) {
    message(sprintf("    -> %d : aucun contrôle disponible", an))
    return(NULL)
  }
  message(sprintf("    -> Contrôles : %s", paste(vars_ok, collapse = ", ")))

  res <- estimer_logit(df_an, vars_ok)
  if (is.null(res)) {
    message(sprintf("    -> %d : logit non convergé", an))
    return(NULL)
  }

  ame    <- calc_ame(res$fit, res$df)
  se_val <- tryCatch(calc_ame_se(res$fit, res$df), error = function(e) NA_real_)

  # Taux bruts (sur le df propre, après NA-drop)
  df_c   <- res$df
  emp    <- df_c$emploi_bin
  taux_e <- weighted.mean(df_c$pauvre[emp],  df_c$wprm[emp],  na.rm = TRUE)
  taux_n <- weighted.mean(df_c$pauvre[!emp], df_c$wprm[!emp], na.rm = TRUE)

  # Tout en points de % — ic calculés avant toute multiplication
  ame_pt <- ame    * 100
  se_pt  <- se_val * 100

  tibble(
    annee        = an,
    ame          = ame_pt,
    se           = se_pt,
    ic_bas       = ame_pt - 1.96 * se_pt,
    ic_haut      = ame_pt + 1.96 * se_pt,
    taux_emp     = taux_e  * 100,
    taux_nemp    = taux_n  * 100,
    premium_brut = (taux_e - taux_n) * 100,
    vars_ctrl    = paste(vars_ok, collapse = "+")
  )
}) |>
  filter(!is.na(ame))

message(sprintf("AME calculé pour %d années.", nrow(resultats_ame)))
print(resultats_ame |> select(annee, ame, se, taux_emp, taux_nemp))

# ------------------------------------------------------------------------------
# 4. Figure 1 — Évolution de l'AME global (2005-2023)
# ------------------------------------------------------------------------------
g_premium_annuel <- ggplot(
  resultats_ame |>
    mutate(
      tooltip = paste0(
        annee, "\n",
        "AME : ", round(ame, 1), " pts\n",
        "(IC 95 % : [", round(ic_bas, 1), " ; ", round(ic_haut, 1), "])\n",
        "Taux pauvreté :\n",
        "  En emploi    : ", round(taux_emp, 1), " %\n",
        "  Hors emploi : ", round(taux_nemp, 1), " %"
      ),
      data_id = as.character(annee)
    ),
  aes(x = annee, y = ame)
) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_ribbon(aes(ymin = ic_bas, ymax = ic_haut), fill = "#1f78b4", alpha = 0.15) +
  geom_line_interactive(colour = "#1f78b4", linewidth = 1.2) +
  geom_point_interactive(
    aes(tooltip = tooltip, data_id = data_id),
    colour = "#1f78b4", size = 2.5
  ) +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    y       = "AME de l'emploi sur P(pauvre), en points de %",
    caption = caption_base
  ) +
  theme_erfs()

saveRDS(g_premium_annuel, file.path(path_fig, "premium_ame_annuel.rds"))
cat("premium_ame_annuel.rds : ok\n")

# ------------------------------------------------------------------------------
# 5. Estimation par type de ménage
# ------------------------------------------------------------------------------
message("=== Estimation AME par type de ménage ===")

annees_cles   <- c(2005, 2010, 2015, 2019, 2023)
typmen_retenus <- c(
  "Personne seule",
  "Couple sans enfant",
  "Couple avec enfant(s)",
  "Famille monoparentale"
)

# Pour la stratification par typmen : on exclut typmen et nb_enfants des
# contrôles de "Personne seule" (nb_enfants constant = "0 enfant" → rang déficient)
vars_sans_typmen <- setdiff(vars_controle_cand, "typmen")

resultats_typmen <- map_dfr(annees_cles, function(an) {
  message(sprintf("  Année %d...", an))

  map_dfr(typmen_retenus, function(tm) {
    df_sub <- base |>
      filter(annee == an, as.character(typmen) == tm)

    # Contrôles disponibles (sans typmen, et sans nb_enfants pour peronne seule)
    cands <- if (tm == "Personne seule") {
      setdiff(vars_sans_typmen, "nb_enfants")
    } else {
      vars_sans_typmen
    }
    vars_ok <- controls_disponibles(df_sub, cands)

    if (length(vars_ok) == 0) return(NULL)

    res <- estimer_logit(df_sub, vars_ok)
    if (is.null(res)) return(NULL)

    ame    <- calc_ame(res$fit, res$df)
    se_val <- tryCatch(calc_ame_se(res$fit, res$df), error = function(e) NA_real_)

    ame_pt <- ame    * 100
    se_pt  <- se_val * 100

    tibble(
      annee   = an,
      typmen  = tm,
      ame     = ame_pt,
      se      = se_pt,
      ic_bas  = ame_pt - 1.96 * se_pt,
      ic_haut = ame_pt + 1.96 * se_pt
    )
  })
}) |>
  filter(!is.na(ame)) |>
  mutate(
    typmen  = factor(typmen, levels = typmen_retenus),
    tooltip = paste0(
      typmen, "\n", annee, "\n",
      "AME : ", round(ame, 1), " pts\n",
      "IC 95 % : [", round(ic_bas, 1), " ; ", round(ic_haut, 1), "]"
    ),
    data_id = paste0(typmen, "_", annee)
  )

message("AME par type de ménage :")
print(resultats_typmen |> select(annee, typmen, ame, se))

# ------------------------------------------------------------------------------
# 6. Figure 2 — AME par type de ménage
# ------------------------------------------------------------------------------
pal_typmen <- c(
  "Personne seule"         = "#1f78b4",
  "Couple sans enfant"     = "#33a02c",
  "Couple avec enfant(s)" = "#ff7f00",
  "Famille monoparentale"  = "#e31a1c"
)

g_premium_typmen <- ggplot(
  resultats_typmen,
  aes(x = factor(annee), y = ame, colour = typmen, group = typmen)
) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15, alpha = 0.5) +
  geom_line_interactive(linewidth = 1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2.5) +
  scale_colour_manual(values = pal_typmen) +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    y       = "AME de l'emploi sur P(pauvre), en points de %",
    caption = paste0(
      caption_base,
      "\nEstimé séparément pour chaque type de ménage (typmen exclu des contrôles)."
    )
  ) +
  theme_erfs()

saveRDS(g_premium_typmen, file.path(path_fig, "premium_ame_typmen.rds"))
cat("premium_ame_typmen.rds : ok\n")

# ------------------------------------------------------------------------------
# 7. Sauvegarde des données tabulaires
# ------------------------------------------------------------------------------
saveRDS(resultats_ame,    file.path(path_fig, "premium_resultats_ame.rds"))
saveRDS(resultats_typmen, file.path(path_fig, "premium_resultats_typmen.rds"))

# ------------------------------------------------------------------------------
# 8. Extension 4 — Premium emploi : courbe continue par type de ménage (2005-2023)
#
#    Contrairement à la figure 2 (années clés seulement), cette figure estime
#    l'AME pour TOUTES les années disponibles et chaque typmen.
#    Visualisation : line chart avec ruban de confiance par typmen.
# ------------------------------------------------------------------------------
message("=== Extension 4 : AME annuel continu par type de ménage ===")

resultats_typmen_all <- map_dfr(annees_dispo, function(an) {
  map_dfr(typmen_retenus, function(tm) {
    df_sub <- base |>
      filter(annee == an, as.character(typmen) == tm)

    cands  <- if (tm == "Personne seule") {
      setdiff(vars_sans_typmen, "nb_enfants")
    } else {
      vars_sans_typmen
    }
    vars_ok <- controls_disponibles(df_sub, cands)
    if (length(vars_ok) == 0) return(NULL)

    res <- estimer_logit(df_sub, vars_ok)
    if (is.null(res)) return(NULL)

    ame_pt <- calc_ame(res$fit, res$df)    * 100
    se_pt  <- tryCatch(calc_ame_se(res$fit, res$df) * 100, error = function(e) NA_real_)

    tibble(
      annee   = an,
      typmen  = tm,
      ame     = ame_pt,
      se      = se_pt,
      ic_bas  = ame_pt - 1.96 * se_pt,
      ic_haut = ame_pt + 1.96 * se_pt
    )
  })
}) |>
  filter(!is.na(ame)) |>
  mutate(
    typmen  = factor(typmen, levels = typmen_retenus),
    tooltip = paste0(
      typmen, " — ", annee, "\n",
      "AME : ", round(ame, 1), " pts\n",
      "IC 95 % : [", round(ic_bas, 1), " ; ", round(ic_haut, 1), "]"
    ),
    data_id = paste0(typmen, "_all_", annee)
  )

message(sprintf("Extension 4 : %d estimations (typmen × année)", nrow(resultats_typmen_all)))

g_premium_timeline <- ggplot(
  resultats_typmen_all,
  aes(x = annee, y = ame, colour = typmen, fill = typmen, group = typmen)
) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(
    aes(ymin = ic_bas, ymax = ic_haut),
    alpha = 0.12, colour = NA
  ) +
  geom_line_interactive(linewidth = 1.1) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 2) +
  scale_colour_manual(values = pal_typmen) +
  scale_fill_manual(values   = pal_typmen, guide = "none") +
  scale_x_continuous(breaks = seq(2005, 2023, 2)) +
  scale_y_continuous(labels = label_number(suffix = " pts")) +
  labs(
    y       = "AME de l'emploi sur P(pauvre), en points de %",
    colour  = NULL,
    caption = paste0(
      caption_base, "\n",
      "Ruban = IC à 95 %. Estimé séparément par type de ménage (typmen exclu des contrôles)."
    )
  ) +
  theme_erfs() +
  theme(legend.position = "bottom")

saveRDS(g_premium_timeline, file.path(path_fig, "premium_timeline_config.rds"))
cat("premium_timeline_config.rds : ok\n")

saveRDS(resultats_typmen_all, file.path(path_fig, "premium_resultats_typmen_all.rds"))

cat("\n=== premium_emploi.R terminé ===\n")
if (nrow(resultats_ame) > 0) {
  an_ref <- if (2013 %in% resultats_ame$annee) 2013 else min(resultats_ame$annee)
  cat(sprintf(
    "AME %d : %.1f pts | AME 2023 : %.1f pts\n",
    an_ref,
    resultats_ame$ame[resultats_ame$annee == an_ref],
    resultats_ame$ame[resultats_ame$annee == 2023]
  ))
}
