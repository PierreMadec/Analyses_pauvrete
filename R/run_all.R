# ==============================================================================
# run_all.R — Script maître du projet ERFS
#
# Exécute l'intégralité du pipeline dans l'ordre :
#   1. Chargement des bases ERFS 2005-2023 (une seule fois)
#   2. Analyse décomposition de la pauvreté
#   3. Analyse du reste à vivre
#   4. Génération des graphiques décomposition
#   5. Génération des graphiques reste à vivre
#   6. Rendu du rapport Quarto (HTML + PDF)
#
# Prérequis :
#   - Packages R : tidyverse, haven, ggiraph, Hmisc, quarto
#   - Données ERFS 2005-2023 et BDF 2017 dans le répertoire du projet
# ==============================================================================

message("=== ERFS — Démarrage du pipeline ===\n")

# 1. Chargement des données ERFS (une seule fois en mémoire)
message("--- Étape 1/6 : Chargement des bases ERFS ---")
#source("charger_erfs.R")

# 3. Analyse décomposition de la pauvreté
message("\n--- Étape 2/6 : Décomposition de la pauvreté ---")
source("decomposition_pauvrete.R")

# 4. Analyse du reste à vivre
message("\n--- Étape 3/6 : Reste à vivre ---")
source("reste_a_vivre.R")

# 5. Graphiques décomposition
message("\n--- Étape 4/6 : Graphiques décomposition ---")
source("graphs_decomposition.R")

# 6. Graphiques reste à vivre
message("\n--- Étape 5/6 : Graphiques reste à vivre ---")
source("graphs_reste_a_vivre.R")

# 7. Rendu du rapport Quarto
message("\n--- Étape 6/6 : Rendu du rapport Quarto ---")
quarto::quarto_render("WP_dynamiques_pauvrete_2005_2023.qmd")

message("\n=== Pipeline terminé ===")
message("Rapport disponible : WP_dynamiques_pauvrete_2005_2023.html / WP_dynamiques_pauvrete_2005_2023.pdf")

