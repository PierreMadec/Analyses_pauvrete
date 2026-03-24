# ERFS — Analyse de la pauvreté monétaire en France (2005-2023)

Analyse longitudinale de la pauvreté monétaire en France à partir des Enquêtes Revenus Fiscaux et Sociaux (ERFS), couvrant la période 2005-2023.

**Auteur** : Pierre Madec, OFCE — Sciences Po
**Rapport** : `analyse_pauvrete.html` / `analyse_pauvrete.pdf`

---

## Prérequis

- **R** ≥ 4.2 et les packages : `tidyverse`, `haven`, `ggiraph`, `Hmisc`, `quarto`
- **Quarto** ≥ 1.4 (pour le rendu du rapport)
- Accès aux données ERFS 2005-2023 et BDF 2017 (format SAS7BDAT ou DTA)

---

## Configuration

Aucune configuration nécessaire. Le projet utilise un fichier `.Rproj` — tous les chemins sont relatifs à la racine du projet (données, graphiques, BDF).

---

## Exécution

### Pipeline complet (recommandé)

```r
source("run_all.R")
```

Ce script orchestre l'ensemble du pipeline dans l'ordre et charge les données **une seule fois**.

### Exécution étape par étape

```r
source("charger_erfs.R")             # 1. Chargement ERFS 2005-2023 (~15-20 min)
source("decomposition_pauvrete.R")   # 2. Analyse décomposition
source("reste_a_vivre.R")            # 3. Analyse reste à vivre
source("graphs_decomposition.R")     # 4. Graphiques décomposition
source("graphs_reste_a_vivre.R")     # 5. Graphiques reste à vivre
quarto::quarto_render("analyse_pauvrete.qmd")  # 6. Rapport
```

---

## Structure du projet

```
ERFS/
├── run_all.R                   ← Script maître (pipeline complet)
│
├── charger_erfs.R              ← Chargement des bases ERFS 2005-2023
├── decomposition_pauvrete.R    ← Calculs : décomposition de la pauvreté
├── reste_a_vivre.R             ← Calculs : reste à vivre (BDF + ERFS)
├── graphs_decomposition.R      ← Graphiques décomposition (11 figures)
├── graphs_reste_a_vivre.R      ← Graphiques reste à vivre (8 figures)
│
├── analyse_pauvrete.qmd        ← Rapport Quarto (HTML + PDF)
├── analyse_pauvrete.html       ← Rapport HTML (interactif, généré)
├── analyse_pauvrete.pdf        ← Rapport PDF (statique, généré)
│
├── data/
│   └── parametres_macro.csv   ← IPC, RSA socle, SMIC 2005-2023
│
└── graphiques/                 ← Sorties graphiques (PNG + RDS, générées)
    ├── g1_taux_pauvrete.*
    ├── g2_decomposition.*
    └── ...
```

---

## Méthodologie

### Décomposition de la pauvreté (`decomposition_pauvrete.R`)
- Décomposition télescopique exacte en 4 effets : seuil, structure, redistribution, revenus primaires
- Décomposition microéconométrique Oaxaca-Blinder / Fairlie (2005) pour 2017 vs 2023
- Seuil de pauvreté : 60 % de la médiane du niveau de vie

### Reste à vivre (`reste_a_vivre.R`)
- Imputation de la structure de consommation BDF 2017 dans les ERFS 2018-2023
- Consommation contrainte : alimentation (C01), tabac/alcool (C02), logement/énergie (C04)
- Indexation temporelle par division COICOP (IPC base 2018)

**Hypothèse principale** : la structure de consommation est figée à 2017. Les résultats pour 2022-2023 doivent être interprétés avec prudence (comportements d'adaptation non captés).

---

## Paramètres macro (`data/parametres_macro.csv`)

Fichier CSV éditable pour la mise à jour annuelle. Colonnes :
- `annee` : année (2005-2023)
- `ipc` : indice des prix à la consommation (base 100 = 2015)
- `rsa_socle` : RSA socle personne seule (EUR/mois)
- `smic_net` : SMIC net mensuel 35h (EUR/mois)
