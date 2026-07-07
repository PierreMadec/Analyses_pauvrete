# Extensions analytiques — "Quels emplois créés ? Pour quel impact sur la pauvreté ?"

**Pierre Madec — note de travail, juin 2026**

---

L'article actuel répond à "qui sont les travailleurs pauvres et pourquoi leur nombre augmente ?". 
Il ne répond pas à "quels sont les emplois créés entre 2010 et 2023, et dans quelle mesure leur nature 
explique-t-elle la hausse de la pauvreté laborieuse ?". Les deux questions sont complémentaires : 
la première est centrée sur la structure des ménages, la seconde sur la structure des emplois.

---

## Extension 1 — Shift-share sur les conditions d'emploi (priorité haute)

### Question
L'augmentation de la pauvreté laborieuse provient-elle d'une dégradation des *conditions d'emploi* 
(plus de CDD, plus de temps partiel subi, secteurs plus précaires) ou d'un vieillissement/paupérisation 
*de la demande* (les emplois existants sont plus souvent occupés par des travailleurs pauvres) ?

### Méthode
Appliquer la même décomposition shift-share que celle faite sur les statuts d'activité, mais sur 
les *types d'emploi* définis par les conditions de travail.

Pour chaque "type d'emploi" k défini par {contrat × intensité horaire × catégorie socioprofessionnelle},
calculer chaque année :
- `N_k(t)` : nombre de travailleurs (PR en emploi) de ce type
- `TP_k(t)` : taux de pauvreté de ces travailleurs

La contribution du type k à la pauvreté laborieuse globale est `contrib_k = TP_k × N_k / N`.

Sa variation se décompose en :
- **Effet taux** : le taux de pauvreté de ce type d'emploi a changé
  `= ΔTP_k × N_k(t0) / N(t0)`
- **Effet structure** : la part de ce type d'emploi parmi les travailleurs a changé
  `= TP_k(t0) × Δ(N_k/N)`

### Ce qu'on espère voir
- Si "effet structure" domine pour les CDD/temps partiel → la précarisation de l'emploi est 
  structurellement responsable de la hausse de la pauvreté laborieuse
- Si "effet taux" domine → les conditions d'emploi sont stables mais les travailleurs 
  occupant ces emplois sont devenus plus pauvres (pour des raisons de ménage !)
- Le croisement avec le résultat Oaxaca est la pièce maîtresse : si l'effet taux 
  domine pour les conditions d'emploi, c'est cohérent avec l'effet de coefficients Oaxaca 
  (dégradation conditionnelle)

### Variables disponibles dans l'ERFS
- `contrat` (CDI / CDD / intérim / autre) — disponible depuis 2015 environ
- `tppred` ou variable d'intensité horaire (temps partiel / complet) — plus long historique
- `csp` ou `pcs` (catégorie socioprofessionnelle) — disponible
- Secteur d'activité (`naf`) — disponible mais à vérifier la continuité de nomenclature

### Fichier R suggéré
`R/shiftshare_conditions_emploi.R`
Produit : `shiftshare_conditions.rds` (graphique facetté, analogue à `g11c_shiftshare.rds` 
mais sur les conditions d'emploi)

---

## Extension 2 — Profil des "emplois nets créés" 2010-2023 (priorité haute)

### Question
Si on compare les travailleurs de 2023 à ceux de 2010, quelles sont les caractéristiques 
des emplois "en plus" ? S'agit-il d'emplois de qualité ou d'emplois précaires ?

### Méthode
L'ERFS est une coupe transversale (pas de panel), mais on peut calculer un "emploi net créé" 
comme la différence de distribution entre 2010-2012 et 2021-2023.

Pour chaque cellule (k) de la distribution des conditions d'emploi :
- `effectif_k(t1)` : poids de cette cellule parmi les travailleurs en 2010-2012
- `effectif_k(t2)` : poids en 2021-2023
- `TP_k(t2)` : taux de pauvreté de cette cellule en 2021-2023

Ordonner les cellules par leur taux de pauvreté (des plus faibles aux plus élevés), 
calculer `Δ_effectif_k = effectif_k(t2) - effectif_k(t1)`.

**Visualisation** : graphique "waterfall" ou scatter plot (taux de pauvreté × Δ effectif), 
permettant de voir si les emplois créés nets sont majoritairement dans les cellules à faible 
ou fort taux de pauvreté.

### Résultat attendu / question ouverte
- Si les emplois nets créés sont dans des cellules à fort taux de pauvreté → 
  la nature des emplois explique directement la hausse de la pauvreté laborieuse
- Si les emplois nets créés sont dans des cellules à faible taux de pauvreté 
  mais que ce taux a augmenté pour ces cellules → c'est un effet de coefficients 
  (les politiques n'ont pas suffi à les protéger)

---

## Extension 3 — Interaction typmen × conditions d'emploi (priorité moyenne)

### Question
Les familles monoparentales qui ont accédé à l'emploi sur la période le font-elles dans 
de meilleures ou moins bonnes conditions que les autres travailleurs ? Sont-elles davantage 
concentrées dans les emplois à temps partiel subi, CDD, secteurs précaires ?

### Méthode
Calculer, pour chaque configuration de ménage et chaque année :
- La part du temps partiel subi
- La part en CDD/intérim
- La distribution sectorielle

Comparer l'évolution entre configurations : si les familles monoparentales sont surreprésentées 
dans les emplois précaires ET si cette surreprésentation augmente, cela explique pourquoi leur 
inclusion dans l'emploi ne les sort pas de la pauvreté.

**Visualisation** : graphique en radar ou barres groupées (config × type d'emploi × date).

### Intérêt analytique
Ce croisement permet de lier les deux angles de l'article :
- Structure du ménage (déjà documentée) 
- Conditions d'emploi (insuffisamment documentées actuellement)

---

## Extension 4 — Évolution temporelle continue du premium emploi par configuration

### Question
Le premium emploi s'est-il érodé progressivement ou par sauts ? Pour quelles configurations 
et à quelle date ?

### Méthode
Le script `R/premium_emploi.R` produit déjà des AME annuels. La figure `premium_ame_typmen.rds` 
ne montre que des années sélectionnées. 

Ajouter une visualisation en courbe continue (line chart par configuration, 2005-2023) :
```r
# Dans premium_emploi.R : ajouter fig_premium_timeline
# Pour chaque config × année : AME de l'emploi + IC 95%
# Line chart avec ruban de confiance
```

Ce graphique permettrait d'identifier si l'érosion est continue (progressive) ou structurelle 
(suite à une réforme, à la crise de 2008, etc.). Il fournirait aussi la durée sur laquelle 
l'effet de coefficients Oaxaca s'est construit.

---

## Extension 5 — Simulation contrefactuelle : "Si la composition avait été celle de 2010..."

### Question
De combien le taux de pauvreté laborieuse serait-il différent aujourd'hui si la composition 
des travailleurs (par configuration de ménage) était restée celle de 2010 ?

### Méthode
La décomposition Oaxaca répond déjà à cette question : la composition contribue à +0,50 pt.

Mais on peut aller plus loin avec une **simulation directe** :
1. Pondérer les travailleurs de 2023 pour que leur distribution par configuration corresponde 
   à celle de 2010 (repondération par entropie ou ratio matching)
2. Calculer le taux de pauvreté de ces travailleurs repondérés
3. Comparer au taux observé en 2023

Ce simulateur permet aussi de répondre à : "Que se passerait-il si davantage de familles 
monoparentales accédaient à des conditions de bi-activité ?" en modifiant les poids 
de la distribution des configurations.

### Avantage
Très pédagogique pour un article de politique publique (chiffrer l'effet d'une politique).
Techniquement, c'est du reweighting standard (`WeightIt` ou calcul manuel).

---

## Architecture suggérée pour un article enrichi

```
Partie 1 — Constat
  §1.1 Paradoxe emploi/pauvreté (G1, G2, G3 — inchangés)

Partie 2 — La structure des ménages comme déterminant principal (actuel §3.1-3.3)
  §2.1 Configurations à risque (G4)
  §2.2 Oaxaca : composition vs comportement (G6-G9)
  §2.3 Pourquoi la composition s'est dégradée (G10, G11 taux emploi)

Partie 3 — La nature des emplois créés (nouvelle)
  §3.1 Profil des emplois nets créés (Extension 2)
  §3.2 Shift-share sur les conditions d'emploi (Extension 1)
  §3.3 Interaction configuration × conditions d'emploi (Extension 3)
  
Partie 4 — Synthèse et effets des politiques
  §4.1 Effet de coefficients : empreinte des politiques (existant G8 + G9 slope)
  §4.2 Premium emploi : érosion continue (Extension 4)
  §4.3 Simulation : que faire ? (Extension 5)
```

Cette architecture répond à la question du titre de façon complète :
- "L'emploi ne protège plus" : documenté (Partie 1)
- "Pourquoi ? Structure des ménages" : Partie 2
- "Pourquoi ? Nature des emplois" : Partie 3 (nouvelle)
- "Les politiques ont amorti mais pas résolu" : Partie 4
