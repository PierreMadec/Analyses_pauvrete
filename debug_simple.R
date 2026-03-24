library(tidyverse)
source("charger_erfs.R")

# SIMPLE : juste regarder ce qui existe en 2017
df <- erfs$individus2017

cat("=== COLONNES D'ACTIVITÉ EN 2017 ===\n\n")

cols_acteu <- names(df)[grepl("acteu", names(df), ignore.case = TRUE)]
cat("Colonnes trouvées:\n")
print(cols_acteu)

# Focus sur l'activité du conjoint
cat("\n=== ACTIVITÉ DU CONJOINT (acteuprmcj) ===\n")
if ("acteuprmcj" %in% names(df)) {
  cat("Valeurs dans acteuprmcj:\n")
  print(table(df$acteuprmcj, useNA = "ifany"))
  cat("\nClasse de acteuprmcj:\n")
  print(class(df$acteuprmcj))
} else {
  cat("acteuprmcj NOT FOUND in 2017\n")
}

# Focus sur l'activité de la PR
cat("\n=== ACTIVITÉ PERSON OF REFERENCE (acteuprm) ===\n")
if ("acteuprm" %in% names(df)) {
  cat("Valeurs dans acteuprm:\n")
  print(table(df$acteuprm, useNA = "ifany"))
  cat("\nClasse de acteuprm:\n")
  print(class(df$acteuprm))
} else {
  cat("acteuprm NOT FOUND in 2017\n")
}

# Chercher les 6-catégories
cat("\n=== ACTIVITÉ 6-CATÉGORIES ===\n")
if ("acteu6prm" %in% names(df)) {
  cat("acteu6prm:\n")
  print(table(df$acteu6prm, useNA = "ifany"))
}
if ("acteu6prmcj" %in% names(df)) {
  cat("\nacteu6prmcj:\n")
  print(table(df$acteu6prmcj, useNA = "ifany"))
}
