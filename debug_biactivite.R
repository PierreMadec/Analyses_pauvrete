library(tidyverse)

source("charger_erfs.R")

# Vérifier sur une année simple : 2017
df_2017_indiv <- erfs$individus2017 |>
  head(1000)

# Qu'est-ce qu'on a comme colonnes d'activité ?
cat("Colonnes acteu* dans individus2017:\n")
print(names(df_2017_indiv)[grepl("acteu", names(df_2017_indiv), ignore.case = TRUE)])

# Vérifier les valeurs distinctes
cat("\nvaleurs distinctes de acteu6prm (si elle existe):\n")
if ("acteu6prm" %in% names(df_2017_indiv)) {
  print(table(df_2017_indiv$acteu6prm, useNA = "ifany"))
}

cat("\nvaleurs distinctes de acteuprmcj (si elle existe):\n")
if ("acteuprmcj" %in% names(df_2017_indiv)) {
  print(table(df_2017_indiv$acteuprmcj, useNA = "ifany"))
}

# Tester le code de detecter_et_recoder_activite
source("decomposition_pauvrete.R", local = TRUE) # charger les fonctions

df_test <- df_2017_indiv |>
  mutate(
    acteu_pr_raw = detecter_et_recoder_activite(
      df_2017_indiv,
      candidats_acteu3 = "acteuprlcj",
      candidats_acteu6 = c("acteu6prmcj", "acteu6cj")
    ),
    acteu_cj_raw = detecter_et_recoder_activite(
      df_2017_indiv,
      candidats_acteu3 = "acteuprlcj",
      candidats_acteu6 = c("acteu6prmcj", "acteu6cj")
    )
  )

cat("\nacteu_pr_raw (person of reference):\n")
print(table(df_test$acteu_pr_raw, useNA = "ifany"))

cat("\nacteu_cj_raw (spouse/conjoint):\n")
print(table(df_test$acteu_cj_raw, useNA = "ifany"))
