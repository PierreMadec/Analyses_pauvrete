library(haven)
library(tidyverse)
library(questionr)
library(openxlsx) 
library(MetricsWeighted)
library(readxl) 
library(dplyr) 
library(purrr)



# chemin_srcv <- "U:/DONNEES-DE-REFERENCE/REF-INSEE-SRCV/SRCV 2018/"
# chemin_code <- "C:/Users/eleonore.richard/Documents/CNLE/Lettre de mission/code"
# chemin_output <- "C:/Users/eleonore.richard/Documents/CNLE/Lettre de mission/fig" 
# basemen<-"menages18_diff"
# baseind<-"individus18_diff"


chemin_erfs <- "D:/Utilisateurs/mpucci/OneDrive/Documents/DONNEES/ERFS/ERFS 2023/SAS/"
chemin_erfs <- "C:/Users/mpucci/OneDrive/Documents/DONNEES/ERFS/ERFS 2023/SAS/"
basemen<-"fpr_menage_2023"
baseind<-"fpr_indiv_2023"

# 1. Lecture des tables Sas ----

menage   <- read_sas(paste0(chemin_erfs,basemen,".sas7bdat"), NULL) %>% 
  rename_with(.cols = everything(), .fn =\(x) toupper(x)) %>% 
  rename(IDENT=IDENT23) %>%
  mutate(STATOC_LOG = case_when(LOGT =="1" ~"PROPRIETAIRE",  # proprietaire non accédant
                                LOGT =="2" ~"ACCEDANT",  
                                LOGT =="3" ~"LOC_SOCIAL",  
                                LOGT =="4" ~"LOC_PRIVE",  
                                TRUE ~ "AUTRES" # autres cas : meublés, logés gratuits => hors champ de l'étude
                                ),
                            
         ZONE = case_when(TUU2020 %in% c("7","8") ~ "3", # MGP et grandes villes
                          TUU2020 == "0" ~ "1",          # zone rurale
                          TRUE ~ "2"),                   # villes petites et moyennes
         
         CAT_PR=case_when(CS8CORPR == "7" | as.numeric(AGEPRL)>= 64 ~ "RET",
                          CS8CORPR %in% c("1","2","3","4","5","6","8") ~ "ACT",
                          TRUE ~ "NR"),
         
         CAT_CJ=case_when(CS8CORCJ == "7" | as.numeric(AGEPRLCJ)>64 ~ "RET",
                          CS8CORCJ %in% c("1","2","3","4","5","6","8") ~ "ACT",
                          TRUE ~ "NR"),
         ) %>% select(IDENT,WPRM,
                      TYPLOG5,# préférable à TYMEN5 pour imputer le coût du logement
                      NIVVIEM,REVDISPM,
                      STATOC_LOG,ZONE,CAT_PR,CAT_CJ,NBPERSLOG,LOGT) 

# Stats sur les config familiales des logements et sur les STATOC_LOG à exclure

tab<-wtd.table(menage$TYPLOG5,useNA="no", weights = menage$WPRM)
round(100*tab / sum(tab),2) # 37.28  9.10 26.55 24.60  2.46

tab<-wtd.table(menage$LOGT,useNA="no", weights = menage$WPRM)
round(100*tab / sum(tab),2) # 25.79 - 32.03 - 12.43 - 21.58 et (meublé-hôtel : 2.25, logé_gratuit : 5.91, NR :  0.01) 

tab<-wtd.table(menage$STATOC_LOG,useNA="no", weights = menage$WPRM)
round(100*tab / sum(tab),2) # 
# ACCEDANT       AUTRES    LOC_PRIVE   LOC_SOCIAL PROPRIETAIRE 
# 32.03         8.16        21.58        12.43        25.79 

# on enlève les ménages logés gratuitement ou en meublé/hôtel
menage<-menage %>% filter(STATOC_LOG != "AUTRES")



indiv   <- read_sas(paste0(chemin_erfs,baseind,".sas7bdat"), NULL) %>% 
  rename_with(.cols = everything(), .fn =\(x) toupper(x)) %>% 
  rename(IDENT = IDENT23) %>% 
  mutate(COUPL_LOG=if_else(is.na(COUPL_LOG),2,COUPL_LOG))

# TEST
# PRCJ0<-indiv %>% filter(LPRL %in% c("1","2") & AGE>=15 & AGE<25 & NBENFIND_LOG ==0) 
# table(PRCJ0$AGE,PRCJ0$TYPLOG5)
# 
# PRCJ<-PRCJ0 %>% select(IDENT) %>% group_by(IDENT) %>% slice(1)
# 
# test<-indiv %>% select(IDENT,LPRL,AGE,TYPLOG5)
# 
# PRCJ1<-PRCJ %>% inner_join(test,by="IDENT") %>% filter(TYPLOG5 =="5") 

# 2 cas bizarres, enfant 2 ou 13 ans TYPLOG=1 
# des couples avec un mineur ou ménages complexes de mineurs (sans doute coloc)
# des cas TYPLOG5=2 ou TYPLOG="4" où 1 enfant est PR au lieu du parent

#  => pour compter les enfanst on ,ne peut pas utiliser lprl : on s'assure qu'ils n'ont pas eux mêm d'nefnt et ne sont pas en couple
table(indiv$NBENFIND_LOG,useNA="ifany")

# 2. Nombre d'enfants dans indiv : que ce soient les enfants de la PR ou non ----
   # certains ne seront pas comptés comme "enfants" s'ils ne vivent pas dans une famille mono ou dans un couple avec enfants


nbenf <- indiv %>% select(IDENT, NBENFIND_LOG, COUPL_LOG, AGE) %>% 
  mutate(
         enf0_2=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=0 & AGE<=2,1,0),
         enf3_10=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=3 & AGE<=10,1,0),
         enf11_14=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=11 & AGE<=14,1,0),
         enf15_17=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=15 & AGE<=17,1,0),
         enf18_24=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=18 & AGE<=24,1,0),
         enf0_17=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=0 & AGE<=17,1,0),
         enf0_24=if_else(NBENFIND_LOG ==0 & COUPL_LOG != 1 & AGE>=0 & AGE<=25,1,0),
         indiv=1 
         ) %>% 
  group_by(IDENT) %>% 
  summarise(nbenf0_2=sum(enf0_2),
            nbenf3_10=sum(enf3_10),
            nbenf11_14=sum(enf11_14),
            nbenf15_17=sum(enf15_17),
            nbenf18_24=sum(enf18_24),
            nbenf0_17=sum(enf0_17),
            nbenf0_24=sum(enf0_24),
            nbindiv=sum(indiv) # pour vérifier avec NBPERSLOG
            ) 


age_enf <- indiv %>% filter(NBENFIND_LOG ==0 & COUPL_LOG!= 1 & AGE<18) %>% 
  select(IDENT,AGE) %>% 
  mutate(AGE_TR=case_when(AGE<3 ~ "0_2",
                          AGE<11 ~ "3_10",
                          AGE<15 ~ "11_14",
                          TRUE ~ "15_17")
         ) %>% 
  group_by(IDENT) %>%
  arrange(desc(AGE), .by_group = TRUE) %>%
  mutate(
    rang = row_number(),
  ) %>%
  select(-AGE) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = rang,
    values_from = AGE_TR,
    names_prefix = "AGE"
  ) %>% select(IDENT, starts_with("AGE")) 


# 3. Appariement menage/nb d'enfants et création catégories ménages ----

men <- menage %>% 
  inner_join(nbenf, by = "IDENT") %>% 
  left_join(age_enf, by = "IDENT") %>% 
  mutate(
    nbenf0_2=if_else(TYPLOG5 %in% c("1","3"),0,nbenf0_2),
    nbenf3_10=if_else(TYPLOG5 %in% c("1","3"),0,nbenf3_10),
    nbenf11_14=if_else(TYPLOG5 %in% c("1","3"),0,nbenf11_14),
    nbenf15_17=if_else(TYPLOG5 %in% c("1","3"),0,nbenf15_17),
    nbenf18_24=if_else(TYPLOG5 %in% c("1","3"),0,nbenf18_24),
    nb_adulte=case_when(TYPLOG5=="1" ~ 0,# adultes en plus du ou des parents ou de la première personne d'un ménage complexe
                        TYPLOG5=="2" ~ pmax(0,nbindiv-nbenf0_24-1),
                        TYPLOG5=="3" ~ 0,
                        TYPLOG5=="4" ~ pmax(0,nbindiv-nbenf0_24-2),
                        TRUE ~ pmax(0,nbindiv-nbenf0_24-1)),
    CAS_TYPE1 = case_when(
                           # on retient uniquement si tous les enfanst sont mineurs
                          # si enfants majeurs et ou mineurs on comptera comme pour un ménage complexe
                          # --- 1. PERSONNES SEULES ---
                          TYPLOG5 == "1" & CAT_PR == "ACT" ~ "ACT_I0",
                          TYPLOG5 == "1" & CAT_PR == "RET" ~ "RET_I0",
                          # --- 2. FAMILLES MONOPARENTALES ---
                          TYPLOG5 == "2" & nbenf0_24 == 1 & nbenf0_17 == 1 ~ "ISO1",
                          TYPLOG5 == "2" & nbenf0_24 == 2 & nbenf0_17 == 2 ~ "ISO2",
                          TYPLOG5 == "2" & nbenf0_24 == 3 & nbenf0_17 == 3 ~ "ISO3",
                          TYPLOG5 == "2" & nbenf0_24 >= 4 & nbenf0_17 >= 4 ~ "ISO4", 
                          # --- 3. COUPLES AVEC ENFANTS (TYPLOG5 == 4) ---
                          TYPLOG5 == "4" & nbenf0_24 == 1 & nbenf0_17 == 1 ~ "COU1",
                          TYPLOG5 == "4" & nbenf0_24 == 2 & nbenf0_17 == 2 ~ "COU2",
                          TYPLOG5 == "4" & nbenf0_24 == 3 & nbenf0_17 == 3 ~ "COU3",
                          TYPLOG5 == "4" & nbenf0_24 >= 4 & nbenf0_17 >= 4 ~ "COU4", 
                          # --- 4. COUPLES SANS ENFANTS (TYPLOG5 == 3) ---
                          TYPLOG5 == "3" & CAT_PR == "RET" & CAT_CJ == "RET" ~ "RET_COU",
                          TYPLOG5 == "3" ~ "ACT_COU",
                          # --- 5 MENAGES COMPLEXES AVEC ENFANTS
                          TYPLOG5 == "5" & nbenf0_24 == 1 & nbenf0_17 == 1 ~ "COMP1",
                          TYPLOG5 == "5" & nbenf0_24 == 2 & nbenf0_17 == 2 ~ "COMP2",
                          TYPLOG5 == "5" & nbenf0_24 == 3 & nbenf0_17 == 3 ~ "COMP3",
                          TYPLOG5 == "5" & nbenf0_24 >= 4 & nbenf0_17 >= 4 ~ "COMP4", 
                          # --- 5 MENAGES COMPLEXES SANS ENFANT
                          TRUE ~ "COMP0"),
         CAS_TYPE2 = case_when(# on compte ls enfants mineurs et on ajoutera les enfants majeurs en coût supplémentaire
                               # si enfants majeurs seulement on comptera comme pour un ménage complexe
                              # --- 1. PERSONNES SEULES ---
                               TYPLOG5 == "1" & CAT_PR == "ACT" ~ "ACT_IS0",
                               TYPLOG5 == "1" & CAT_PR == "RET" ~ "RET_I0",
                               # --- 2. FAMILLES MONOPARENTALES ---
                               TYPLOG5 == "2" & nbenf0_17 == 1 ~ "ISO1",
                               TYPLOG5 == "2" & nbenf0_17 == 2 ~ "ISO2",
                               TYPLOG5 == "2" & nbenf0_17 == 3 ~ "ISO3",
                               TYPLOG5 == "2" & nbenf0_17 >= 4 ~ "ISO4", 
                               # --- 3. COUPLES AVEC ENFANTS (TYPLOG5 == 4) ---
                               TYPLOG5 == "4" & nbenf0_17 == 1 ~ "COU1",
                               TYPLOG5 == "4" & nbenf0_17 == 2 ~ "COU2",
                               TYPLOG5 == "4" & nbenf0_17 == 3 ~ "COU3",
                               TYPLOG5 == "4" & nbenf0_17 >= 4 ~ "COU4", 
                               # --- 4. COUPLES SANS ENFANTS (TYPLOG5 == 3) ---
                               TYPLOG5 == "3" & CAT_PR == "RET" & CAT_CJ == "RET" ~ "RET_COU",
                               TYPLOG5 == "3" ~ "ACT_COU",
                               # --- 5 MENAGES COMPLEXES AVEC ENFANTS
                               TYPLOG5 == "5" & nbenf0_17 == 1 ~ "COMP1",
                               TYPLOG5 == "5" & nbenf0_17 == 2 ~ "COMP2",
                               TYPLOG5 == "5" & nbenf0_17 == 3 ~ "COMP3",
                               TYPLOG5 == "5" & nbenf0_17 >= 4 ~ "COMP4", 
                               # --- 5 MENAGES COMPLEXES SANS ENFANT
                               TRUE ~ "COMP0"),
    CAS_TYPE = case_when(
      # --- 1. PERSONNES SEULES AVEC OU SANS ENFANT---
      TYPLOG5 == 1 & CAT_PR == "RET" ~ "ISO_RET",
      TYPLOG5 %in% c("1","2") & nbindiv == 1 + nbenf0_24 ~ "ISO_ACT",
      # --- 2. COUPLES AVEC OU SANS ENFANT ---
      TYPLOG5 == "3" & CAT_PR == "RET" & CAT_CJ == "RET" ~ "COU_RET",
      TYPLOG5 %in% c("3","4") & nbindiv == 2 + nbenf0_24~ "COU_ACT",      
      # --- 3 MENAGES COMPLEXES AVEC ENFANTS
      TRUE ~ "COMPLEX"),
    AGE1=if_else(is.na(AGE1),"XX",AGE1),
    AGE2=if_else(is.na(AGE2),"XX",AGE2),
    AGE3=if_else(is.na(AGE3),"XX",AGE3),
    AGE4=if_else(is.na(AGE4),"XX",AGE4),
    AGE5=if_else(is.na(AGE5),"XX",AGE5),
    AGE6=if_else(is.na(AGE6),"XX",AGE6)
      
  ) 




# 4. Import paramètres BdR et appariement table ménage ----

# Fonction générique pour créer les varaibles de composantes des BDR à partir de la feuille de paramètres
compute_param <- function(data, sheet_name, var_name) {
  
  param <- read_excel("parametres_new.xlsx", sheet = sheet_name)
  
  data %>% 
    left_join(param, by = c("ZONE","CAS_TYPE")) %>% 
    rowwise() %>% 
    mutate(
      !!var_name := MONTANT0 +
        get(paste0("SUP_ENF1_", AGE1)) +
        get(paste0("SUP_ENF2_", AGE2)) +
        get(paste0("SUP_ENF3_", AGE3)) +
        pmax(0, nbenf0_17 - 3) * get(paste0("SUP_ENF4_", AGE4)) +
        nbenf18_24 * SUP_18_24 +
        nb_adulte * SUP_ADULTE
    ) %>% 
    ungroup() %>% 
    select(-c(MONTANT0, starts_with("SUP_")))
}

# Liste des composantes à calculer : le nom de la composante est égal au nom de la feuille excel
liste_params <- list(
  BDR_HL = "BDR_HL",
  COMM   = "COMM",
  BANQUE="BANQUE",
  ECOLE="ECOLE",
  ASSUR="ASSUR",
  TAXES="TAXES",
  SANTE="SANTE",
  GARDE="GARDE",
  TRANSPORT="TRANSPORT",
  ALIM="ALIM"
)

# Application en chaîne
men2 <- reduce(
  names(liste_params),
  .init = men,
  .f = function(df, sheet) compute_param(df, sheet, liste_params[[sheet]])
  )



compute_param2 <- function(data, sheet_name, var_name) {
  
  param <- read_excel("parametres_new.xlsx", sheet = sheet_name)
  
  data %>% 
    left_join(param, by = c("ZONE","CAS_TYPE","STATOC_LOG")) %>% 
    rowwise() %>% 
    mutate(
      !!var_name := MONTANT0 +
        get(paste0("SUP_ENF1_", AGE1)) +
        get(paste0("SUP_ENF2_", AGE2)) +
        get(paste0("SUP_ENF3_", AGE3)) +
        pmax(0, nbenf0_17 - 3) * get(paste0("SUP_ENF4_", AGE4)) +
        nbenf18_24 * SUP_18_24 +
        nb_adulte * SUP_ADULTE
    ) %>% 
    ungroup() %>% 
    select(-c(MONTANT0, starts_with("SUP_")))
}

# Liste des composantes à calculer : le nom de la composante est égal au nom de la feuille excel
liste_params2 <- list(
  LOG = "LOG",
  ENER_LOG   = "ENER_LOG",
  BDR="BDR"
)

# Application en chaîne
men3 <- reduce(
  names(liste_params2),
  .init = men2,
  .f = function(df, sheet) compute_param2(df, sheet, liste_params2[[sheet]])
)




donnees<-men3 %>% 
  mutate( 
          dep_drees = LOG+ENER_LOG+COMM+BANQUE+ECOLE+ASSUR, 
          dep_cnle = dep_drees + TAXES+SANTE+GARDE+TRANSPORT, 
          dep_cnle_alim = dep_cnle + ALIM,
          reste_vivre_log=(REVDISPM/12-LOG-ENER_LOG)/nbindiv,
          reste_vivre_drees=(REVDISPM/12-dep_drees)/nbindiv,
          reste_vivre_cnle=(REVDISPM/12-dep_cnle)/nbindiv,
          reste_vivre_alim=(REVDISPM/12-dep_cnle_alim)/nbindiv,
          reste_vivre_bdr=(REVDISPM/12-BDR)/nbindiv,
          REVDISP_ind=REVDISPM/12/nbindiv,
          NVIE=as.numeric(NIVVIEM)/12,
          PAUVRE=if_else(NVIE<=0.6*weighted_median(NVIE,w=WPRM),1,0),
          INSUF_LOG=if_else(reste_vivre_log<=0,1,0),
          INSUF_DREES=if_else(reste_vivre_drees<=0,1,0),
          INSUF_CNLE=if_else(reste_vivre_cnle<=0,1,0),
          INSUF_ALIM=if_else(reste_vivre_alim<=0,1,0),
          INSUF_BDR=if_else(reste_vivre_bdr<=0,1,0),

          CAT_BR=case_when(INSUF_LOG == 1 ~ "1. Restriction logement",
                           INSUF_DREES == 1 ~ "2. Restriction dépenses pré-engagées",
                           INSUF_CNLE == 1 ~ "3. Restriction dépenses contraintes",
                           INSUF_ALIM == 1 ~ "4. Restriction dépenses contraintes et alimentaires",
                           INSUF_BDR == 1 ~ "5. Restriction dépenses pour une vie décente",
                           REVDISPM/12<=1.25*BDR ~ "6. Accès à une vie décente",
                           TRUE ~ "7. Aisance budgétaire (au moins 1,25 BDR)"
                           ),
         CAT_BR2=case_when(INSUF_CNLE == 1 ~ "1. Restriction dépenses contraintes",
                          INSUF_ALIM == 1 ~ "2. Restriction dépenses contraintes et alimentaires",
                          INSUF_BDR == 1 ~ "3. Restriction dépenses pour une vie décente",
                          REVDISPM/12<=1.25*BDR ~ "4. Accès à une vie décente",
                          TRUE ~ "5. Aisance budgétaire (au moins 1,25 BDR)"
         ),
          CAT_NV=case_when(NVIE<=0.4*weighted_median(NVIE,w=WPRM) ~ "1. Pauvreté monétaire sévère (<=SPM40)",
                           NVIE<=0.6*weighted_median(NVIE,w=WPRM) ~ "2. Pauvreté monétaire moins intense (>SPM40)",
                           NVIE<=weighted_quantile(NVIE,w=WPRM,0.4) ~ "3. Modeste non pauvre (entre SPM60 et D4)",
                           TRUE ~ "4. Aisance monétaire")
          
          ) 

men4<-men3 %>% filter(NVIE>=0)



table(men4$BDR)
table(men4$CAT_NV)
table(men4$CAT_BR)
table(men4$CAT_BR2)



