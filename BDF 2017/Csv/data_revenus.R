library(data.table)
library(insee)
library(writexl)
library(tidyverse)
library(readxl)
library(MetricsWeighted)

getwd()
setwd("E:/.shortcut-targets-by-id/1sON2nKjAJAn3F-0831NEQIxiQHmsi6S6/BDF2017")
getwd()

start_date<-"2018-01-01"

source("data.R")

STATUT <- read_delim("E:/.shortcut-targets-by-id/1sON2nKjAJAn3F-0831NEQIxiQHmsi6S6/BDF2017/BDF 2017/Csv/DEPMEN.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(IDENT_MEN, Stalog)

MENAGE <- read_delim("E:/.shortcut-targets-by-id/1sON2nKjAJAn3F-0831NEQIxiQHmsi6S6/BDF2017/BDF 2017/Csv/MENAGE.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(IDENT_MEN, REVDISP, NIVIE, REVTOT, REVACT, PREST_FAM_TOT,  CHOMAGE, RETRAITES, PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, PREST_PRECARITE_RSA, PPA,
         PREST_LOGEMENT, REVPAT, REVEXC, IMPOTREV_M, TAXHAB_M, QNIVIE1, QNIVIE2) %>%
  pivot_longer(!IDENT_MEN, names_to = "Type_revenu", values_to = "Valeur") %>%
  left_join(STATUT, by="IDENT_MEN")

MENAGE2 <- read_delim("E:/.shortcut-targets-by-id/1sON2nKjAJAn3F-0831NEQIxiQHmsi6S6/BDF2017/BDF 2017/Csv/MENAGE.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(IDENT_MEN, PREST_FAM_TOT, PREST_LOGEMENT,  CHOMAGE, RETRAITES, PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, PREST_PRECARITE_RSA, PPA) %>%
  pivot_longer(!IDENT_MEN, names_to = "Type_revenu", values_to = "Valeur") %>%
  left_join(STATUT, by="IDENT_MEN")

INDIVIDU <- read_delim("E:/.shortcut-targets-by-id/1sON2nKjAJAn3F-0831NEQIxiQHmsi6S6/BDF2017/BDF 2017/Csv/INDIVIDU.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

param_revalo <- readxl::read_excel("E:/Mon Drive/OFCE/OLD/Revalo_prest_2022.xlsx",sheet = "Feuil1") %>%
  pivot_longer(!date, names_to = "Type_revenu", values_to = "revalo") %>%
  pivot_wider(names_from = "date", values_from = "revalo")

# param_revalo_HypRevalo <- readxl::read_excel("E:/Mon Drive/OFCE/OLD/Revalo_prest_2022_Hyp.xlsx",sheet = "Feuil1") %>%
#   pivot_longer(!date, names_to = "Type_revenu", values_to = "revalo") %>%
#   pivot_wider(names_from = "date", values_from = "revalo")

RETRAITES <- MENAGE2 %>% left_join(param_revalo, by="Type_revenu") %>%
  filter(Type_revenu=="RETRAITES") %>%
  mutate(valeur_0118=(1+`2018-01-01`)*Valeur/12,
         valeur_0218=(1+`2018-02-01`)*valeur_0118,
         valeur_0318=(1+`2018-03-01`)*valeur_0218,
         valeur_0418=(1+`2018-04-01`)*valeur_0318,
         valeur_0518=(1+`2018-05-01`)*valeur_0418,
         valeur_0618=(1+`2018-06-01`)*valeur_0518,
         valeur_0718=(1+`2018-07-01`)*valeur_0618,
         valeur_0818=(1+`2018-08-01`)*valeur_0718,
         valeur_0918=(1+`2018-09-01`)*valeur_0818,
         valeur_1018=(1+`2018-10-01`)*valeur_0918,
         valeur_1118=(1+`2018-11-01`)*valeur_1018,
         valeur_1218=(1+`2018-12-01`)*valeur_1118,
         
         valeur_0119=(1+`2019-01-01`)*valeur_1218,
         valeur_0219=(1+`2019-02-01`)*valeur_0119,
         valeur_0319=(1+`2019-03-01`)*valeur_0219,
         valeur_0419=(1+`2019-04-01`)*valeur_0319,
         valeur_0519=(1+`2019-05-01`)*valeur_0419,
         valeur_0619=(1+`2019-06-01`)*valeur_0519,
         valeur_0719=(1+`2019-07-01`)*valeur_0619,
         valeur_0819=(1+`2019-08-01`)*valeur_0719,
         valeur_0919=(1+`2019-09-01`)*valeur_0819,
         valeur_1019=(1+`2019-10-01`)*valeur_0919,
         valeur_1119=(1+`2019-11-01`)*valeur_1019,
         valeur_1219=(1+`2019-12-01`)*valeur_1119,
         
         valeur_0120=(1+`2020-01-01`)*valeur_1219,
         valeur_0220=(1+`2020-02-01`)*valeur_0120,
         valeur_0320=(1+`2020-03-01`)*valeur_0220,
         valeur_0420=(1+`2020-04-01`)*valeur_0320,
         valeur_0520=(1+`2020-05-01`)*valeur_0420,
         valeur_0620=(1+`2020-06-01`)*valeur_0520,
         valeur_0720=(1+`2020-07-01`)*valeur_0620,
         valeur_0820=(1+`2020-08-01`)*valeur_0720,
         valeur_0920=(1+`2020-09-01`)*valeur_0820,
         valeur_1020=(1+`2020-10-01`)*valeur_0920,
         valeur_1120=(1+`2020-11-01`)*valeur_1020,
         valeur_1220=(1+`2020-12-01`)*valeur_1120,
         
         valeur_0121=(1+`2021-01-01`)*valeur_1220,
         valeur_0221=(1+`2021-02-01`)*valeur_0121,
         valeur_0321=(1+`2021-03-01`)*valeur_0221,
         valeur_0421=(1+`2021-04-01`)*valeur_0321,
         valeur_0521=(1+`2021-05-01`)*valeur_0421,
         valeur_0621=(1+`2021-06-01`)*valeur_0521,
         valeur_0721=(1+`2021-07-01`)*valeur_0621,
         valeur_0821=(1+`2021-08-01`)*valeur_0721,
         valeur_0921=(1+`2021-09-01`)*valeur_0821,
         valeur_1021=(1+`2021-10-01`)*valeur_0921,
         valeur_1121=(1+`2021-11-01`)*valeur_1021,
         valeur_1221=(1+`2021-12-01`)*valeur_1121,
         
         valeur_0122=(1+`2022-01-01`)*valeur_1221,
         valeur_0222=(1+`2022-02-01`)*valeur_0122,
         valeur_0322=(1+`2022-03-01`)*valeur_0222,
         valeur_0422=(1+`2022-04-01`)*valeur_0322,
         valeur_0522=(1+`2022-05-01`)*valeur_0422,
         valeur_0622=(1+`2022-06-01`)*valeur_0522,
         valeur_0722=(1+`2022-07-01`)*valeur_0622,
         valeur_0822=(1+`2022-08-01`)*valeur_0722,
         valeur_0922=(1+`2022-09-01`)*valeur_0822,
         valeur_1022=(1+`2022-10-01`)*valeur_0922,
         valeur_1122=(1+`2022-11-01`)*valeur_1022,
         valeur_1222=(1+`2022-12-01`)*valeur_1122,
         
         valeur_0123=(1+`2023-01-01`)*valeur_1222,
         valeur_0223=(1+`2023-02-01`)*valeur_0123,
         valeur_0323=(1+`2023-03-01`)*valeur_0223,
         valeur_0423=(1+`2023-04-01`)*valeur_0323,
         valeur_0523=(1+`2023-05-01`)*valeur_0423,
         valeur_0623=(1+`2023-06-01`)*valeur_0523,
         valeur_0723=(1+`2023-07-01`)*valeur_0623,
         valeur_0823=(1+`2023-08-01`)*valeur_0723,
         valeur_0923=(1+`2023-09-01`)*valeur_0823,
         valeur_1023=(1+`2023-10-01`)*valeur_0923,
         valeur_1123=(1+`2023-11-01`)*valeur_1023,
         valeur_1223=(1+`2023-12-01`)*valeur_1123) %>%
    group_by(IDENT_MEN) %>%
    summarise(Retraites_1221=sum(valeur_1220, na.rm = TRUE),
              Retraites_1222=sum(valeur_1222, na.rm = TRUE),
              Retraites_1223=sum(valeur_1223, na.rm = TRUE),
              VAR_Retraites_22=Retraites_1222/Retraites_1221-1,
              VAR_Retraites_23=Retraites_1223/Retraites_1222-1) %>%
  mutate(IDENT_MEN=as.numeric(IDENT_MEN))


PSoc <- MENAGE2 %>% left_join(param_revalo, by="Type_revenu") %>%
  filter(Type_revenu!="RETRAITES") %>%
  mutate(valeur_0118=(1+`2018-01-01`)*Valeur/12,
         valeur_0218=(1+`2018-02-01`)*valeur_0118,
         valeur_0318=(1+`2018-03-01`)*valeur_0218,
         valeur_0418=(1+`2018-04-01`)*valeur_0318,
         valeur_0518=(1+`2018-05-01`)*valeur_0418,
         valeur_0618=(1+`2018-06-01`)*valeur_0518,
         valeur_0718=(1+`2018-07-01`)*valeur_0618,
         valeur_0818=(1+`2018-08-01`)*valeur_0718,
         valeur_0918=(1+`2018-09-01`)*valeur_0818,
         valeur_1018=(1+`2018-10-01`)*valeur_0918,
         valeur_1118=(1+`2018-11-01`)*valeur_1018,
         valeur_1218=(1+`2018-12-01`)*valeur_1118,
         
         valeur_0119=(1+`2019-01-01`)*valeur_1218,
         valeur_0219=(1+`2019-02-01`)*valeur_0119,
         valeur_0319=(1+`2019-03-01`)*valeur_0219,
         valeur_0419=(1+`2019-04-01`)*valeur_0319,
         valeur_0519=(1+`2019-05-01`)*valeur_0419,
         valeur_0619=(1+`2019-06-01`)*valeur_0519,
         valeur_0719=(1+`2019-07-01`)*valeur_0619,
         valeur_0819=(1+`2019-08-01`)*valeur_0719,
         valeur_0919=(1+`2019-09-01`)*valeur_0819,
         valeur_1019=(1+`2019-10-01`)*valeur_0919,
         valeur_1119=(1+`2019-11-01`)*valeur_1019,
         valeur_1219=(1+`2019-12-01`)*valeur_1119,
         
         valeur_0120=(1+`2020-01-01`)*valeur_1219,
         valeur_0220=(1+`2020-02-01`)*valeur_0120,
         valeur_0320=(1+`2020-03-01`)*valeur_0220,
         valeur_0420=(1+`2020-04-01`)*valeur_0320,
         valeur_0520=(1+`2020-05-01`)*valeur_0420,
         valeur_0620=(1+`2020-06-01`)*valeur_0520,
         valeur_0720=(1+`2020-07-01`)*valeur_0620,
         valeur_0820=(1+`2020-08-01`)*valeur_0720,
         valeur_0920=(1+`2020-09-01`)*valeur_0820,
         valeur_1020=(1+`2020-10-01`)*valeur_0920,
         valeur_1120=(1+`2020-11-01`)*valeur_1020,
         valeur_1220=(1+`2020-12-01`)*valeur_1120,
         
         valeur_0121=(1+`2021-01-01`)*valeur_1220,
         valeur_0221=(1+`2021-02-01`)*valeur_0121,
         valeur_0321=(1+`2021-03-01`)*valeur_0221,
         valeur_0421=(1+`2021-04-01`)*valeur_0321,
         valeur_0521=(1+`2021-05-01`)*valeur_0421,
         valeur_0621=(1+`2021-06-01`)*valeur_0521,
         valeur_0721=(1+`2021-07-01`)*valeur_0621,
         valeur_0821=(1+`2021-08-01`)*valeur_0721,
         valeur_0921=(1+`2021-09-01`)*valeur_0821,
         valeur_1021=(1+`2021-10-01`)*valeur_0921,
         valeur_1121=(1+`2021-11-01`)*valeur_1021,
         valeur_1221=(1+`2021-12-01`)*valeur_1121,
         
         valeur_0122=(1+`2022-01-01`)*valeur_1221,
         valeur_0222=(1+`2022-02-01`)*valeur_0122,
         valeur_0322=(1+`2022-03-01`)*valeur_0222,
         valeur_0422=(1+`2022-04-01`)*valeur_0322,
         valeur_0522=(1+`2022-05-01`)*valeur_0422,
         valeur_0622=(1+`2022-06-01`)*valeur_0522,
         valeur_0722=(1+`2022-07-01`)*valeur_0622,
         valeur_0822=(1+`2022-08-01`)*valeur_0722,
         valeur_0922=(1+`2022-09-01`)*valeur_0822,
         valeur_1022=(1+`2022-10-01`)*valeur_0922,
         valeur_1122=(1+`2022-11-01`)*valeur_1022,
         valeur_1222=(1+`2022-12-01`)*valeur_1122,
         
         
         valeur_0123=(1+`2023-01-01`)*valeur_1222,
         valeur_0223=(1+`2023-02-01`)*valeur_0123,
         valeur_0323=(1+`2023-03-01`)*valeur_0223,
         valeur_0423=(1+`2023-04-01`)*valeur_0323,
         valeur_0523=(1+`2023-05-01`)*valeur_0423,
         valeur_0623=(1+`2023-06-01`)*valeur_0523,
         valeur_0723=(1+`2023-07-01`)*valeur_0623,
         valeur_0823=(1+`2023-08-01`)*valeur_0723,
         valeur_0923=(1+`2023-09-01`)*valeur_0823,
         valeur_1023=(1+`2023-10-01`)*valeur_0923,
         valeur_1123=(1+`2023-11-01`)*valeur_1023,
         valeur_1223=(1+`2023-12-01`)*valeur_1123) %>%
  group_by(IDENT_MEN) %>%
  summarise(PSoc_1221=sum(valeur_1220, na.rm = TRUE),
            PSoc_1222=sum(valeur_1222, na.rm = TRUE),
            PSoc_1223=sum(valeur_1223, na.rm = TRUE),
            VAR_PSoc_22=PSoc_1222/PSoc_1221-1,
            VAR_PSoc_23=PSoc_1223/PSoc_1222-1) %>%
  mutate(IDENT_MEN=as.numeric(IDENT_MEN))

prev_ipc <- readxl::read_excel("C:/Users/pierr/Downloads/IPC_PiM_fev23.xlsx",range = "A3:D412") %>%
 mutate(DATE = CURRENCY, FRCONPRCF=`E...2`, FRCPXENGF=`E...3`, FRCPENEGF=`E...4`) %>%
 select(-CURRENCY, -`E...2`, -`E...3`, -`E...4`, -FRCPXENGF , -FRCPENEGF) %>%
 filter(year(DATE)>2021 & month(DATE)>0) %>%
 pivot_wider(names_from = DATE, values_from = FRCONPRCF) %>%
 mutate(var_0123=`2023-01-01`/`2022-12-01`,
        var_0223=`2023-02-01`/`2023-01-01`,
        var_0323=`2023-03-01`/`2023-02-01`,
        var_0423=`2023-04-01`/`2023-03-01`,
        var_0523=`2023-05-01`/`2023-04-01`,
        var_0623=`2023-06-01`/`2023-05-01`,
        var_0723=`2023-07-01`/`2023-06-01`,
        var_0823=`2023-08-01`/`2023-07-01`,
        var_0923=`2023-09-01`/`2023-08-01`,
        var_1023=`2023-10-01`/`2023-09-01`,
        var_1123=`2023-11-01`/`2023-10-01`,
        var_1223=`2023-12-01`/`2023-11-01`) %>%
 select(starts_with("var"))

NDV <- MENAGE %>% 
  filter(Type_revenu=="NIVIE") %>% 
  mutate(IDENT_MEN=as.numeric(IDENT_MEN)) %>%
  mutate(NDV=Valeur) %>%
  select(-Type_revenu, -Valeur) 

decile <- read_delim("MENAGE.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(IDENT_MEN, REVDISP, NIVIE, REVTOT, REVACT, PREST_FAM_TOT,  CHOMAGE, RETRAITES, PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, PREST_PRECARITE_RSA, PPA,
         PREST_LOGEMENT, REVPAT, REVEXC, IMPOTREV_M, TAXHAB_M, QNIVIE1, QNIVIE2, pondmen) %>%
  mutate(dec=cut(NIVIE, MetricsWeighted::weighted_quantile(NIVIE, w=pondmen, 0:10/10), include.lowest=TRUE, labels=str_c("d", 1:10))) %>%
  mutate(IDENT_MEN=as.numeric(IDENT_MEN))
 
final_data_prev <- final_data |> pivot_wider(names_from = DATE, values_from = ipc) |> select(-pondmen) |>
  mutate(`2023-01-01`=`2022-12-01`*prev_ipc$var_0123,
         `2023-02-01`=`2023-01-01`*prev_ipc$var_0223,
         `2023-03-01`=`2023-02-01`*prev_ipc$var_0323,
         `2023-04-01`=`2023-03-01`*prev_ipc$var_0423,
         `2023-05-01`=`2023-04-01`*prev_ipc$var_0523,
         `2023-06-01`=`2023-05-01`*prev_ipc$var_0623,
         `2023-07-01`=`2023-06-01`*prev_ipc$var_0723,
         `2023-08-01`=`2023-07-01`*prev_ipc$var_0823,
         `2023-09-01`=`2023-08-01`*prev_ipc$var_0923,
         `2023-10-01`=`2023-09-01`*prev_ipc$var_1023,
         `2023-11-01`=`2023-10-01`*prev_ipc$var_1123,
         `2023-12-01`=`2023-11-01`*prev_ipc$var_1223) |>
  pivot_longer(!IDENT_MEN  , names_to = "DATE", values_to = "ipc")

PA <- final_data %>%
  left_join(decile, by="IDENT_MEN") %>%
  filter(REVDISP>0) %>%
  pivot_wider(names_from = DATE, values_from = ipc) %>%
  mutate(VAR_IPC_22=`2022-12-01`/`2021-12-01`-1) %>%
  #select(IDENT_MEN, VAR_IPC, NDV, pondmen,Stalog) %>%
  left_join(PSoc %>% select(IDENT_MEN, VAR_PSoc_22), by="IDENT_MEN") %>% filter(VAR_PSoc_22 != "NA") %>%
  left_join(RETRAITES %>% select(IDENT_MEN, VAR_Retraites_22), by="IDENT_MEN") %>% filter(VAR_Retraites_22 != "NA") %>%
  mutate(POIDS_PSoc=(PREST_FAM_TOT+CHOMAGE+PREST_PRECARITE_VIEIL+PREST_PRECARITE_HAND+PREST_PRECARITE_RSA+PPA)/(REVTOT)) %>%
  mutate(POIDS_Retraites=(RETRAITES)/(REVTOT)) %>%
  mutate(VAR_PA_PSoc_22=(VAR_PSoc_22-VAR_IPC_22)*POIDS_PSoc) %>%
  mutate(VAR_PA_Retraites_22=(VAR_Retraites_22-VAR_IPC_22)*POIDS_Retraites) %>%
  select(IDENT_MEN, dec, VAR_PA_Retraites_22, VAR_PA_PSoc_22, VAR_IPC_22, pondmen.x) 


PA_prev <- final_data_prev %>%
  left_join(decile, by="IDENT_MEN") %>%
  filter(REVDISP>0) %>%
  pivot_wider(names_from = DATE, values_from = ipc) %>%
  mutate(VAR_IPC_22=`2022-12-01`/`2021-12-01`-1, VAR_IPC_23=`2023-12-01`/`2022-12-01`-1) %>%
  #select(IDENT_MEN, VAR_IPC, NDV, pondmen,Stalog) %>%
  left_join(PSoc %>% select(IDENT_MEN, VAR_PSoc_22, VAR_PSoc_23), by="IDENT_MEN") %>% filter(VAR_PSoc_23 != "NA") %>%
  left_join(RETRAITES %>% select(IDENT_MEN, VAR_Retraites_22, VAR_Retraites_23), by="IDENT_MEN") %>% filter(VAR_Retraites_22 != "NA") %>%
  mutate(POIDS_PSoc=(PREST_FAM_TOT+CHOMAGE+PREST_PRECARITE_VIEIL+PREST_PRECARITE_HAND+PREST_PRECARITE_RSA+PPA)/(REVTOT)) %>%
  mutate(POIDS_Retraites=(RETRAITES)/(REVTOT)) %>%
  mutate(VAR_PA_PSoc_22=(VAR_PSoc_22-VAR_IPC_22)*POIDS_PSoc, VAR_PA_PSoc_23=(VAR_PSoc_23-VAR_IPC_23)*POIDS_PSoc) %>%
  mutate(VAR_PA_Retraites_22=(VAR_Retraites_22-VAR_IPC_22)*POIDS_Retraites, VAR_PA_Retraites_23=(VAR_Retraites_23-VAR_IPC_23)*POIDS_Retraites) %>%
  select(IDENT_MEN, dec, VAR_PA_Retraites_22, VAR_PA_PSoc_22, VAR_IPC_22, VAR_PA_Retraites_23, VAR_PA_PSoc_23, VAR_IPC_23, pondmen) 

PA$VAR_PA_Retraites_22 <- PA$VAR_PA_Retraites_22 %>%  replace_na(0) 
PA$VAR_PA_PSoc_22 <- PA$VAR_PA_PSoc_22 %>%  replace_na(0) 

PA_prev$VAR_PA_Retraites_22 <- PA_prev$VAR_PA_Retraites_23 %>%  replace_na(0) 
PA_prev$VAR_PA_PSoc_22 <- PA_prev$VAR_PA_PSoc_23 %>%  replace_na(0) 
PA_prev$VAR_PA_Retraites_23 <- PA_prev$VAR_PA_Retraites_23 %>%  replace_na(0) 
PA_prev$VAR_PA_PSoc_23 <- PA_prev$VAR_PA_PSoc_23 %>%  replace_na(0) 

PA <- PA %>%   
  group_by(dec) %>%
  summarise(var_Retraites_22=weighted.mean(VAR_PA_Retraites_22, pondmen.x, na.rm=TRUE),
            var_PSoc_22=weighted.mean(VAR_PA_PSoc_22, pondmen.x, na.rm=TRUE))
  
PA_prev <- PA_prev %>%   
  group_by(dec) %>%
  summarise(var_Retraites_23=weighted.mean(VAR_PA_Retraites_23, pondmen, na.rm=TRUE),
            var_PSoc_23=weighted.mean(VAR_PA_PSoc_23, pondmen, na.rm=TRUE))

writexl::write_xlsx(PA,path="E:/Mon Drive/OFCE/PB Budget 2023/Contrib_Prest_PA_22.xlsx")
# fwrite(PA_prev,file="Contrib_Prest_PA_23.csv")
writexl::write_xlsx(PA_prev,path="E:/Mon Drive/OFCE/PB Budget 2023/Contrib_Prest_PA_23.xlsx")
