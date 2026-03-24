#Fichiers de sortie

sortie_nomen<-"nomen4digits.xlsx"
sortie_data<-"data4digits.xlsx"


#Paramètres séries à garder #

freq<-"M"
nat<-"INDICE"
corr<-"BRUT"
men<-"ENSEMBLE"
base<-"2015"
ref<- "FE"

source<-"IPC-2015"

idbank_list <- get_idbank_list(source)

nomen<-idbank_list%>%select(COICOP2016,COICOP2016_label_fr)%>%
  group_by(COICOP2016)%>%
  slice(1)%>%ungroup()%>%
  mutate(count_string=str_length(COICOP2016))%>%
  filter(count_string==4)%>%
  select(-count_string)

series<-left_join(idbank_list,nomen,by="COICOP2016")%>%
  filter(FREQ==freq,
         NATURE==nat,
         CORRECTION==corr,
         MENAGES_IPC==men,
         BASIND==base,
         REF_AREA==ref)%>%
  mutate(count_string=str_length(COICOP2016))%>%
  filter(count_string==4)%>%select(idbank)%>%pull(idbank)

data = 
  get_insee_idbank(series) %>%
  split_title() %>% 
  add_insee_metadata()


data_ipc04<-data%>%
  select(DATE,OBS_VALUE,COICOP2016)%>%
  pivot_wider(names_from = COICOP2016,values_from =OBS_VALUE,names_prefix = "I")%>%
  arrange(DATE)

write_xlsx(nomen,sortie_nomen,col_names = TRUE, format_headers = TRUE)
write_xlsx(data_ipc04,sortie_data,col_names = TRUE, format_headers = TRUE)


# 3 digits

nomen<-idbank_list%>%select(COICOP2016,COICOP2016_label_fr)%>%
  group_by(COICOP2016)%>%
  slice(1)%>%ungroup()%>%
  mutate(count_string=str_length(COICOP2016))%>%
  filter(count_string==3)%>%
  select(-count_string)%>%
  filter(COICOP2016 %in% c("071","111","031","125","092","094","123"))

series<-left_join(idbank_list,nomen,by="COICOP2016")%>%
  filter(FREQ==freq,
         NATURE==nat,
         CORRECTION==corr,
         MENAGES_IPC==men,
         BASIND==base,
         REF_AREA==ref)%>%
  filter(COICOP2016 %in% c("071","111","031","125","092","094","123"))%>%
  select(idbank)%>%pull(idbank)

data = 
  get_insee_idbank(series) %>%
  split_title() %>% 
  add_insee_metadata()


data_ipc03<-data%>%
  select(DATE,OBS_VALUE,COICOP2016)%>%
  pivot_wider(names_from = COICOP2016,values_from =OBS_VALUE,names_prefix = "I")%>%
  arrange(DATE)


# 2 digits

nomen<-idbank_list%>%select(COICOP2016,COICOP2016_label_fr)%>%
  group_by(COICOP2016)%>%
  slice(1)%>%ungroup()%>%
  mutate(count_string=str_length(COICOP2016))%>%
  filter(count_string==2)%>%
  select(-count_string)%>%filter(COICOP2016 !=(c("00","SO")) )


series<-left_join(idbank_list,nomen,by="COICOP2016")%>%
  filter(FREQ==freq,
         NATURE==nat,
         CORRECTION==corr,
         MENAGES_IPC==men,
         BASIND==base,
         REF_AREA==ref)%>%
  mutate(count_string=str_length(COICOP2016))%>%
  filter(count_string==2)%>%filter(COICOP2016 %in% (nomen$COICOP2016))%>%select(idbank)%>%pull(idbank)

data = 
  get_insee_idbank(series) %>%
  split_title() %>% 
  add_insee_metadata()


data_ipc02<-data%>%
  select(DATE,OBS_VALUE,COICOP2016)%>%
  pivot_wider(names_from = COICOP2016,values_from =OBS_VALUE,names_prefix = "I")%>%
  arrange(DATE)

rm(data,idbank_list,nomen,base,corr,freq,men,nat,ref,series,source)


# DATA IPC FINAL 

data_ipc_04_BDF<-data_ipc04%>%
  mutate(I0221 = I0220,
         I0412 = I0411,
         I0450 = (I0451+I0452)/2,
         I0521 = I0520,
         I0541 = I0540,
         I0571 = I0561,
         I0811 = I0810,
         I0812 = I0820,
         I0813 = I0830,
         I0961 = I0960,
         I1012 = I1020,
         I1013 = I1040,
         I1121 = I1120,
         I1241 = I1240,
         I1261 = I1262,
         I1271 = I1270)

data_ipc_03_BDF<-data_ipc03%>%
  mutate(I0714 = I071,
         I1113 = I111,
         I1233 = I123,
         I1251 = I125,
         I1255 = I125)

data_ipc_02_BDF<-data_ipc02%>%
  mutate(I0131 = I01,
         I0241 = I02,
         I0331 = I03,
         I0461 = I04,
         I0631 = I06,
         I0641 = I06,
         I0741 = I07,
         I0814 = I08,
         I0971 = I09,
         I1011 = I10,
         I1014 = I10,
         I1015 = I10,
         I1281 = I12,
         I1291 = I12)  

data_ipc_BDF<-data_ipc_04_BDF%>%
  merge(data_ipc_03_BDF,by="DATE")%>%
  merge(data_ipc_02_BDF,by="DATE")


# Traitement spécial 0311 0923 et 0943
data_ipc_BDF_1 <-data_ipc_BDF %>%
  mutate(I092=I092/I0923[DATE=="2016-01-01"]*100,
         I094=I094/I0943[DATE=="2016-01-01"]*100,
         I031=I031/I031[DATE=="2015-12-01"]*I0311[DATE=="2015-12-01"])%>%
  mutate(I0923=if_else(is.na(I0923),I092,I0923),
         I0943=if_else(is.na(I0943),I094,I0943),
         I0311=if_else(is.na(I0311),I031,I0311)) %>%
  pivot_longer(!DATE, names_to = "Produits", values_to = "value") %>%
  pivot_wider(names_from = DATE, values_from = value) 

prev_ipc_alim <- data_ipc_BDF_1 %>% filter(str_detect(Produits, "I01")==TRUE | str_detect(Produits, "I02")==TRUE) %>% 
  mutate(`2022-06-01`=1.007*`2022-05-01`,
         `2022-07-01`=1.006*`2022-06-01`,
         `2022-08-01`=1.006*`2022-07-01`,
         `2022-09-01`=1.006*`2022-08-01`,
         `2022-10-01`=1.004*`2022-09-01`,
         `2022-11-01`=1.004*`2022-10-01`,
         `2022-12-01`=1.004*`2022-11-01`) %>%
  pivot_longer(!Produits, names_to = "DATE", values_to = "value") %>%
  pivot_wider(names_from = Produits, values_from = value)

prev_ipc_NRJ <- data_ipc_BDF_1 %>% filter(str_detect(Produits, "I045")==TRUE) %>% 
  mutate(`2022-06-01`=1.004*`2022-05-01`,
         `2022-07-01`=1.000*`2022-06-01`,
         `2022-08-01`=1.000*`2022-07-01`,
         `2022-09-01`=1.000*`2022-08-01`,
         `2022-10-01`=1.000*`2022-09-01`,
         `2022-11-01`=1.000*`2022-10-01`,
         `2022-12-01`=1.000*`2022-11-01`) %>%
  pivot_longer(!Produits, names_to = "DATE", values_to = "value") %>%
  pivot_wider(names_from = Produits, values_from = value)

prev_ipc_loyer <- data_ipc_BDF_1 %>% filter(str_detect(Produits, "I041")==TRUE) %>% 
  mutate(`2022-06-01`=1.000*`2022-05-01`,
         `2022-07-01`=1.035*`2022-06-01`,
         `2022-08-01`=1.000*`2022-07-01`,
         `2022-09-01`=1.000*`2022-08-01`,
         `2022-10-01`=1.000*`2022-09-01`,
         `2022-11-01`=1.000*`2022-10-01`,
         `2022-12-01`=1.000*`2022-11-01`) %>%
  pivot_longer(!Produits, names_to = "DATE", values_to = "value") %>%
  pivot_wider(names_from = Produits, values_from = value)

prev_ipc_autres <- data_ipc_BDF_1 %>% filter(str_detect(Produits, "I045")==FALSE & str_detect(Produits, "I041")==FALSE & str_detect(Produits, "I01")==FALSE & str_detect(Produits, "I02")==FALSE ) %>% 
  mutate(`2022-06-01`=1.000*`2022-05-01`,
         `2022-07-01`=1.004*`2022-06-01`,
         `2022-08-01`=1.003*`2022-07-01`,
         `2022-09-01`=1.003*`2022-08-01`,
         `2022-10-01`=1.003*`2022-09-01`,
         `2022-11-01`=1.003*`2022-10-01`,
         `2022-12-01`=1.002*`2022-11-01`) %>%
  pivot_longer(!Produits, names_to = "DATE", values_to = "value") %>%
  pivot_wider(names_from = Produits, values_from = value)

data_ipc_BDF <- prev_ipc_alim %>%
  left_join(prev_ipc_loyer, by=c("DATE"))%>%
  left_join(prev_ipc_NRJ, by=c("DATE"))%>%
  left_join(prev_ipc_autres, by=c("DATE"))

rm(data_ipc02,data_ipc03,data_ipc04,data_ipc_02_BDF,data_ipc_03_BDF,data_ipc_04_BDF)
