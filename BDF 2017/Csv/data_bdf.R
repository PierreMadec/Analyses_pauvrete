fread("C05.csv", header= TRUE)-> data_bdf_c05

data_temp<-data_bdf_c05%>%
  select(IDENT_MEN,CTOT)%>%
  arrange(IDENT_MEN)

data_bdf_c04<-data_bdf_c05 %>%
  pivot_longer(cols=2:(dim(data_bdf_c05)[2]-1),names_to="COICOP_5d",values_to="C_5d")%>%
  mutate(COICOP_4d=str_sub(COICOP_5d,2,5))%>%
  group_by(IDENT_MEN,COICOP_4d)%>%
  mutate(C_4d=sum(C_5d))%>%
  slice(1)%>%
  ungroup()%>%
  filter(COICOP_5d!="CTOT")%>%
  select(-c(COICOP_5d,C_5d))%>%
  merge(data_temp,by="IDENT_MEN")%>%
  mutate(sh_4d=C_4d/CTOT)%>%
  select(IDENT_MEN,pondmen,COICOP_4d,sh_4d)%>%
  pivot_wider(names_from = "COICOP_4d",values_from = "sh_4d",names_prefix = "C")%>%
  select((!starts_with(c("C13","C14")))) 
  
# data_bdf_c04<-data_bdf_c04[,is.element(colnames(data_bdf_c04),c("C1311","C1312","C1314","C1315","C1316","C1321","C1322","C1331","C1341","C1342","C1351","C1361","C1371","C1372","C1411"))==FALSE]

rm(data_bdf_c05,data_temp)
