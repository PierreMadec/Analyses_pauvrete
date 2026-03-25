# Diagnostic sur les IPC disponibles

var<-colnames(data_bdf_c04)[3:123]%>%as.data.frame()%>%mutate(name=paste0("I",substr(`.`,2,5)))%>%pull(name)
data_ipc_BDF<-data_ipc_BDF%>%select("DATE",all_of(var))
data_ipc_BDF$manq<-rowSums(is.na(data_ipc_BDF))
data_ipc_BDF<-data_ipc_BDF%>%relocate("DATE","manq")

data_ipc_BDF[data_ipc_BDF$DATE=="1999-01-01",] %>% summarise(across(everything(), ~ sum(is.na(.))))

plot(data_ipc_BDF$DATE,data_ipc_BDF$manq,type="o",xlab="Date",ylab="IPC manquants")

## Tous les IPC à partir de 2000-01-01 ##
## Pour étendre à tout l'échantillon pour 1998 il faut ajouter 12.53 assurance lié à la santé##

data_ipc_BDF<-data_ipc_BDF%>%select(-manq)%>%
  filter(DATE>=start_date)

# test<-data_bdf_c04[,var[1]]
