source("data_ipc2.R")
source("data_bdf.R")
#source("data_diagnostic.R")

memory.limit(size=40000) # J'ai eu besoin de ça vu la taille de la base sur mon poste perso


var<-substr((colnames(data_bdf_c04))[3:length(colnames(data_bdf_c04))],2,5)

final_data <- expand_grid(IDENT_MEN = data_bdf_c04$IDENT_MEN, DATE=data_ipc_BDF$DATE)%>%
  # filter(IDENT_MEN<2)%>%
  merge(data_bdf_c04,by="IDENT_MEN")%>%
  merge(data_ipc_BDF,by="DATE")

final_data_temp<-matrix(NA,dim(final_data)[1],length(var))

for (i in 1:dim(final_data_temp)[2]){
  final_data_temp[,i]<-final_data[,paste0("C",var[i])]*final_data[,paste0("I",var[i])]
}

final_data_temp<-final_data_temp%>%as.data.frame()
colnames(final_data_temp)<-var

final_data<-final_data%>%
  cbind(final_data_temp)%>%
  select(DATE,IDENT_MEN,pondmen,all_of(var))%>%
  mutate(ipc=rowSums(across(var)))%>%
  select(DATE,IDENT_MEN,pondmen,ipc)%>%
  arrange(IDENT_MEN,DATE)


fwrite(final_data, "IPC_men.csv")
fwrite(data_bdf_c04, "C04.csv")
fwrite(data_ipc_BDF, "IPC_nomenBDF.csv")

# test<-fread("IPC_men.csv", header= TRUE)

rm(final_data_temp,data_bdf_c04,data_ipc_BDF)

gc()

