library(lubridate)
library(ggplot2)
library(plotrix)
library(Hmisc)
library(data.table)
library(tidyverse)

#Paramètres du code

last.month<-"03"

last.simulated.month<-03


ofce<-0
ma<-0


#Paramètres vieillissement des revenus

viellissement_chom     <- 3.56/100  # IPC moyenne 2017 - IPC 2020
viellissement_ret      <- -2.1/100  # Fiche 4 - Evolution pension moyenne https://drees.solidarites-sante.gouv.fr/publications-documents-de-reference/panoramas-de-la-drees/les-retraites-et-les-retraites-edition-0#:~:text=La%20pension%20moyenne%20tous%20r%C3%A9gimes,pensions%20inf%C3%A9rieure%20%C3%A0%20l'inflation. 
viellissement_revindep <- 5.9/100   # Evol Compta nat Rev EI ménages / Emploi non salarié
viellissement_sal      <- 4.9/100   # Evol Compta nat Masse salariale / Emploi salarié ETP
viellissement_autres   <- 3.56/100  #  IPC moyenne 2017 - IPC 2020


prev_inflation <- 0.9  # Prev HICP ECFIN en MA pour 2021 - Nov 2020 https://ec.europa.eu/economy_finance/forecasts/2020/autumn/ecfin_forecast_autumn_2020_fr_en.pdf
ipc2021 <- 5.1 #GA IPCH 12/2021 - pour graphique 1

# prev_inflation <- 0.6  # Prev IPC en MA pour 2021 selon le FMI dans le WEO d'octobre 2020 https://www.imf.org/en/Publications/WEO/Issues/2020/09/30/world-economic-outlook-october-2020#Statistical%20Appendix

## DATA FINALE##


# Importation BdF 2017

fread("INDIVIDU.csv", header= TRUE) -> IND
fread("MENAGE.csv", header= TRUE)   -> MEN17
fread("IPC_men.csv", header= TRUE)  -> data
fread("DEPINDIV.csv", header= TRUE) -> DEPIND
fread("IPC_men.csv", header= TRUE)  -> IPC_men
fread("C05.csv", header= TRUE)     ->  C05

data_ind<-IND%>%
  filter(AG>16)%>%
  mutate(elig=1*(((CHOMAGE*(1+viellissement_chom)+RETRAITES*(1+viellissement_ret)+REVINDEP*(1+viellissement_revindep)+SALAIRES*(1+viellissement_sal)))/12)<=2000)%>%
  mutate(cheque=100*(elig==TRUE))%>%
  select(IDENT_MEN,cheque)%>%
  merge((MEN17%>%select(IDENT_MEN,pondmen)),by="IDENT_MEN")%>%
  mutate(pondelig=pondmen*(cheque>0))%>%
  select(-pondmen)

indiv_elig<-round(sum(data_ind$pondelig)/1000000,1)

data_ind<-data_ind%>%
  select(-pondelig)%>%
  group_by(IDENT_MEN)%>%
  mutate(SUM_CHEQUE=sum(cheque))%>%
  filter(row_number(IDENT_MEN)==1)%>%
  ungroup()%>%
  select(IDENT_MEN,SUM_CHEQUE)


if (ma!=0){
  # Inflation en moyenne annuelle
  data<-data%>%
    mutate(annee=year(DATE))%>%
    filter(annee == 2020 | annee==2021)%>%
    group_by(IDENT_MEN,annee)%>%
    summarise(ipc=mean(ipc))%>%
    ungroup()%>%
    group_by(IDENT_MEN)%>%
    mutate(inflation=100*(ipc[2]/ipc[1]-1))%>%
    filter(row_number(annee) == 2)%>%
    ungroup()%>%
    select(IDENT_MEN,inflation)
}

if (ma==0){
  # Inflation en glissement annuel
  data<-data%>%
    filter(DATE   == paste0("2019-", last.month, "-01", collapse = "")  |
             DATE == paste0("2020-", last.month, "-01", collapse = "")  | 
             DATE == paste0("2021-", last.month, "-01", collapse = "")  | 
             DATE == paste0("2022-", last.month, "-01", collapse = ""))%>%
    group_by(IDENT_MEN)%>%
    mutate(inflation=100*(ipc[4]/ipc[1]-1))%>%
    mutate(inflation2=100*(ipc[4]/ipc[3]-1))%>%
    filter(row_number(DATE) == 4)%>%
    ungroup()%>%
    select(IDENT_MEN,inflation,inflation2)
}

data_cheque<-merge(data_ind,MEN17,by="IDENT_MEN")%>%
  select(IDENT_MEN,pondmen,SUM_CHEQUE)

montant_cheque<- round(sum(data_cheque$SUM_CHEQUE*data_cheque$pondmen)/1000/1000000,2)

data_cheque_NRJ<-MEN17%>%
  mutate(uc_chq=1+0.25*(NPERS==1.5)+0.5*(NPERS==2)+0.3*(NPERS-2)*(NPERS>2),
         REVPRIM0=0.9*(CHOMAGE*(1+0)+RETRAITES*(1+0)+REVINDEP*(1+0)+SALAIRES*(1+0)),
         REVPRIM=0.9*(CHOMAGE*(1+viellissement_chom)+RETRAITES*(1+viellissement_ret)+REVINDEP*(1+viellissement_revindep)+SALAIRES*(1+viellissement_sal)),
         rev_chq=round(REVPRIM/uc_chq,1),
         mtq_chq=144*(uc_chq==1)+190*(uc_chq>1)*(uc_chq<2)+227*(uc_chq>=2),
         mtq_chq2=(rev_chq<5600)*round(mtq_chq/1,1)+
           (rev_chq>=5600)*(rev_chq<6700)*round(mtq_chq/1.5,1)+
           (rev_chq>=6700)*(rev_chq<7700)*round(mtq_chq/3,1)+
           (rev_chq>=7700)*(rev_chq<10700)*round(mtq_chq/3,1)+
           (rev_chq>=10700)*0+50*(rev_chq<7700),
         vieillissement=REVPRIM/REVPRIM0,
         SUM_CHEQUE_NRJ=100*(mtq_chq2>0))%>%
  select(IDENT_MEN,uc_chq,rev_chq,mtq_chq,mtq_chq2,pondmen,vieillissement,SUM_CHEQUE_NRJ)

montant_cheque_NRJ <- round(sum(data_cheque_NRJ$pondmen*(data_cheque_NRJ$SUM_CHEQUE_NRJ))/1000000/1000,2)
menag_elig_NRJ     <- round(sum(data_cheque_NRJ$pondmen[data_cheque_NRJ$SUM_CHEQUE_NRJ>0])/1000000,1)

viellissement_REV<-data_cheque_NRJ%>%
  select(IDENT_MEN,vieillissement)

viellissement_REV$vieillissement[is.nan(viellissement_REV$vieillissement)]<-viellissement_autres


data_cheque_NRJ<-data_cheque_NRJ%>%
  select(IDENT_MEN,SUM_CHEQUE_NRJ,pondmen)


datatemp0<-IPC_men%>%
  mutate(annee=year(DATE))%>%
  group_by(IDENT_MEN,annee)%>%
  summarise(ipc=mean(ipc))%>%
  ungroup()%>%
  group_by(IDENT_MEN)%>%
  mutate(inflation=100*(ipc/lag(ipc)-1))%>%
  filter(annee>=2017)%>%
  # filter(row_number(annee) >1 )%>%
  ungroup()%>%
  select(-c(ipc))%>%
  # select(-annee)%>%
  pivot_wider(names_from = annee,values_from = inflation)%>%
  mutate(moyenne=(`2017`+`2018`+`2019`)/3)%>%
  mutate(viellissement_conso=(1+`2018`/100)*(1+`2019`/100))%>%
  select(IDENT_MEN,moyenne,viellissement_conso)

datatemp1<-IPC_men%>%
  mutate(annee=year(DATE),
         mois=month(DATE))%>%
  filter(annee==2017 | annee==2021| annee == 2022)%>%
  group_by(IDENT_MEN)%>%
  expand(annee,mois)%>%
  ungroup()%>%
  left_join(IPC_men%>%
              mutate(annee=year(DATE),
                     mois=month(DATE)),
            by=c("IDENT_MEN","annee","mois"))%>%
  fill(ipc,pondmen)%>%
  filter(is.na(DATE)==FALSE | ((is.na(DATE)==TRUE) & (mois<=last.simulated.month)))%>%
  group_by(IDENT_MEN,annee)%>%
  summarise(ipc=mean(ipc))%>%
  ungroup()%>%
  pivot_wider(names_from = annee,values_from = ipc)%>%
  mutate(inflation2021=100*(`2021`/`2017`-1),
         inflation2022=100*(`2022`/`2017`-1))%>%
  select(IDENT_MEN,inflation2021,inflation2022)
    
datatemp2<-merge(datatemp0,datatemp1,by="IDENT_MEN")%>%
  mutate(inflationCF2021=viellissement_conso*(1+moyenne/100)*(1+moyenne/100),
         inflationCF2022=inflationCF2021*(1+moyenne/100),
         inflationCOVID2021=(inflation2021/100)+1,
         inflationCOVID2022=(inflation2022/100)+1)%>%
  mutate(choc_2021=inflationCOVID2021-inflationCF2021,
         choc_2022=inflationCOVID2022-inflationCF2022,
         pos_bis=1*(choc_2022>0))%>%
  filter(is.na(pos_bis)==FALSE)%>%
  merge(MEN17%>%select(IDENT_MEN,pondmen,DNIVIE2),by="IDENT_MEN")%>%
  select(IDENT_MEN,inflationCOVID2021,inflationCOVID2022,inflationCF2021,inflationCF2022,viellissement_conso,pos_bis,DNIVIE2,pondmen)

shr_choc_bis<-round(wtd.mean(datatemp2$pos_bis, weights = datatemp2$pondmen),2)

tab2022<-datatemp2%>%
  group_by(DNIVIE2)%>%
  summarise(Npos    = sum(pondmen*pos_bis),
            N       = sum(pondmen))%>%
  mutate(Npos=round(100*Npos/N,0))%>%
  ungroup()

# datatemp2<-datatemp0%>%
#   select(IDENT_MEN,`2019`,`2020`,`2021`,moyenne,viellissement_conso,vieillissement_conso_bis)

# datatemp<-IPC_men%>%
#   mutate(annee=year(DATE))%>%
#   filter(annee == 2017 | annee == 2020 | annee==2021)%>%
#   group_by(IDENT_MEN,annee)%>%
#   summarise(ipc=mean(ipc))%>%
#   ungroup()%>%
#   group_by(IDENT_MEN)%>%
#   mutate(inflation=100*(ipc/ipc[1]-1))%>%
#   filter(row_number(annee) >1 )%>%
#   ungroup()%>%
#   select(-c(ipc))%>%
#   # select(-annee)%>%
#   pivot_wider(names_from = annee,values_from = inflation)
# 
# datatemp2<-IPC_men%>%
#   mutate(annee=year(DATE))%>%
#   filter(annee == 2019 | annee == 2020 )%>%
#   group_by(IDENT_MEN,annee)%>%
#   summarise(ipc=mean(ipc))%>%
#   ungroup()%>%
#   group_by(IDENT_MEN)%>%
#   mutate(inflation2020=100*(ipc/ipc[1]-1))%>%
#   filter(row_number(annee) >1 )%>%
#   ungroup()%>%
#   select(-c(ipc,annee))
# 

CTOT <- C05 %>% select(IDENT_MEN, CTOT)%>%
  merge(datatemp2,by="IDENT_MEN")%>%
  mutate(CTOT2020=CTOT*viellissement_conso,
         CTOT2021=CTOT*inflationCOVID2021,
         CTOT2022=(last.simulated.month/12)*CTOT*inflationCOVID2022,
         CTOT2021CF=CTOT*inflationCF2021,
         CTOT2022CF=(last.simulated.month/12)*CTOT*inflationCF2022,
         Perte2021=CTOT2021CF-CTOT2021,
         Perte2022=CTOT2022CF-CTOT2022)%>%
  select(IDENT_MEN,Perte2021,Perte2022,CTOT,CTOT2020,CTOT2021,CTOT2021CF,CTOT2022CF)


data_bdf<-merge(MEN17,CTOT,by="IDENT_MEN")

data_final<-merge(data_bdf,data,by="IDENT_MEN")%>%
  mutate(AGEPR2=1*(AGEPR<30)+2*(AGEPR>=30)*(AGEPR<40)+3*(AGEPR>=40)*(AGEPR<50)+4*(AGEPR>=50)*(AGEPR<65)+5*(AGEPR>=65))%>%
  mutate(TUU2=0*(TUU==0)+1*(TUU>0)+1*(TUU==8))%>%
  merge(data_cheque%>%select(-pondmen),by="IDENT_MEN")%>%
  merge(data_cheque_NRJ%>%select(-pondmen),by="IDENT_MEN")%>%
    filter(is.na(inflation)==FALSE)

data_2<-DEPIND%>%
  merge(data_final%>%select(IDENT_MEN,inflation),by="IDENT_MEN")%>%
  merge(MEN17,by="IDENT_MEN")

INF1<-data_2%>%
  filter(MODE_TRANS1==1)%>%
  select(inflation,pondmen)%>%
  summarise(inflation=round(wtd.mean(inflation, weights = pondmen),2),
            N= sum(pondmen))%>%
  mutate(TPT=1)

INF3<-data_2%>%
  filter(MODE_TRANS3==1)%>%
  select(inflation,pondmen)%>%
  summarise(inflation=round(wtd.mean(inflation, weights = pondmen),2),
            N= sum(pondmen))%>%
  mutate(TPT=3)

INF5<-data_2%>%
  filter(MODE_TRANS5==1)%>%
  select(inflation,pondmen)%>%
  summarise(inflation=round(wtd.mean(inflation, weights = pondmen),2),
            N= sum(pondmen))%>%
  mutate(TPT=5)

INF_TPT<-rbind(INF1,INF3,INF5)%>%mutate(N=N)

rm(INF1,INF3,INF5,data_2)
rm(data_bdf,data,IND,data_cheque,data_ind)
rm(datatemp0)

## GRAPH

#Graph 1 : hétérogénéité d'inflation

data_hist<-data_final%>%
  select(pondmen,CTOT, inflation2)%>%
  filter(inflation2<10 & inflation2>0)

range(data_hist$inflation2)

gr1<- weighted.hist(data_hist$inflation,data_hist$pondmen*data_hist$CTOT,freq=FALSE,
              breaks=seq(from=0,to=10,length=15),
              main="Distribution de l'inflation par ménage",
              xlab = "Inflation",
              ylab = "Densité")

abline(v = ipc2021, col = "blue")

rm(data_hist)

# #Graph 2 : hétérogénéité du rencherissement du panier des biens 
# 
# data_hist2<-data_final%>%
#   select(pondmen,Perte,REVTOT)%>%
#   filter(Perte>0)%>%
#   filter(REVTOT>0)%>%
#   mutate(Perte_pct=Perte/REVTOT)%>%
#   filter(Perte_pct<0.1)
# 
# wtd.mean(data_hist2$Perte, weights = data_hist2$pondmen)
# range(data_hist2$Perte)
# 
# gr2<- weighted.hist(data_hist2$Perte,data_hist2$pondmen,freq=FALSE,
#               breaks=seq(from=0,to=9000,length=19),
#               main="Distribution du rencherissement du panier de consommation (en euros)",
#               ylab = "Densité")
# abline(v = wtd.mean(data_hist2$Perte, weights = data_hist2$pondmen), col = "blue")
# 
# 
# gr3<- weighted.hist(data_hist2$Perte_pct*100,data_hist2$pondmen,freq=FALSE,
#               breaks=seq(from=0,to=0.1*100,length=11*2-1),
#               main="Distribution du rencherissement du panier de consommation (en % du revenu)",
#               ylab = "Densité")
# abline(v = summary(data_hist2$Perte_pct)[4]*100, col = "blue")
# 
# rm(data_hist2)

## FONCTION 

calculs<-function(x,y){
  xt<-parse(text=x)
  yt<-parse(text=y)
  outdata<-data_final%>%
    group_by(eval(xt))%>%
    summarise(moyenne = round(wtd.mean(eval(parse(text=y0)), weights = pondmen*CTOT2020),2),
              et      = round(sqrt(wtd.mean(((eval(parse(text=y0))-moyenne)^2), weights =  pondmen*CTOT2020)),2),
              q10     = round(wtd.quantile(eval(parse(text=y0)), weights =  pondmen*CTOT2020, probs = 0.10, type = 'quantile'),2),
              q25     = round(wtd.quantile(eval(parse(text=y0)), weights =  pondmen*CTOT2020, probs = 0.25, type = 'quantile'),2),
              q50     = round(wtd.quantile(eval(parse(text=y0)), weights =  pondmen*CTOT2020, probs = 0.50, type = 'quantile'),2),
              q75     = round(wtd.quantile(eval(parse(text=y0)), weights =  pondmen*CTOT2020, probs = 0.75, type = 'quantile'),2),
              q90     = round(wtd.quantile(eval(parse(text=y0)), weights =  pondmen*CTOT2020, probs = 0.90, type = 'quantile'),2),
              N       = sum(pondmen))%>%
    mutate(N=round(100*N/sum(N),0))%>%
    ungroup()
  colnames(outdata)[1]<-x
  return(outdata)
}


## RESULTATS

#Inflation

y0<-"inflation2"

stat_inflation <- data_final%>%
  summarise(moyenne = round(wtd.mean(eval(parse(text=y0)), weights = pondmen),2),
            et      = round(sqrt(wtd.mean(((eval(parse(text=y0))-moyenne)^2), weights = pondmen)),2),
            q10     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.1, type = 'quantile'),2),
            q20     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.2, type = 'quantile'),2),
            q30     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.3, type = 'quantile'),2),
            q40     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.4, type = 'quantile'),2),
            q50     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.5, type = 'quantile'),2),
            q60     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.6, type = 'quantile'),2),
            q70     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.7, type = 'quantile'),2),
            q80     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.8, type = 'quantile'),2),
            q90     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.9, type = 'quantile'),2),
            N       = sum(pondmen))%>%
  mutate(N=round(100*N/sum(N),0))

stat_inflation2 <- data_final%>%
  summarise(moyenne = round(wtd.mean(eval(parse(text=y0)), weights = pondmen*CTOT2020),2),
            et      = round(sqrt(wtd.mean(((eval(parse(text=y0))-moyenne)^2), weights = pondmen*CTOT2020)),2),
            q10     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.1, type = 'quantile'),2),
            q20     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.2, type = 'quantile'),2),
            q30     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.3, type = 'quantile'),2),
            q40     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.4, type = 'quantile'),2),
            q50     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.5, type = 'quantile'),2),
            q60     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.6, type = 'quantile'),2),
            q70     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.7, type = 'quantile'),2),
            q80     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.8, type = 'quantile'),2),
            q90     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.9, type = 'quantile'),2),
            q95     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.95, type = 'quantile'),2),
            q97     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.97, type = 'quantile'),2),
            q98     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.98, type = 'quantile'),2),
            q99     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen*CTOT2020, probs = 0.99, type = 'quantile'),2),
            N       = sum(pondmen))%>%
  mutate(N=round(100*N/sum(N),0))

stat_inflation_TYPMEN<-calculs(x="TYPMEN5",y=y0)%>%select(TYPMEN5,moyenne,q50)
stat_inflation_AGEPR <-calculs(x="AGEPR2",y=y0)
stat_inflation_TUU <-calculs(x="TUU2",y=y0)%>%select(TUU2,moyenne,q50)
stat_inflation_DEC <-calculs(x="DNIVIE2",y=y0)%>%select(DNIVIE2,moyenne,q90)
stat_inflation_SITUA <-calculs(x="SITUAPR",y=y0)%>%select(SITUAPR,moyenne,q50)
stat_inflation_AISE <-calculs(x="Aise",y=y0)%>%select(Aise,moyenne,q50,N)
stat_inflation_CHOM <-calculs(x="Chomagea",y=y0)


stat_inflation_DEC_TUU <- data_final%>%
  group_by(TUU2,DNIVIE2)%>%
  summarise(moyenne = round(wtd.mean(eval(parse(text=y0)), weights = pondmen*CTOT2020),2))%>%
  ungroup()%>%
  pivot_wider(names_from = "TUU2",values_from = "moyenne")

fwrite(stat_inflation_DEC_TUU,file="DEC_TUU.csv")

# #Rencherissement panier
# 
# y0<-"Pertec"
# 
# stat_Perte <- data_final%>%
#   summarise(moyenne = round(wtd.mean(eval(parse(text=y0)), weights = pondmen),0),
#             q10     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.1, type = 'quantile'),0),
#             q20     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.2, type = 'quantile'),0),
#             q30     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.3, type = 'quantile'),0),
#             q40     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.4, type = 'quantile'),0),
#             q50     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.5, type = 'quantile'),0),
#             q60     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.6, type = 'quantile'),0),
#             q70     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.7, type = 'quantile'),0),
#             q80     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.8, type = 'quantile'),0),
#             q90     = round(wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.9, type = 'quantile'),0),
#             N       = sum(pondmen))%>%
#   mutate(N=round(100*N/sum(N),0))
# 
# stat_Perte_TYPMEN<-calculs(x="TYPMEN5",y=y0)
# stat_Perte_AGEPR <-calculs(x="AGEPR2",y=y0)
# stat_Perte_TUU <-calculs(x="TUU2",y=y0)
# stat_Perte_DEC <-calculs(x="DNIVIE2",y=y0)
# stat_Perte_SITUA <-calculs(x="SITUAPR",y=y0)

#CTOT

# y0<-"CTOT"
# 
# stat_CTOT <- data_final%>%
#   summarise(moyenne = wtd.mean(eval(parse(text=y0)), weights = pondmen),
#             q10 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.1, type = 'quantile'),
#             q20 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.2, type = 'quantile'),
#             q30 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.3, type = 'quantile'),
#             q40 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.4, type = 'quantile'),
#             q50 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.5, type = 'quantile'),
#             q60 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.6, type = 'quantile'),
#             q70 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.7, type = 'quantile'),
#             q80 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.8, type = 'quantile'),
#             q90 = wtd.quantile(eval(parse(text=y0)), weights = pondmen, probs = 0.9, type = 'quantile'),
#             N =   sum(pondmen))

# stat_CTOT_TYPMEN<-calculs(x="TYPMEN5",y=y0)
# stat_CTOT_AGEPR <-calculs(x="AGEPR2",y=y0)
# stat_CTOT_TUU <-calculs(x="TUU2",y=y0)
# stat_CTOT_DEC <-calculs(x="DNIVIE2",y=y0)
# stat_CTOT_SITUA <-calculs(x="SITUAPR",y=y0)

## https://www.book.utilitr.org/surveydata.html

#Distribution du Chèque inflation

freq_CHEQUE <- data_final%>%
  group_by(DNIVIE2)%>%
  summarise(NBMEN = sum(pondmen),
            NBMEN_Cheque =sum(pondmen*(SUM_CHEQUE!=0)),
            II=sum(pondmen*SUM_CHEQUE))%>%
  ungroup()%>%
  mutate(freq_CHEQUE=NBMEN_Cheque/NBMEN*100)%>%
  select(DNIVIE2,freq_CHEQUE,II)
    
fwrite(freq_CHEQUE,file="Freq.csv")

freq_CHEQUE_NRJ <- data_final%>%
  group_by(DNIVIE2)%>%
  summarise(NBMEN = sum(pondmen),
            NBMEN_Cheque =sum(pondmen*(SUM_CHEQUE_NRJ!=0)),
            CE=sum(pondmen*SUM_CHEQUE_NRJ))%>%
  ungroup()%>%
  mutate(freq_CHEQUE_NRJ=NBMEN_Cheque/NBMEN*100)%>%
  select(DNIVIE2,freq_CHEQUE_NRJ,CE)

fwrite(freq_CHEQUE_NRJ,file="Freq_NRJ.csv")


# Compensation des pertes Ensemble Crise Sanitaire

data_compens<-data_final%>%
  select(IDENT_MEN,pondmen,SUM_CHEQUE,SUM_CHEQUE_NRJ,REVDISP,DNIVIE2,Perte2021,Perte2022)%>%
  merge(viellissement_REV,by="IDENT_MEN")%>%
  mutate(Delta_REV=SUM_CHEQUE+SUM_CHEQUE_NRJ+Perte2021+Perte2022)%>%
  mutate(Gagnant=1*(Delta_REV>0)-1*(Delta_REV<0))

stat_compens<-data_compens%>%
  group_by(DNIVIE2)%>%
  summarise(N=sum(pondmen),
            RD=sum(pondmen*REVDISP*vieillissement*(1+last.simulated.month/12)),
            II=sum(pondmen*SUM_CHEQUE),
            CE=sum(pondmen*SUM_CHEQUE_NRJ),
            P21=sum(pondmen*Perte2021),
            P22=sum(pondmen*Perte2022),
            Delta_REV=sum(pondmen*Delta_REV),
            N_G=sum(pondmen*(Gagnant== 1)),
            N_P=sum(pondmen*(Gagnant==-1)))%>%
  ungroup()%>%
  mutate(II_pct=round(II/RD*100,2),
         CE_pct=round(CE/RD*100,2),
         P21_pct =round(P21/RD*100,2),
         P22_pct =round(P22/RD*100,2),
         pct_G =round(N_G/N*100,2),
         pct_P =round(N_P/N*100,2),
         Delta_NdV=round(Delta_REV/RD*100,2))%>%
  select(DNIVIE2,II_pct,CE_pct,P21_pct,P22_pct,Delta_NdV,pct_G,pct_P)

fwrite(stat_compens,file="Resultats.csv")

stat_compens_macro<-data_compens%>%
  summarise(N=sum(pondmen),
            RD=sum(pondmen*REVDISP*vieillissement*(1+last.simulated.month/12)),
            II=sum(pondmen*SUM_CHEQUE),
            CE=sum(pondmen*SUM_CHEQUE_NRJ),
            P21=sum(pondmen*Perte2021),
            P22=sum(pondmen*Perte2022),
            N_G=sum(pondmen*(Gagnant== 1)),
            N_P=sum(pondmen*(Gagnant==-1)),
            Delta_NdV=sum(pondmen*Delta_REV))%>%
  mutate(II_pct=round(II/RD*100,2),
         CE_pct=round(CE/RD*100,2),
         P21_pct =round(P21/RD*100,2),
         P22_pct =round(P22/RD*100,2),
         pct_G=round(N_G/N*100,2),
         pct_P=round(N_P/N*100,2),
         Delta_NdV=round(Delta_NdV/RD*100,2))%>%
  select(II_pct,CE_pct,P21_pct,P22_pct,Delta_NdV,pct_G,pct_P)

fwrite(stat_compens_macro,file="Resultats_macro.csv")

N_men<-(data_compens%>%summarise(N=sum(pondmen)))[1,1]


# En filtrant sur les perdants

stat_compens_perdants<-data_compens%>%
  filter(Perte2021+Perte2022<0)%>%
  group_by(DNIVIE2)%>%
  summarise(N=sum(pondmen),
            RD=sum(pondmen*REVDISP*vieillissement*(1+last.simulated.month/12)),
            II=sum(pondmen*SUM_CHEQUE),
            CE=sum(pondmen*SUM_CHEQUE_NRJ),
            P21=sum(pondmen*Perte2021),
            P22=sum(pondmen*Perte2022),
            Delta_REV=sum(pondmen*Delta_REV),
            N_G=sum(pondmen*(Gagnant== 1)),
            N_P=sum(pondmen*(Gagnant==-1)))%>%
  ungroup()%>%
  mutate(II_pct=round(II/RD*100,2),
         CE_pct=round(CE/RD*100,2),
         P21_pct =round(P21/RD*100,2),
         P22_pct =round(P22/RD*100,2),
         pct_G =round(N_G/N*100,2),
         pct_P =round(N_P/N*100,2),
         Delta_NdV=round(Delta_REV/RD*100,2))%>%
  select(DNIVIE2,II_pct,CE_pct,P21_pct,P22_pct,Delta_NdV,pct_G,pct_P)

stat_compens_macro_perdants<-data_compens%>%
  filter(Perte2021+Perte2022<0)%>%
  summarise(N=sum(pondmen),
            RD=sum(pondmen*REVDISP*vieillissement*(1+last.simulated.month/12)),
            II=sum(pondmen*SUM_CHEQUE),
            CE=sum(pondmen*SUM_CHEQUE_NRJ),
            P21=sum(pondmen*Perte2021),
            P22=sum(pondmen*Perte2022),
            N_G=sum(pondmen*(Gagnant== 1)),
            N_P=sum(pondmen*(Gagnant==-1)),
            Delta_NdV=sum(pondmen*Delta_REV))%>%
  mutate(II_pct=round(II/RD*100,2),
         CE_pct=round(CE/RD*100,2),
         P21_pct =round(P21/RD*100,2),
         P22_pct =round(P22/RD*100,2),
         pct_G=round(N_G/N*100,2),
         pct_P=round(N_P/N*100,2),
         Delta_NdV=round(Delta_NdV/RD*100,2))%>%
  select(II_pct,CE_pct,P21_pct,P22_pct,Delta_NdV,pct_G,pct_P)


fwrite(stat_compens_perdants,file="Resultats_perdants.csv")
fwrite(stat_compens_macro_perdants,file="Resultats_macro_perdants.csv")


tab_compens_macro_perdants<-data_compens%>%
  filter(Perte2021+Perte2022<0)%>%
  summarise(N=round(sum(pondmen)/1000000,1),
            II=round(sum(pondmen*SUM_CHEQUE)/1000000000,2),
            CE=round(sum(pondmen*SUM_CHEQUE_NRJ)/1000000000,2))

budget<-(tab_compens_macro_perdants[1,2]+tab_compens_macro_perdants[1,3])
shr_budget<-round((budget)/(montant_cheque+montant_cheque_NRJ),2)


shr_perdants<-(data_compens%>%
  filter(Perte2021+Perte2022<0)%>%
  summarise(N=sum(pondmen)))[1,1]/N_men

