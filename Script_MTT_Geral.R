#Função pra padronizar os dados pro MTT


library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/teodo/Desktop")

data<-as.data.frame(read_excel("C:/Users/teodo/Downloads/plate_data.xlsx"))
data<-data[,-1]
data<-as.matrix(data)
colnames(data)=NULL


prep_mtt<-function(dados, 
                   branco = c(), 
                   reps = 4, 
                   trat,
                   ctrl_pos = c(),ctrl_neg = c(),
                   con_max,
                   dilut = 2
                   ){
  #Pegar a média do branco, subtrai do geral, se existir branco
  if(class(branco) == "numeric"){
    med_b<-mean(data[branco[1]:(branco[1]+reps-1),branco[2]])
    dados<-round(dados-med_b,3)
  }
  #Criar matriz com coordenadas dos tratamentos
  ntrats<-length(trat)/3
  tratamentos<-matrix(nrow = 3)
  for(i in 1:ntrats){
    if(i == 1){
      tratamentos[,i]<-trat[1:3]
    }else{
      tratamentos<-cbind(tratamentos,trat[(1+3*(i-1)):(3+3*(i-1))])
    }
  }
  #Faz outra matriz, com apenas os tratamentos
  for(i in 1:ntrats){
    if(i == 1){
      dados_new<-dados[tratamentos[1,i]:(tratamentos[1,i]+reps-1),
                       tratamentos[2,i]:tratamentos[3,i]]
    }else{
      dados_new<-rbind(dados_new,
                       dados[tratamentos[1,i]:(tratamentos[1,i]+reps-1),
                             tratamentos[2,i]:tratamentos[3,i]])
    }
  }
  #Adiciona colunas dos ctrls neg e pos (se presentes) na nova matriz
  ##Se houver mais tratamentos, adiciona os ctrls a todos os tratamentos
  if (class(ctrl_neg) == "numeric"){
    c_negs<-rep(dados[ctrl_neg[1]:(ctrl_neg[1]+reps-1),ctrl_neg[2]], ntrats)
    dados_new<-cbind(dados_new, c_negs[1:length(c_negs)])
  }
  if (class(ctrl_pos) == "numeric"){
    c_pos<-rep(dados[ctrl_pos[1]:(ctrl_pos[1]+reps-1),ctrl_pos[2]], ntrats)
    dados_new<-cbind(c_pos[1:length(c_pos)], dados_new)
  }
  #Adicionando vetor com concentrações
  concs<-c(con_max)
  con<-con_max
  for (i in 1:(trat[3]-trat[2])){
    con<-con/dilut
    concs<-c(concs,con)
  }
  if(class(ctrl_neg) == "numeric"){
    con<-con/100
    concs<-c(concs,con)
  }
  if(class(ctrl_pos) == "numeric"){
    con<-con_max*100
    concs<-c(con,concs)
  }
  concs<-round(concs,3)
  #Adicionando linha com as concentrações
  dados_new<-rbind(concs[1:length(concs)],dados_new)
  #Inverte a matriz quanto às colunas
  dados_new<-dados_new[,ncol(dados_new):1]
  #Transpõe os dados
  dados_transp<-t(dados_new)
  return(dados_transp)
}

#Testando a função
prep_total<-prep_mtt(data,branco = c(1,12),
                     trat = c(1,1,10,5,1,10),ctrl_neg = c(1,11),
                     con_max = 50)



#Função para gerar estatísticas dos dados da função acima (normalizada ou não)



setwd("C:/Users/teodo/Desktop")
dados<-read_xlsx("M199_abril.xlsx")
dados<-as.matrix(dados)
colnames(dados) = NULL

get_stats<-function(dados,
                    ntrats = 1,
                    reps = 4,
                    normal = F,
                    ctrl_pos = F,
                    ctrl_neg = F){
  mat_stat<-matrix(nrow = dim(dados)[1], ncol = 1+(3*ntrats))
  mat_stat[,1]<-dados[,1]
  nom_col<-c(paste0("Concentrações"))
  if(normal == F){
    for (i in 1:ntrats){
      media<-round(apply(dados[,(2+reps*(i-1)):(5+reps*(i-1))],1,FUN = mean),3)
      sd<-round(apply(dados[,(2+reps*(i-1)):(5+reps*(i-1))],1,FUN = sd),3)
      sem<-round(sd/reps,3)
      mat_stat[,(2+reps*(i-1))]<-media
      mat_stat[,(3+reps*(i-1))]<-sd
      mat_stat[,(4+reps*(i-1))]<-sem
      nomes<-c(paste0( "Mean_T",i),paste0( "SD_T",i),paste0( "SEM_T",i))
    }
    nom_col<-c(nom_col,nomes)
    colnames(mat_stat)<-nom_col
  }else{
    dados_norm<-dados
    for(i in 1:ntrats){
      media<-round(apply(dados[,(2+reps*(i-1)):(5+reps*(i-1))],1,FUN = mean),3)
      if(ctrl_neg == T){
        tops<-media[1]
      }else{
        tops<-max(media)
      }
      result<-100*dados_norm[,(2+reps*(i-1)):(1+reps+(reps*(i-1)))]/tops
      dados_norm[,(2+reps*(i-1)):(1+reps+(reps*(i-1)))]<-result
      media<-round(apply(dados_norm[,(2+reps*(i-1)):(5+reps*(i-1))],1,
                         FUN = mean),3)
      sd<-round(apply(dados_norm[,(2+reps*(i-1)):(5+reps*(i-1))],1,
                      FUN = sd),3)
      sem<-round(sd/reps,3)
      mat_stat[,(2+3*(i-1))]<-media
      mat_stat[,(3+3*(i-1))]<-sd
      mat_stat[,(4+3*(i-1))]<-sem
      nomes<-c(paste0( "Mean_T",i),paste0( "SD_T",i),paste0( "SEM_T",i))
      nom_col<-c(nom_col,nomes)
    }
    colnames(mat_stat)<-nom_col
  }
  df_stat<-as.data.frame(mat_stat)
  df_stat[,1]<-sprintf("%.3f",df_stat[,1])
  if(ctrl_neg == T){
    if(ctrl_pos == F){
      df_stat[1,1]<-"Controle"
    }else{
      df_stat[1,1]<-"Ctrl Negativo"
    }
  }
  if(ctrl_pos == T){
    df_stat[dim(df_stat)[1],1]<-"Ctrl Positivo"
  }
  df_stat$Concentrações<-factor(df_stat$Concentrações,
                                levels = df_stat$Concentrações)
  return(df_stat)
}


statis<-get_stats(dados = dados,
                  ntrats = 1,
                  reps = 4,
                  ctrl_neg = T,normal = T)

colnames(statis)
class(statis$Concentrações)



#Script para criar gráfico dos dados acima, utilizando ggplot



statis %>% 
  ggplot(aes(x = Concentrações,
             y = Mean_T1))+
  geom_col(colour = "black",fill = "white")+
  theme_bw(base_size = 13, base_line_size = 0.8)+
  geom_errorbar(aes(ymin = Mean_T1 - SD_T1,
                    ymax = Mean_T1 + SD_T1, width = 0.2))+
  ylab("Viabilidade Celular (%)")+
  xlab("Cooncentração (μΜ)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.grid = element_blank())+
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,(max(statis$Mean_T1+statis$SD_T1))+5))+
  labs(title = "Viabilidade Celular por Concentração de Tratamento")


