library(readxl)
library(openxlsx)

prep_mtt<-function(dados, #matriz contendo dados
                   branco = c(), #vetor(linha_inicio, col_inicio)
                   reps = 4, #número de repetições
                   trat,#vetor(linha_inicio, col_inicio, col_fim), p/ cada trat
                   ctrl_pos = c(),ctrl_neg = c(),#vetor(linha_inicio,col_inicio)
                   con_max,#concentração máxima dos tratamentos
                   dilut = 2 #fator da diluição seriada
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
  n_dilut_ori<-trat[3]-trat[2]+1
  if((n_dilut_ori %% 2) != 0){
    n_dilut<-n_dilut_ori+1
  }else{
    n_dilut<-n_dilut_ori
  }
  for (i in 1:((n_dilut-length(con_max))/length(con_max))){
    con<-con/dilut
    concs<-c(concs,con)
    if(i == ((n_dilut-length(con_max))/length(con_max)) &
       (n_dilut_ori%%2) != 0){
      concs<-concs[1:(length(concs)-1)]
    }
  }
  if(class(ctrl_neg) == "numeric"){
    con<-concs[length(concs)]/100
    concs<-c(concs,con)
  }
  if(class(ctrl_pos) == "numeric"){
    con<-con_max[1]*100
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

setwd("C:/Users/teodo/Desktop/Lab/MTT Triagem/")

data<-as.data.frame(read_excel("duas_placas.xlsx"))
data<-data[,-1]
data<-as.matrix(data)
colnames(data)=NULL

prep_total<-prep_mtt(data,branco = c(5,2),
                     trat = c(1,1,6),ctrl_neg = c(5,1),
                     con_max = c(100))

write.xlsx(prep_total, file = "teste.xlsx")


