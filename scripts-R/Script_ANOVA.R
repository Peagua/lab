# Anova MTT

library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(drc)
library(broom)
library(dr4pl)
library(multcomp)


#Função pra padronizar os dados pro MTT



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




# Script

setwd("C:/Users/teodo/Desktop/Lab/MTT-TCC/29-01/")

data<-as.data.frame(read_excel("placas-29-01.xlsx"))
data<-data[,-1]
data<-as.matrix(data)
colnames(data)=NULL

prep_total<-prep_mtt(data,branco = c(5,3),
                     trat = c(5,13,24), ctrl_neg = c(1,12),
                     con_max = c(200,146.65))

statis<-get_stats(dados = prep_total,
                  ntrats = 1,
                  reps = 4,ctrl_neg = T,normal = T)



# Fazendo a ANOVA e teste de Dunnett

# Criando o data frame, e nome das colunas
df_dados<-as.data.frame(prep_total)
nomes_colunas<-c("Grupos", "R1","R2","R3","R4")
colnames(df_dados)<-nomes_colunas
df_dados[,1]<-as.character(df_dados[,1])
df_dados[1,1]<-"Controle"

fatores<-df_dados[,1]


options(pillar.sigfig = 4) # Para mostrar os significativos

# Alongando o DF para criar a coluna apenas com as respostas
df_dados %>% 
  pivot_longer(cols = -1,
    names_to = "Repetições",values_to = "Resposta"
  ) -> df_longo

# Transformando a coluna dos grupos (Controle e concentrações) em fator
# Necessário para o Dunnett -> Controle tem que ser o maior fator
df_longo$Grupos<-factor(df_longo$Grupos,levels = fatores)


# Anova
anova_results<-aov(data = df_longo, Resposta~Grupos)
summary(anova_results)
# plot(anova_results) # para ver resíduos

# Teste de dunnett
teste_dunnett<- glht(
  anova_results,
  linfct = mcp(Grupos = "Dunnett")
)
summary(teste_dunnett)

# Extraindo comparações e p values do Dunnett
res_dunnett<-summary(teste_dunnett)
nomes_contrastes<-names(res_dunnett$test$coefficients)
pvals <- res_dunnett$test$pvalues

# Montando um df contendo as infos supracitadas, com coluna de concentrações
# (retirando o  " - Controle", para ficar igual ao statis)
signif_tbl<- data.frame(
  Concentrações = gsub(" - Controle","",nomes_contrastes),
  pvalue = pvals
)

# Adicionando os asteríscos de significância nos intervalos abaixo
signif_tbl$asterisco<-cut(
  signif_tbl$pvalue,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

# Pra ficar igual a statis
signif_tbl[,1]<-as.numeric(signif_tbl[,1])
signif_tbl[,1]<-sprintf("%.3f",signif_tbl[,1])
signif_tbl[,1]<-factor(signif_tbl[,1],levels = signif_tbl[,1])

# Juntando os asteriscos, p value e statis, p/ montar o gráfico
statis<-left_join(statis, signif_tbl, by = "Concentrações")


# Gráfico de Barras
statis %>% 
  ggplot(aes(x = Concentrações,
             y = Mean_T1))+
  geom_col(colour = "black",fill = "white")+
  theme_bw(base_size = 13, base_line_size = 0.8)+
  geom_errorbar(aes(ymin = Mean_T1 - SD_T1,
                    ymax = Mean_T1 + SD_T1, width = 0.2))+
  geom_text(
    aes(label = asterisco,
        y = Mean_T1 + SD_T1 + 3),
    size = 5
  )+
  annotate("text",
    x = 11, y = 103,
    label = paste0("p < 0.001 (***) , < 0.01 (**) , < 0.05 (*)"),size = 4
  )+
  annotate(
    "text",
    x = 12.5, y = 92,
    label = paste0("p-ANOVA = ***"),size = 4
  )+
  ylab("Viabilidade Celular (%)")+
  xlab("Concentração (μΜ)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.grid = element_blank())+
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,(max(statis$Mean_T1+statis$SD_T1))+5))+
  labs(title = "Viabilidade Celular por Concentração de DDS17")->graf
print(graf)
