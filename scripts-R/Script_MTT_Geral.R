#Arquivo contendo todos os Script relacionados ao TCC até agora, como modelo


library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(drc)
library(broom)
library(dr4pl)



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


#Função para gerar estatísticas dos dados da função acima (normalizada ou não)

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

# Aplicando as funções e scripts

setwd("C:/Users/teodo/Desktop/Lab/MTT TCC/22-01/")

data<-as.data.frame(read_excel("MTT_22-01.xlsx"))
data<-data[,-1]
data<-as.matrix(data)
colnames(data)=NULL

tratamentos<-2
reps<-4
loc_trats<-c(1,1,10,5,1,10)
loc_branco<-c(5,12)
loc_ctrl_neg<-c(1,12)
concentr_max<-c(50)


prep_total<-prep_mtt(data,branco = loc_branco,
                     trat = loc_trats,ctrl_neg = loc_ctrl_neg,
                     con_max = concentr_max)


statis<-get_stats(dados = prep_total,
                  ntrats = tratamentos,
                  reps = reps,
                  ctrl_neg = T,normal = T)

colnames(statis)
class(statis$Concentrações)



#Script para criar gráfico dos dados acima, utilizando ggplot



statis %>% 
  ggplot(aes(x = Concentrações,
             y = Mean_T1))+ # Muda para + tratamentos
  geom_col(colour = "black",fill = "white")+
  theme_bw(base_size = 13, base_line_size = 0.8)+
  geom_errorbar(aes(ymin = Mean_T1 - SD_T1, # Muda para + tratamentos
                    ymax = Mean_T1 + SD_T1, width = 0.2))+ # Muda para + trats
  ylab("Viabilidade Celular (%)")+
  xlab("Cooncentração (μΜ)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.grid = element_blank())+
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,(max(statis$Mean_T1+statis$SD_T1))+5))+ # Muda para + trats
  labs(title = "Viabilidade Celular por Concentração de Tratamento")->graf
#               Muda o título do graf tb p/ + trats
print(graf)



#Script para criar modelo DRC (dose-response curve, loglogit4-parameter)



#Transforma a matriz de dados em df e nomeia as colunas, depois vira tibble
nomes_col<-c("dose")
for(i in 1:tratamentos){
  for(j in 1:reps){
    nomes_col<-c(nomes_col, paste0("T",i,"_R",j))
  }
}

prep_total<-as.data.frame(prep_total)
colnames(prep_total)<-nomes_col

old<-options(pillar.sigfig = 4)
prep_total<-tibble(prep_total)


#Cria um novo tibble, alongando e organizando o anterior (tidy)
#Contém as colunas, dose, amostra, resposta, condição (tratamento) e a replicata
prep_total %>% 
  pivot_longer(
    cols = -dose,
    names_to = "sample",
    values_to = "response"
  ) %>% 
  mutate(
    condition = str_sub(sample,1,
                        (str_locate(string = sample, pattern = "_")[,1]-1)),
    replicate = as.numeric(str_sub(sample,
                                   start = (str_locate(string = sample,
                                                       pattern = "R")[,1]+1),
                                   end = str_length(sample)))
  ) -> prep_tidy

#Apenas uma visualização do tibble tidy, com escala logaritmica
prep_tidy %>% 
  ggplot(aes(x = dose, y = response, group=sample, colour = condition))+
  geom_line()+
  geom_point()+
  scale_x_log10()

#Cria uma variável contendo o tratamento e a resposta máxima média  
prep_tidy %>%
  group_by(dose,condition) %>% 
  summarise(
    response = mean(response)
  ) %>% 
  ungroup() %>% 
  slice(1:tratamentos) %>%
  dplyr::select(-dose) %>% 
  rename(max_response = response) -> max_response

#Cria um tibble normalizando as respostas com base na variável mas_response   
prep_tidy %>% 
  left_join(max_response) %>% 
  mutate(
    norm_response = 100*response/max_response
  ) %>% 
  dplyr::select(dose,sample,condition,replicate,norm_response) -> prep_norm

#Removendo dados que não fazem sentido na análise (resposta negativa)
for(i in 1:nrow(prep_norm)){
  if(prep_norm$norm_response[i]<0){
    prep_norm$norm_response[i]<-NA
  }
}
prep_norm<-na.omit(prep_norm)

#Visualizando a resposta normalizada
prep_norm %>% 
  ggplot(aes(x = dose, y = norm_response, group=sample, colour = condition))+
  geom_line()+
  geom_point()+
  scale_x_log10()

#Faz a média das respostas normalizadas de acordo com as repetições a cada dose
#Adiciona a coluna do desvio padrão
prep_norm %>% 
  group_by(dose, condition) %>% 
  summarise(
    sd = sd(norm_response),
    response = mean(norm_response)
  ) -> prep_mean_norm

#Plotando as médias normalizadas por dose
prep_mean_norm %>% 
  ggplot(aes(x = dose, y = response, colour = condition))+
  geom_line(linewidth = 0.8)+
  geom_point(size = 2)+
  scale_x_log10()+
  geom_errorbar(aes(ymin = response - sd,
                    ymax = response + sd,
                    width = 0.15),linewidth = 0.8)

#Cria o modelo de dose-response (para obter o IC50)
drm(
  data = prep_norm,
  formula = norm_response ~ dose,
  curveid = condition,
  fct = LL.4(names = c("Hill Slope","Min","Max", "IC50"))
) -> modelo
print(tidy(modelo),n=24)
print(summary(modelo),n=24)

# Comparação rápida entre o IC50 os tratamentos no modelo
# Observa-se que apenas os tratamentos 3,5 e 6 tem significância entre si
# Só funciona se tiver o que comparar (mais de uma condição/tratamento)
if (tratamentos > 1){
  compParm(modelo, "IC50", "-")  
}

#Cria um tibble contendo valoes de dose:
# do mínimo ao máximo de doses testadas
# a cada 0.1
predict_data<-tibble(
  dose = seq(min(prep_mean_norm$dose),max(prep_mean_norm$dose)+0.1, by = 0.1)
)
predict_data<-tibble(dose = rep(predict_data$dose, times= tratamentos))

name_trats<-unique(prep_mean_norm$condition)

predict_data %>% 
  mutate(
    condition = rep(name_trats, each=(nrow(predict_data)/tratamentos))
  )->predict_data

#Cria uma coluna do último tibble criado (agora df) com as predições do modelo
#Para cada dose
predict_data<-as.data.frame(predict_data)
predict_data$prediction<-predict(modelo,newdata = predict_data)  

#Finalmente, cria o gráfico plotando: 
# A curva gerada pelo modelo
# Os pontos da média normalizada para cada dose testada, com desvio padrão
prep_mean_norm %>% 
  ggplot(aes(x = dose, 
             y = response, 
             colour = condition))+
  geom_point(size = 2)+
  scale_x_log10()+
  geom_line(data = predict_data,
            aes(x = dose, y = prediction),
            linewidth = 0.8)+
  geom_errorbar(aes(ymin = response - sd,
                    ymax = response + sd,
                    width = 0.15),linewidth = 0.8)


#----------------------#
#--Validando o modelo--#
#----------------------#


#Sumários do modelo
summary(modelo)
tidy(modelo)

#Lack-of-fit test do modelo
# H0: Não há falta de ajuste além do erro experimental
# ou
# H0: O modelo ajusta adequadamente os dados
# Se p > 0.05, o IC50 estimado é estatisticamente defensível
modelFit(modelo)

#Detecta os resíduos outliers (de acordo com o modelo)
outliers<-OutlierDetection(residuals(modelo))
resid(modelo)[outliers]

#Basicamente mostra alguns testes do modelo
#loglikelihood - AIC - p-value do Lack of fit - variância dos resíduos
mselect(modelo)

#Teste para ver se existe significância no efeito de dose
#Se p < 0.0001, existe um efeito de dose altamente significante
noEffect(modelo)

#Intervalo de confiança dos parâmetros (95% é o padrão)
confint(modelo)
