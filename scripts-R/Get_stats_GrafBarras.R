#Estatística descritiva, normalização e histograma (gráfico de barras)

library(readxl)
library(ggplot2)
library(tidyverse)

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
  labs(title = "Viabilidade Celular por Concentração de Tratamento")->graf

print(graf)

#Salvar o gráfico em jpg
ggsave("Graf_Barras.jpg",
       plot = graf,
       width = 7, height = 5, dpi = 300)
