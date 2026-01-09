library(readxl)
library(openxlsx)
library(tidyverse)
library(drc)
library(broom)

set_theme(theme_bw())

prep_total<-read.xlsx("C:/Users/teodo/Desktop/M199_abril.xlsx")
tratamentos<-1
reps<-4

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


library(dr4pl)

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
