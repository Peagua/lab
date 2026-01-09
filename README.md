# Instruções para uso das funções



## Função prep\_mtt:

Primeiro, deve-se carregar o arquivo excel contendo os resultados
A deve se preparar os dados segundo o script abaixo
A Função recebe:

* os dados;
* coordenadas do primeiro poço branco;
* quantidade de repetições;
* coordenadas do primeiro poço tratamento e até onde vai o tratamento;
* se tiver mais de um tratamento, inserir as coordenadas p/ outros trats;
* coordenadas do primeiro poço ctrl positivo (se houver);
* coordenadas do primeiro poço ctrl negativo (se houver);
* concentração máxima de tratamento;
* o tipo de diluição seriada (padrão é 2, para diluição 1:2).



A função retorna uma matriz contendo os dados preparados pra jogar no prisma
Deve salvar o arquivo no formato excel, de preferência
O arquivo então deve ser aberto, e selecionar a partir da segunda linha
Primeira linha fica como V1, V2...)



## Função get\_stats:

Recebe:

* a tabela de dados (formato de matriz);
* n de tratamentos;
* n de repetições;
* se deseja normalizar ou não (T ou F, F é o standard);
* se existe controle positivo (F é std);
* se existe controle negativo (F é std).



A função retorna um dataframe contendo as concentrações (geral), média, desvio padrão e erro padrão
para concentração de cada tratamento.
O script da função get\_stats ainda tem uma parte do script que gera um gráfico de colunas com os resultados
retornados pela função.



## Script Modelo\_geral\_DRC:



Script para gerar modelo DRC (4 parâmetros, drc, loglogistic) a partir de matriz no formato retornado pela

função prep\_mtt.

Tem tudo bem explicado de como o script funciona ao longo dele.

Ele também faz o plot do modelo drc com os pontos de cada tratamento.

Além disso, tem algumas funções para validar modelo, pegar intervalos de confiança, etc...

