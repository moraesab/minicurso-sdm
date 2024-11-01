# -------------------------------------------------------------------------
# Minicurso Introdução a Modelagem de Nicho Ecológico - XII CBC
#
# Objetivo: instalar pacotes necessários
#
# Autor: Alex Barbosa de Moraes
#
# Última atualização: 2024-10-29
#
# Descrição: Executa os passos principais para construção de modelos de nicho ecológico (MNE): 
# (I) Preparação dos dados, (II) Ajuste de modelo, (III) Avaliação do 
# modelo, (IV) predição e (V) visualização. Este script tem o propósito educacional de guiar 
# os primeiros passos na modelagem. Por consequencia, deve-se perceber que para a construção 
# de modelos reais, você deverá revisitar e melhorar certos passos desses script, como por 
# exemplo melhorar o design da amostragem espacial. É importante frisar também que no R 
# existem muitas maneiras diferentes de se alcançar os objetivos pretendidos. O script 
# apresentado aqui é uma maneira rápida e amigável ao usuário (user-friendly) que serve aos 
# propósitos educacionais mas existem scripts muitos mais complexos que aumentam o nível de 
# refinamento do resultado final.
# -------------------------------------------------------------------------

# Start -------

# Limpa a memoria do R

rm(list = ls())

# Antes de iniciar o projeto, é importante que seja criada toda a árvore de diretórios onde iremos armazenar os dados gerados pelo nosso projeto. Nesse curso, iremos criar apenas uma pasta que será destinada a manter os arquivos de nossas variáveis preditoras. 

# Create directory tree ---------------------------------------------------

# Pasta para colocar os rasters das variáveis ambientais

dir.create("data") # Cria a pasta para armazenar os dados
dir.create("variaveis") # Cria a pasta para armazenar as variáveis preditoras
dir.create("output") # Cria a pasta para armazenar nossos resultados

# No R, existem diversos modos de se atingir um mesmo objetivo. Dependendo dos métodos utilizados para fazer cada etapa do processo de modelagem, diferentes pacotes podem ser necessários e, portanto, deverão ser instalados em sua máquina.
# A melhor forma de nos asseguramos de que todos os pacotes necessários para rodar um projeto estejam instalados e prontos para serem usados é adicionando um condicional no início do projeto informando todos os pacotes que são necessários e solicitando a instalação automática de qualquer pacote ausente.

source(file = "R/00_instalar_pacotes.R")

# Depois de garantir que todos os pacotes estejam instalados, vamos carregar todos os pacotes que serão utilizados nesse script.

# Packages --------------------------------------------------------------

library(tidyverse)
#library(biooracler) # Instalação via github. Ver 00_instal_packages_folder_tree.R
library(sdmpredictors) # Variáveis do Bio-Oracle e Marspec
library(maptools)
library(spocc)
library(raster) # Funções em raster
library(sdm) # funções para ajustar MNE


# 1. Registros de ocorrência ----------------------------------------------



# Baixar aluns registros de ocorrência do gbif usando o pacote spocc

sp <- "Pleoticus muelleri" # espécie que iremos buscar


# spocc - pode demorar um tempo... (2min)
occ_spocc <- spocc::occ(query = sp, # um ou vários nomes científicos
                        from = "gbif", # c("gbif", "inat", "vertnet", "idigbio", "ecoengine")
                        has_coords = TRUE, # Apenas registros com coordenadas
                        limit = 10000) # número máximo de registros
occ_spocc

# get data
occ_spocc_data <- spocc::occ2df(occ_spocc) %>%
  dplyr::mutate(species = sp,
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::strstr_to_lower()) %>%
  dplyr::select(name, species, longitude, latitude, year, base)

occ_spocc_data

# Salva a tabela para uso posterior
readr::write_csv(occ_spocc_data, "data/occ.csv")



# 1.1 Carregar dados já existentes ----------------------------------------

occ_spocc_data <- read.csv(file = "data/occ.csv") # Podemos também importar um conjunto de dados já existente.
# ou 
occ_spocc_data <- readr::read_csv(file = "data/occ.csv") # Podemos também importar um conjunto de dados já existente.



#2. Carregar e selecionar dados ambientais --------------------------------



# 2.1 Carregar dataset ----------------------------------------------------


# Do sdmpredictors 

datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)



# 2.2 Inspecionar os conjuntos de dados e camadas marinhas disponí --------


# View(datasets[,c("dataset_code", "description")]) # Visualizar os conjuntos de dados disoníveis

layers <- list_layers(datasets) # Listar as camadas disponíveis dos conjuntos de dados selecionados.

# View(layers[1:2,c("dataset_code", "name", "description",
#                  "primary_type")]) # Visualizar as camadas - primeiras duas linhas




# 2.3 Selecionar camadas ambientais ---------------------------------------


# Download raster com áreas iguais
list(layers$layer_code) # Visualizar o código das camadas para colocar na função de baixo

layercodes <- c("MS_bathy_5m", "MS_biogeo06_bathy_slope_5m",
                "BO22_phosphatemean_bdmax",
                "BO22_dissoxmean_bdmax",
                "BO22_ph",
                "BO22_salinitymean_bdmax",
                "BO22_carbonphytomean_bdmax",
                "BO22_lightbotmean_bdmax",
                "BO22_tempmean_bdmax","BO22_tempmin_bdmax") # Seleciona as camadas desejadas para fazer o download


predictorsR <- load_layers(layercodes, equalarea = FALSE, rasterstack = FALSE, datadir = "variaveis") #down(load) dos rasters selecionados. Por padrão, a função está baixando algumas camadas como arquivos ZIP. Não é possível fazer o stack de arquivos zip, então, eu defini o argumento para que ele não faça o stack. Desse forma, cada camada será carregada separadamente num objeto do tipo lista. Eu uso a linha de baixo para juntar todas em um objeto de camadas empilhadas (stack). (tempo: 3 min)

# Se o download falhar, rodar a linha anterior novamente, até que seja feito o download de todas as camadas e tudo seja carregado corretamente. Essa falha poderá acontecer várias vezes durante o processo.

predictorsR <- raster::stack(predictorsR)


# 2.4 Carregar raster do PC -----------------------------------------------

# predictorsR <- raster::stack( c("variaveis/MS_bathy_5m.tif") ) # carrega manualmente camadas selecionadas. Elas não podem está em arquivo zip.

# OU

# Lista todos os raster em um determinado diretório e carrega todos de uma vez só em um único objeto com camadas empilhadas


# predictorsR <- paste0("variaveis/", dir(path ="variaveis", pattern = ".tif$") ) %>% 
#  raster::stack()



# 2.5 Ajustar nome das variáveis ------------------------------------------


# Visualiza os nomes atuais e a ordem em que as camadas estão.

names(predictorsR) 

#Altera os nomes das variaveis para facilitar a manipulacao
names(predictorsR) <- c("bathymetry", "bathy_slope_5m",
                        "phosphatemean","dissoxmean",
                        "ph",
                        "salinitymean",
                        "carbonphytomean", 
                        "lightbotmean",
                        "tempmean_b","tempmin_b")


# 2.6 Basic checks --------------------------------------------------------

# Se rodar o nome do objeto que contém as camadas, podemos ver informações gerais sobre as camadas como a resolução, a extensão, os valores mínimos e máximos. Usando a função "plot" podemos visualizar todas as camadas que estão empilhadas, ou então podemos definir apenas algumas camadas de interesse para visualizar.

predictorsR #check extent, resolution and projection

# A resolução do raster se refere ao tamanho das células. Em outras palavras, quanta superfície da Terra essa célula está cobrindo. Aqui, a resolução é dada em graus decimais.

# Visualizar todas as camadas

plot(predictorsR)

# Visualizar apenas algumas camadas

plot(predictorsR[[1:3]])


# 2.7 Recortar ------------------------------------------------------------

# As camadas importadas estão a nível mundial. Trabalhar com elas assim, além de ser extremamente custoso computacionalmente, é também inadequado para a modelagem. Por isso, precisamos resringir as camadas bioclimáticas apenas para a nossa área de estudo. 

# Nem sempre sabemos exatamente a latitude e longitude da nossa área de interesse, por isso, vamos plotar primeiro um mapa mundial simples disponível no pacote maptools e usar a função "drawExtent" do pacote raster para desenhar os limites da nossa área de interesse e obter os valores de latitude e longitude. A função pede que se selecione dois pontos: superior esquerdo e inferior direito. A partir daí ele vai criar um quadrado e informar os valores no console. Depois, armazenamos as informações de lat e lon informadas e as usamos na função "crop" para cortar todas as nossas camadas climáticas de uma só vez.

# Plotar o mapa simplificado do mundo para facilitar a seleção da nossa área de estudo.

data(wrld_simpl) # Carrega um mapa mundial do pacote Maptools


# Plota o mapa do mundo. Os argumentos "xlim" e "ylim" definem os limites do nosso mapa, restringindo a visualização.
plot(wrld_simpl, axes=TRUE,col="light yellow") # mapa mundi
plot(wrld_simpl, xlim=c(-100,-20), ylim=c(-55,5), axes=TRUE,col="light yellow") # plota o mapa com lat e long definida (drawExtent abaixo) 

raster::drawExtent() # Permite selecionar no mapa a área que a gente quer trabalhar. Clicar no topo esquerdo e base direita. Ao selecionar a área, pegar as coordenadas e inserir na função de corte. 

ext <- c(-40,-69,-55,-14) # insere aqui a área de corte. Argumentos: xmax, xmin, ymax, ymin.
preditors_crop <- raster::crop(predictorsR, ext) # Faz o recorte.

plot(preditors_crop[[4]]) # Visualiza o mapa cortado na área selecionada.

# Nota: A função crop faz sempre um recorte quadrado. para fazer recortes mais complexos, é necessário criar ou usar polígonos e usar a função "mask".


# 2.8 Padronizar preditores -----------------------------------------------


# Os preditores possuem unidades diferentes, portanto, também possuem dimensões diferentes. Para que a variância deles seja comparável, efetuaremos uma padronização nas variáveis para que todos fiquem com média zero.

#round(apply(raster::values(preditors_crop), 2, summary), 4) #visualiza a variancia dos preditores
predictors_stand <- raster::scale (preditors_crop) # Faz a padronização
round(apply(raster::values(predictors_stand), 2, summary), 4) #confere se deu tudo certo



# 2.9 Calcular correlação -------------------------------------------------


# Variáveis com alta correlação e/ou multicolinaridade entre si podem influenciar negativamente nossos modelos, por isso é necessário retirá-las. Para isso, vamos avaliar nossas variáveis através de uma matriz de correlação e visualizar graficamente os grupos correlacionados para facilitar nossa tomada de decisão sobre quais variáveis deverão sair e quais deverão permanecer para a construção dos nossos modelos. Devemos utilizar nosso conhecimento biológico para melhor decidir que variáveis devem permanecer.

cors <- ENMTools::raster.cor.matrix(predictors_stand) # Faz a correlação entre as variáveis
# cors <- ENMTools::raster.cor.matrix(env)

threshold = 0.7  ## Define o máximo de correlação permitida

dists <- as.dist(1 - abs(cors))
clust <- hclust(dists)
groups <- cutree(clust, h = 1 - threshold)

# Visualiza os grupos de correlação: variáveis abaixo da linha vermelha e dentro do mesmo agrupamento apresentam alta correlação entre si.

plot(clust)
abline(h = 1 - threshold, col = 'red', lwd = 2)

env <- raster::dropLayer(predictors_stand, c("phosphatemean","tempmin_b", "bathymetry", "carbonphytomean" )) # Exclui variáveis da nossa pilha de camadas ambientais. c("phosphatemean","tempmin_b", "bathymetry", "carbonphytomean")


# 2.10 Global VIF test ----------------------------------------------------

# Fator de Inflacao da Variavel - Teste de colinearidade (multivariado)

usdm::vif(predictors_stand)
usdm::vif(env)


# Salva os rasters para uso posterior

dir.create("variaveis/final")
raster::writeRaster(x = env, 
                    filename = paste0("variaveis/final/varbioclim_", names(env)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)



# 3. Triagem dos registros de ocorrência ----------------------------------


# Com as camadas ambientais selecionadas, nosso próximo passo é carregar e organizar nossos dados bióticos. Esse conjunto de dados compreende a tabela com o local de ocorrência da espécie que queremos modelar. Ao importar um conjunto de dados, precisamos primeiro ter certeza que o R importou todas as informações corretamente e depois fazer uma série de checagens e limpeza nos dados antes que o conjunto esteja pronto para ser analisado. As limpezas são necessárias para garantir que o conjunto de dados não tenha inconsistência nos pontos de ocorrência que possam influenciar negativamente e/ou enviesar nossos resultados, garantindo modelos o mais próximos da realidade possível. Diversos procedimentos podem ser feitos nessa etapa e uma série de filtros poderão ser empregados. Para fins de simplificação, faremos apenas algumas ações simples.

# Usaremos o conjunto de dados de ocorrência obtido anteriormente através do Global Biodiversity Information Facility (Gbif), um sistema global de informação sobre a biodiversidade que agrega dados de ocorrência provenientes de diversas fontes diferentes. Uma vez que os dados são oriundos de diversas fontes, nem todas as ocorrências são confiáveis e muitas deles podem está duplicadas no banco de dados, por isso é imprescindível tratar com cautela todo o conjunto de dados. Aqui, Efetuaremos uma limpeza seguindo alguns critérios e posteriormente iremos visualizar nossos dados de uma forma interativa que permita a identificação de pontos que pareçam inconsistentes.


# 3.1. Checagens básicas --------------------------------------------------

nrow(occ_spocc_data)             # Qunatas linhas
str(occ_spocc_data)              # Classe das variáveis
attributes(occ_spocc_data)       # Atributos
head(occ_spocc_data)             # Primeiras linhas
any(duplicated(occ_spocc_data))  # Existe alguma linha duplicada? #Any chama uma variável lógica. Se tirar, ele vai mostrar todas as linhas que estão duplicadas.

# Renomear variáveis

occ_data <- dplyr::rename(occ_spocc_data, 
                    lat  = latitude,
                    lon  = longitude
) # Vamos renomear para facilitar nosso uso dessas variáveis posteriormente. Na função "rename", o nome da esquerda é o novo nome que queremos dar para a variável. O nome da direita é o original

# Faz uma visualização simplificada dos nossos dados usando o plot básico do R.

plot(wrld_simpl, 
#     xlim=c(-60,-20), 
#     ylim=c(-55,-5), 
     axes=TRUE,col="light yellow")

points(occ_data$lon, occ_data$lat, 
       col="orange", pch=20, cex=0.75) # Adiciona os pontos de ocorrência no mapa.
points(occ_data$lon, occ_data$lat, 
       col="red", cex=0.75)



# 3.2. Limpeza de dados ---------------------------------------------------


# 3.2.1. Aplica uma série de filtros as nossos conjunto de dados ----------


occ_data_clean <- occ_data %>%
  group_by(species) %>% # Define como os dados devem ser agrupados para limpeza. Útil quando estamos aplicando o filtro para mais de uma espécie de uma só vez.
  arrange(species, desc(year), lat) %>%  # reorganiza o conjunto de dados para que registros do mesmo lugar e duplicados fiquem juntos
  distinct(lat, lon, .keep_all = TRUE) %>%  # remove linhas duplicadas baseado no lat e lon. A função mantém só o primeiro registro. Nesse caso, manteremos apenas o mais recente uma vez que os dados já foram ordenados anteriormente para que o primeiro registro sempre seja o mais recente.
  filter(year > 2000) %>%  #retira todos os registros mais antigos que os anos 2000, para equiparar com as datas dos layers criados no Bio-oracle 
  ungroup()

# visualiza

plot(wrld_simpl, 
     xlim=c(-60,-20), 
     ylim=c(-55,-5), 
     axes=TRUE,col="light yellow")

points(occ_data_clean$lon, occ_data_clean$lat, 
       col="orange", pch=20, cex=0.75) # Adiciona os pontos de ocorrência no mapa.
points(occ_data_clean$lon, occ_data_clean$lat, 
       col="red", cex=0.75)


# 3.2.2. Filtrar pontos apenas para a área de estudo (Opcional) -----------

occ_data_clean = occ_data_clean %>% filter(lon>=ext[2], lon<=ext[1], lat>=ext[3], lat <=ext[4]) #dplyr filter points to study area


# 3.2.3. Remover pontos fora do oceano (Opcional) -------------------------


coords <- occ_data_clean %>% dplyr::select(lon, lat) # Separa só as coordenadas
presvals <- raster::extract(env[[1]], coords) # Extrai os valores da variaveis
presvals <- as.data.frame(presvals)

rem <- bind_cols(occ_data_clean, presvals) #junta os valores ao nosso conjunto de dados

dados_ocean <- filter (rem, !is.na(presvals)) # filtra as linhas que retornaram valor numérico do nosso raster (registros em terra ou sem informação abiótica)

dados_earth <- filter (rem, is.na(presvals)) # filtra as linhas que não retornaram nenhum valor numérico do nosso raster (registros em terra ou sem informação abiótica)


# 3.2.4. Visualização interativa dos pontos -------------------------------

library(rbokeh)
figure(width = 800, height = 450, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_points(lon, lat, data = dados_ocean, size = 5,
            hover = c(species, lon,lat, year)) # Cria um mapa interativo que pode ser salvo em HMTL e aberto por qualquer navegador. No argumento "ly_points" podemos definir as variáveis queremos que sejam mostradas ao passar o mouse (hover) sobre um ponto. É útil para que possamos saber exatamente onde um ponto deveria estar, caso ele esteja caindo no lugar errado.


# 3.2.5. Limpeza espacial -------------------------------------------------

# Como temos muitos pontos ainda e muitos deles estão muito próximos uns dos outros. Faremos uma limpeza espacial.

# Objeto para armazenar os registros espacialmente limpos 

data_thin <- tibble::tibble()

# Cria um looping que faz a limpeza espacial por espécie, separadamente, e depois junta tudo novamente.

for(i in dados_ocean$species %>% unique){
  
  print(paste("Ajustando dados da espécie", i))
  
  sp <- filter(dados_ocean, species == i)
  
  dados_species <- spThin::thin( loc.data = sp, 
                                 lat.col = "lat", 
                                 long.col = "lon", 
                                 spec.col = "species", 
                                 thin.par = 20, # distancia em km entre os pontos
                                 reps = 1, 
                                 locs.thinned.list.return = TRUE, 
                                 write.files = FALSE
  )
  
  dados_thin_list <- dados_species[[1]]
  
  thin_rec <- dados_ocean %>% 
    dplyr::filter(species == i & lat %in% dados_thin_list$Latitude & lon %in% dados_thin_list$Longitude)
  
  # Combina as avaliacoes
  data_thin <- dplyr::bind_rows(data_thin, thin_rec)
  
}

figure(width = 800, height = 450, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_points(lon, lat, data = data_thin, size = 5,
            hover = c(species, lon,lat, year))


# Salva a tabela para uso posterior
readr::write_csv(data_thin, "data/occ_clean.csv")

# 4. Modelando ------------------------------------------------------------


# Com os dados biótico e abiótico organizamos, podemos partir para a construção dos nossos modelos. O ajuste do modelo é o coração de qualquer aplicação de modelagem. Diversos algoritmos estão disponíveis para serem utilizados e há uma infinidade de configurações que podem ser feitas. Podemos tanto escolher um único método e apresentar nosso melhor resultado dele, quanto podemos aplicar diversos métodos diferentes e combiná-los em modelos consensuais (ensemble). 

# A exploração do comportamento do modelo é estritamente parte da etapa de avaliação do modelo. Verificar a plausibilidade da relação espécie-ambiente ajustada pela inspeção visual das curvas de resposta e pela avaliação dos coeficientes do modelo e da importância das variáveis é parte do trabalho. Existem diferentes formas de aplicar e avaliar os resultados, no entanto, para simplificar, utilizaremos aqui apenas um método, usando o pacote SDM. Esse pacote facilita a nossa vida, transformando processos complexos em funções mais simples, assim como também traz um retorno visual dos resultados de forma amigável.



# 4.1. Preparação dos dados -----------------------------------------------


# O pacote SDM usa os dados de ocorrência de uma espécie como uma coluna binária de presença/ausência (0/1). Por isso, faremos algumas alterações no nosso conjunto de dados bióticos.

data <- data_thin %>% dplyr::select(lat,lon) # Vamos manter apenas a coluna de lat e lon dos pontos de ocorrência e excluir a coluna com o nome da espécie.

data <- data %>% mutate(p.muelleri = 1) # Criamos uma variável chamada "p.muelleri" e daremos o valor "1" para todas as linhas, indicando presença.

coordinates(data) <- c("lon", "lat") # Transforma o nosso data frame em um conjunto de pontos espaciais.



# Usaremos agora as funções do pacote SDM para preparar nossos dados. Usaremos a função "sdmData" para organizar nossos dados de uma forma que o pacote consiga compreender.

# Nesta função, podemos especificar o conjunto de dados de treino (pode ser pontos espaciais ou simplesmente um data.frame) e preditores (se disponíveis, como um objeto raster). Além disso, se houver um conjunto de dados independente disponível para ser usado para medir o desempenho do modelo (avaliação/validação), podemos fornecê-lo por meio do argumento do test. Uma fórmula também pode ser usada para especificar as variáveis de resposta e explicativas. Veja mais sobre a formula no arquivo de poio do pacote.

sdm_data <- sdmData(formula = p.muelleri~., 
                    train = data, 
                    predictors = env) # indicamos na formula o nome da coluna binária de presença/ausência, em seguida o nome do conjunto de dados de treino e por fim o objeto onde estão os nossos preditores.

sdm_data # Ao rodar esse objeto, podemos ver um resumo dos nossos dados. Podemos ver que nossos conjunto de dados contém apenas dados de presença.


# Para alguns algoritmos, é importante que tenhamos dados de ausência ou dados de background para melhor caracterizar ambientalmente a área de estudo. Quando não temos dados de ausências verdadeiras, podemos usar pseudo-ausências. Podemos gerar as pseudo-ausência incluindo o argumento "bg", indicando a quantidade de pontos que queremos e o método para a criação desses pontos (ver outros métodos no manual do pacote).

sdm_data <- sdmData(formula = p.muelleri~., 
                    train = data, 
                    predictors = env, 
                    bg=list(n=1000,method='gRandom',remove=TRUE) 
                    )
sdm_data


# Com os dados devidamente preparados, podemos finalmente fazer nossos modelos. Usaremos a função "sdm" para gerar os modelos. Nesse argumento, devemos inserir a fórmula do modelo (que deve ser igual ao colocado no objeto de preparação dos dados), em seguinda o objeto sdmdata criado anteriormente. No argumento 'methods' especificamos quais algoritmos queremos usar para modelar. Podemos especificar apenas um, como também podemos usar vários diferentes. Podemos usar a função "getmethodNames()" para ver todos os algoritmos disponíveis. O pacote é bastante flexível e também permite customizar algoritmos e incluir como um método novo para que seja utilizado.

# Caso só exista os 3 primeiros argumentos, a função irá calcular as estatísticas de performance usando o conjunto de dados de treino (o mesmo conjunto usado para ajustar o modelo). O ideal é que a gente tenha um conjunto de dados independente para testar o modelo (que pode ser indicado no argumento "test" do sdmData). Porém, na maioria dos casos não temos esses dados disponíveis, então, podemos dividir nosso conjunto de dados original em conjunto de teste e treino como uma solução alternativa. O particionamento pode ser feito apenas uma vez ou diversas vezes (muitas réplicas). Existem também diversos métodos que podem ser aplicados para fazer o particionamento como subamostragem, validação cruzada, bootstrap, etc.. Veja os métodos de particionamento no arquivo de poio do pacote.

# Nesse nosso projeto, vamos ajustar 2 modelos e avaliar eles em 2 rodadas de amostragem, em cada uma delas, particionando 30% do nosso conjunto de dados como dados de teste e 70% como treino.



# 4.2. Ajuste do modelo e avaliação ---------------------------------------


getmethodNames()

# Faz o modelo ou multiplos modelos. Os modelos são indicados no argumento methods. Pode ser colocado apenas 1 ou multiplos.

model <- sdm(p.muelleri~., 
             sdm_data, 
             methods=c(
               #"bioclim",
               #"bioclim.dismo",
               #"brt",
               #"domain.dismo",
               #"fda",
               #"gam",
               #"glm",
               #"glmnet",
               #"mahal.dismo",
               #"mars",
               "maxent",
               #"maxlike",
               #"mda",
               #"rpart",
               "rf"
               #"svm"
               ), 
             replication=c("sub", "boot"),
             test.percent=30, 
             n=2, 
             parallelSetting=list(ncore=4,mthod="parallel"))



# Executando o objeto do nosso modelo veremos um resumo de tudo o que foi feito.
model

getModelInfo(model) # info on runs including modelID, whether they are successfully fitted and evaluated, etc.

# We can generate the roc curve and compare the results for all models:
roc(model)

# Podemos gerar a curva de resposta das variáveis. Avaliar essas curvas de resposta é importante por que você consegue visualizar a resposta da espécie ao gradiente de cada variável. Podemos colocar o objeto do nosso modelo direto na função para avaliar a curva considerando todos os modelos gerados e todas as variáveis ou podemos especificar a ID de modelos específicos para avaliar as curvas apenas dos modelos selecionados.

rcurve(model)

rcurve(model, id=3:5)

# Uma outra função é a de avaliação da importância das variáveis para os modelos. Para isso usamos a função getVarImp para resgatar a importância das variáveis. Aqui, você também pode especificar a ID dos modelos que você quer. Se for especificado apenas 1 modelo, ele vai mostrar a importância das variáveis para aquele modelo. Se você especificar mais de um ou não especificar, a função vai mostrar a importância da variável com um intervalo de confiança.

getVarImp(model) # getVarImp(model, id=1)

# você pode colocar essa função dentro da função de plot para ver esses mesmos resultados mas como um gráfico.

plot(getVarImp(model)) # plot(getVarImp(model, id=1))

# É possível também incluir o argumento 'method' para obter a informação dos modelos construídos apenas por um algoritmo específico.

plot(getVarImp(model, method="maxent"))


# Por fim, podemos visualizar todos estes resultados e alguns outros de forma mais amigável através de uma interface gráfica, usando a função "gui" 

gui(model) # visualizar todos os resultados numa interface gráfica.




# Podemos usar a saída do ajuste para fazer a predição na área de estudo ou projetar em um novo local ou em um novo tempo. Para fazer isso, usamos a função "predict". Os argumentos básicos dessa função é o objeto dos nossos modelos seguido das variáveis ambientais da área em que se deseja projetar a adequabilidade. Se for uma outra área, utilizar as variáveis da outra área. Se for em outro momento no tempo, utilizar as variáveis do cenário que se deseja predizer.



# 4.3. Predição e visualização --------------------------------------------


# The predict function can be used for this purpose:

p1 <- predict(model, env, filename="output/predict.tif", overwrite=TRUE) # Gera a predição para cada um dos modelos feitos.

names(p1) # visualiza o nome de todas as predições criadas.

plot(p1, col = viridis::turbo(100)) # plota todas as predições criadas

# Ao invés de termos uma predição para cada modelo construído, podemos juntar todos resultados em um resultado de consenso único:o ensemble. O emsemble, resume todos os resultados em um único raster (mapa), utilizando algum critério. Para fazer isso, usamos a função "ensemble", que recebe como argumento básico o objeto do nosso modelo, seguido das variáveis ambientais da área em que se deseja projetar a adequabilidade. No argumento "setting" definimos o método que deve ser utilizado para gerar a previsão consensual. 

# No nosso projeto, faremos um ensemble usando a média ponderada pela estatística AUC (veja mais métodos no arquivo de apoio do pacote)


en1 <- ensemble(x = model, newdata = env, filename="output/ensemble.tif", parallelSetting = list(ncores = 3, method = "parallel"), setting=list(method="weighted", stat="AUC"), overwrite=TRUE )

# É possível fazer um ensemble de apenas modelos específicos usando o argumento id, dentro do argumento setting.
. 
en1 <- ensemble(model, env, filename="output/ensemble.tif", setting=list(id=c(1,3,5,8), method="weighted", stat="AUC"), overwrite=TRUE )

#Plotar um mapa básico alterando o esquema de cores.
cl<- colorRampPalette(c("#3E49BB","#3498DB","yellow", "orange", "red", "darkred"))
plot(en1, col=cl(200))


# Uma outra forma de representar o resultado é incluir a camada em cima de outros mapas em uma interface interativa. Nesse caso, usaremos o pacote mapview para visualizar os mapas.

mapview::mapview(en1, col.regions=cl(200))

# Podemos incluir os nossos pontos de ocorrência no mapa para visualizar a predição e as ocorrências registradas.  Para fazer isso, primeiro temos que ajustar a projeção dos nossos dados de ocorrência para a mesma projeção do nosso raster.

proj4string(data) <- projection(en1)

# Depois, basta incluir os pontos com o simbolo de +

mapview::mapview(en1, col.regions=cl(200)) + data


# Personalizar mapas

# Obtém um shape da América do Sul
li <- rnaturalearth::ne_countries(scale = 50, continent = "South America", returnclass = "sf") %>%
  sf::st_union(rnaturalearth::ne_countries(scale = 50, country = "France", returnclass = "sf")) %>%
  sf::st_crop(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")) %>%
  sf::st_union() %>%
  nngeo::st_remove_holes() %>%
  sf::st_as_sf()
li

# Transforma o raster da predição em tabela
da <- raster::rasterToPoints(en1) %>%
  tibble::as_tibble()
da

# Monta o mapa no ggplot

map_sui <- ggplot() +
  geom_raster(data = da, aes(x, y, fill = ensemble_weighted)) +
  geom_sf(data = li, fill = NA, color = "gray30") +
  geom_point(data = dados_ocean, aes(x = lon, y = lat), colour="black", fill= "yellow",pch=21, size = 2) +
  scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
  scale_fill_gradientn(colours = viridis::turbo(100)) +
  coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
  labs(x = "Longitude", y = "Latitude", fill = "Suitability") +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                         style = north_arrow_fancy_orienteering(line_col = "black",
                                                                fill = c("#000000","#e5e5e5"), 
                                                                text_col = "#000000",
                                                                text_face = "bold",
                                                                text_size = 12)) +
  theme_bw() +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white",
                                         size = 0.3,
                                         linetype = "solid",
                                         colour = "black"),
        axis.title = element_text(size = 12, face = "plain"),
        legend.position.inside = c(.85, .17)); map_sui



# End -------