#########################################################################
# Arquivo para replicabilidade
# A percepção do impacto das elições de 2018 sobre a violência contra pessoas
# LGBT nos estados da BA, SP e RJ

# Autores: Victor Barbosa Barbosa
#          Ana Carolina dos Santos Costas

#-------------------------------------------------------------------------

# Definindo diretorio

setwd("C:/Users/DELL/Documents/2019.2/Projetos/Artigo LGBT/Arquivos quantitativos")

#-----------------------------------------------------------------------------------------------------

# bibliotcas uteis

library(readxl)
library(ggplot2)
#------------------------------------------------------------------------------------------------------

# carregando a base de dados 

bd <- read_xlsx("C:/Users/DELL/Documents/2019.2/Projetos/Artigo LGBT//Arquivos quantitativos/Dados_Abertos_Violencia_LGBT+nas Eleicoes.xlsx")

# visualiando os dados 

head(bd)
tail(bd)

# Selecionando as variaveis (por questoes de organizacao, optou-se porque renomear as variaveis)

bd$estado <- bd$Estado

bd$vitimizacao <- bd$`P.9- Você sofreu algum tipo de violência motivada por sua orientação sexual e/ou identidade de gênero durante as eleições de 2018?`

bd$engajamento <- bd$`P.1- Como você considera seu grau de envolvimento com as eleições de 2018?`

bd$percepcao <- bd$`P.3- Durante as eleições, no segundo semestre de 2018, você considera que a violência contra pessoas LGBT+:` 

bd$terceiros <- bd$`P.5- Você tomou conhecimento de violência(s) cometida(s) contra conhecido/a LGBT+ ou pessoa próxima LGBT+ durante o segundo semestre de 2018?`

bd$contexto <- bd$`P. 6- Você considera que a(s) violência(s) cometida(s) esteja(m) relacionada(s) em alguma medida com o contexto eleitoral de 2018?`

# ---------------------------------------------------------------------------------------------------

# Separando as variaveis que serao utulizadas no estudo em um bjeto menor

variaveis <- c("estado", "vitimizacao", "engajamento", "percepcao", "contexto", "terceiros")


# Criando uma nova base apenas com as variaveis que usaremos no trabalho ####

base <- bd[, variaveis]

# ---------------------------------------------------------------------------------------------------

# Transformando as variaveis ####

# Engajamento #### ----------------------------------------------------------------------------------

table(base$engajamento) # visualizando os dados

base$engajamento <- ifelse(base$engajamento %in% c("nao sabe", "nenhum engajamento"), "nenhum", base$engajamento)
base$engajamento <- ifelse(base$engajamento == "pouco engajamento", "pouco", base$engajamento)
base$engajamento <- ifelse(base$engajamento == "muito engajamento", "muito", base$engajamento)

table(base$engajamento)


# Estado #### ----------------------------------------------------------------------------------------

table(base$estado)

base$estado <- ifelse(base$estado == "bahia", "Bahia", base$estado)
base$estado <- ifelse(base$estado == "rio de janeiro", "Rio de Janeiro", base$estado)
base$estado <- ifelse(base$estado == "sao paulo", "São Paulo", base$estado)

table(base$estado)

# Contexto ------------------------------------------------------------------------------------------

table(base$contexto)

base$contexto <- ifelse(base$contexto %in% c("NA", "nao sabe/nao respondeu"), "nao", base$contexto)

# Terceiros -----------------------------------------------------------------------------------------

table(base$terceiros)

base$terceiros <- ifelse(base$terceiros %in% c("nao sabe/nao respondeu"), "nao", base$terceiros)

# Vitimizacao -----------------------------------------------------------------------------------

table(base$vitimizacao)

base$vitimizacao <- ifelse(base$vitimizacao == "nao sabe/nao respondeu", "nao", base$vitimizacao)

#####################################################################################################

# Gráaficos #### 

# Figura 6 - Grafico de barras empilhadas associando estados a vitimizacao ------------------------------------------

t.estado.vit <- table(base$estado, base$vitimizacao) # salvando tabela de frequencia de todas as variaveis do grafico 

# transformando os valores do objeto t.estad.vit em decimais 

t.estado.vit['Bahia',] <- t.estado.vit['Bahia',] / sum(t.estado.vit['Bahia',])
t.estado.vit['Rio de Janeiro',] <- t.estado.vit["Rio de Janeiro",] / sum(t.estado.vit['Rio de Janeiro',])
t.estado.vit['São Paulo',] <- t.estado.vit['São Paulo',] / sum(t.estado.vit['São Paulo',])

t.estado.vit # para visualizar a tabela no console e preencher o data.frame para a criacao do grafico

g.estado.vit <- data.frame(estado = c('Bahia', 'Bahia', 'Rio de Janeiro', 'Rio de Janeiro', ' São Paulo', ' São Paulo'),
                               vitima = c("Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima"),
                               frequencia = c(0.4947368, 0.5052632, 0.4600000, 0.5400000, 0.5024390, 0.4975610))



ggplot(g.estado.vit, aes(y =frequencia, x = estado, fill = vitima)) +
  geom_bar(stat = 'identity') +
  labs(y = "Frequência", x = "", fill = "Vitimização") + #alterando os rotulos
  ggtitle("Figura 6: Vitimização nos estados") +
  geom_hline(yintercept = 0.5)


# em sp foi entrevistas duas vezes mais pessoas que nos outros estados e msm assim
# os resultados continuam semelhantes

# Grafico associando engajamento a vitimizacao --------------------------------------------------------------------


t.eng.vit <- table(base$engajamento, base$vitimizacao) # unindo as tableas de frequencia

table(base$engajamento)

# transformando os valores do objeto t.eng.vit em decimais 

t.eng.vit['muito',] <- t.eng.vit['muito',] / sum(t.eng.vit['muito',])
t.eng.vit['nenhum',] <- t.eng.vit['nenhum',] / sum(t.eng.vit['nenhum',])
t.eng.vit['pouco',] <- t.eng.vit['pouco',] / sum(t.eng.vit['pouco',])

table(t.eng.vit) # para preencher o data.frame

g.eng.vit <- data.frame(engajamento = c("muito", "muito", "pouco", "pouco", "nenhum", "nenhum"),
                           vitima = c("Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima"),
                           frequencia = c(0.4268775, 0.5731225, 0.5703125, 0.4296875, 0.7894737,0.2105263))



ggplot(g.eng.vit, aes(y =frequencia, x = engajamento, fill = vitima)) +
  geom_bar(stat = 'identity') +
  labs(y = "Frequência", x = "", fill = "Vitimização") + #alterando os rotulos
  ggtitle("Figura 5: Engajamento associado à vitimização") +
  geom_hline(yintercept = 0.5)



#### Figura 3 #### Percepcao de violencia #### ----------------------------------------

table(base$percepcao)

g.per <- data.frame(rotulos= c("aumentou muito", "aumentou pouco", "nem aumentou nem diminuiu", 
                               "diminuiu muito", "diminuiu pouco", "não sabe/não respondeu"),
                    frequência = c(321, 50, 16, 1, 2, 10))

ggplot(g.per, aes(y= frequência, x= rotulos)) +
  geom_bar(colour = "black", fill="black", width=.8, stat="identity") +
  coord_flip() +
  scale_x_discrete(name="Percepção de violência") +
  ggtitle("Figura 3: Percepção de violência")

#### Figura 4 - Grafico de barras para varivael contexto --------------------------------------------------------

table(base$contexto) # visualizando os dados

#Construindo dataframe

g.contexto <-data.frame(Contexto = c("Influenciou", "Não influenciou"),
                        Frequência = c(332, 67))

ggplot(g.contexto, aes(y = Frequência, x = Contexto)) + # componenetes basicos do grafico
  geom_bar(stat = "identity") +   #  stat = "identity" por padrao
  ggtitle("Figura 4: Contexto eleitoral e violência contra LGBTs")

# Figura 2 #### Grafico de barras da variavel terceiros --------------------------------------------------------
# Você tomou conhecimento de violência(s) cometida(s) contra conhecido/a LGBT+ ou pessoa próxima LGBT+ durante o segundo semestre de 2018?`


table(base$terceiros)

# Criando dataframe

g.terceiros <- data.frame(vitimadas = c("não", "sim"),
                          frequência = c(50, 349))


ggplot(g.terceiros, aes(y = frequência, x = vitimadas)) + # componenetes basicos do grafico
  geom_bar(stat = "identity") +   #  stat = "identity" por padrao
  ggtitle("Figura 2: Violência contra conhecidos/pessoas próximas")


# Figura 1 #### Grafico da variavel vitimizacao ---------------------------------------------------------

table(base$vitimizacao)

g.vit <- data.frame(vitização = c("não", "sim"),
                    frequência = c(196, 204))

ggplot(g.vit, aes(x = vitização, y = frequência)) +
  geom_bar(stat = "identity") +
  ggtitle("Figura 1: Vitimização") +
  geom_hline(yintercept = 196)

#-----------------------------------------------------------------------------------------------------
###########################################################################################

# Iniciando analise das estatistícas de geografia de voto

# definindo diretorio

getwd()

# solicitando pacotes necessarios


library(readxl)
library(ggplot2)
library(stringr)

# GEOGRAFIA DE VOTO 

#-----------------------------------------------------------------------------------------------------

# BAHIA

# solicitando abertura do arquivo

dBA1 <- read.csv("resultado_eleicao_turno_1_BA.csv", 
                 header = TRUE, 
                 sep = ";")  # banco de dados para o turno 1

#observando o banco 
head(dBA1)
dim(dBA1)

# filtrando as linhas necessarias
dBA1 <- dBA1 [1:2,]

View(dBA1)

# gerando o grafic0

GVBA1 <- ggplot(data = dBA1, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()


GVBA1 + ggtitle("Figura 7: Resultado Eleitoral - BA 1º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4"))

# abrindo o banco de dados para bahia 2º turno
dBA2 <- read.csv("resultado_eleicao_turno_2_BA.csv", 
                 header = TRUE, 
                 sep = ";")

#observando o banco 
head(dBA2)
dim(dBA2)


# gerando o grafic0

GVBA2 <- ggplot(data = dBA2, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()

GVBA2 + ggtitle("Figura 8: Resultado Eleitoral - BA 2º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4")) 


#-----------------------------------------------------------------------------------------------------
# RIO DE JANEIRO

# solicitando abertura do arquivo para Rj 

dRJ1 <- read.csv("resultado_eleicao_turno_1_RJ.csv", 
                 header = TRUE, 
                 sep = ";")  # banco de dados para o turno 1

head(dRJ1)

# filtrando as linhas necessarias
dRJ1 <- dRJ1 [c(1,3),]

# gerando o grafic0

GVRJ1 <- ggplot(data = dRJ1, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()

# adicionando rotulos 
GVRJ1 + ggtitle("Figura 9: Resultado Eleitoral - RJ 1º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4")) 

# abrindo o banco de dados para RIO DE JANEIRO 2º turno
dRJ2 <- read.csv("resultado_eleicao_turno_2_RJ.csv", 
                 header = TRUE, 
                 sep = ";")

#observando o banco 
head(dRJ2)
dim(dRJ2)


# gerando o grafic0

GVRJ2 <- ggplot(data = dRJ2, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()

GVRJ2 + ggtitle("Figura 10: Resultado Eleitoral - RJ 2º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4")) 


#-----------------------------------------------------------------------------------------------------
# solicitando abertura do arquivo para SP

dSP1 <- read.csv("resultado_eleicao_turno_1_SP.csv", 
                 header = TRUE, 
                 sep = ";")  # banco de dados para o turno 1

head(dSP1)

# filtrando as linhas necessarias
dSP1 <- dSP1 [1:2,]

# gerando o grafic0

GVSP1 <- ggplot(data = dSP1, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()

# adicionando rotulos 
GVSP1 + ggtitle("Figura 11: Resultado Eleitoral - SP 1º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4")) 


# abrindo o banco de dados para SAO PAULO 2º turno
dSP2 <- read.csv("resultado_eleicao_turno_2_SP.csv", 
                 header = TRUE, 
                 sep = ";")

#observando o banco 
head(dSP2)
dim(dSP2)


# gerando o grafic0

GVSP2 <- ggplot(data = dSP2, aes(x = Nome.do.candidato..urna., y = X..votos.válidos, fill = as.factor(Nome.do.candidato..urna.))) + geom_col() + theme_light()

GVSP2 + ggtitle("Figura 12: Resultado Eleitoral - SP 2º Turno") + 
  labs(x = "Candidato", y = "Percentual de Votos", fill = "Candidato") +
  scale_fill_manual(values=c("firebrick3", "springgreen4"))




