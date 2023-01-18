#Curso de regressão - Naiara Alcantara - 
#Disciplina UFPA

#Regressão####
##Linear simples####

#Primeiro definir o diretório de trabalho
getwd()
setwd("C:/Users/nayar/OneDrive/8. AMBIENTE DE PROGRAMAÇÃO R/1. CURSO 2023")

#Pacotes utilizados####
library(readr)
library(ggplot2)
library(sjPlot)
library(memisc)
library(ggfortify)
library(olsrr)
library(coefplot)


#Abertura da base de dados
sen2018 <- read_delim("sen2018.csv", delim = ";", 
                      escape_double = FALSE, col_types = cols(IDADE_DATA_POSSE = col_number(), 
                                                              votos = col_number(), TOTAL_RECEITAS = col_number()), 
                      trim_ws = TRUE)

#Visualização da base de dados####
View(sen2018)   

colnames(sen2018)
# [1] "SQ_CANDIDATO"             "SIGLA_UF"                 "NOME_CANDIDATO"          
# [4] "NUMERO_PARTIDO"           "SIGLA_PARTIDO"            "IDADE_DATA_POSSE"        
# [7] "DESCRICAO_SEXO"           "DESCRICAO_GRAU_INSTRUCAO" "DESCRICAO_ESTADO_CIVIL"  
# [10] "DESCRICAO_COR_RACA"       "DESCRICAO_OCUPACAO"       "DESC_SIT_TOT_TURNO"      
# [13] "SITUACAO_REELEICAO"       "DES_SITUACAO_CANDIDATURA" "votos"                   
# [16] "TOTAL_RECEITAS"          

table(sen2018$DES_SITUACAO_CANDIDATURA)
# APTO 
# 311 

table(sen2018$DESC_SIT_TOT_TURNO)
# ELEITO NÃO ELEITO 
# 54           257 

#O total de votos está relacionado com ter sido eleito? 
#O que será que influencia o total de votos?
#Será que a receita pode ser uma variável preditiva 

#Icialmente temos que deverificar se há relação entre 
# total de votos (que variável é essa?)
# e receita (e essa, como podemos chamá-la?)


#Gráfico dispersão####
#Análise gráfica entre receitas e votos
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point()
#Imagem 1 


#Notem que os valores das receitas são tão altos 
#que aparecem em notação científica, queremos alterar isso?
#Se sim, podemos fazer o seguinte:
options(scipen = 999)
#Imagem 2 

#Organização do gráfico
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receita", y= "Votos", title = "Receitas por votos") +
  theme_classic()
#Imagem 3

Gra1 <- ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receita", y= "Votos", title = "Receitas por votos") +
  theme_classic()


#Imagem 4 - aprimorando
Gra1 <- ggplot(data = sen2018, 
               aes(y = votos, x= TOTAL_RECEITAS,
                   size = TOTAL_RECEITAS, 
                   color =votos)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T, color = "red") +
  labs(x ="Receita", y= "Votos", 
       title = "Receitas por votos")+ theme_classic() 


formatacao <- theme(text = element_text(family = "serif", size = 14),
                    title = element_text(color = "black"),
                    axis.line = element_line(color = "black")) +
  theme(legend.position="none") 

# “left”,“top”, “right”, “bottom” anda none

Gra1 + formatacao

#Certo, essa já uma visualização gráfica mais próxima 
#do real, no entanto, vejam que as escolas de valores 
#são bastante distintas?

#Então para melhorar a visualização e tornar estatísticamente
#mais proporcional os dados, podemos realizar umatransformação logarítmica
#utilizando a função log

sen2018$log.votos <- log(sen2018$votos)
sen2018$log.receitas <- log(sen2018$TOTAL_RECEITAS)


ggplot(data = sen2018, aes(y = log.votos, x= log.receitas )) +
  geom_point()

#Construção do modelo
Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#Apresentação do Erro
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#  NA/NaN/Inf in 'y'

#Análise da base para identificar o erro

#Correção do erro
sen2018[is.na(sen2018) | sen2018== "Inf"] =NA
sen2018[is.na(sen2018) | sen2018== "-Inf"] =NA
sen2018[is.na(sen2018) | sen2018== "NAN"] =NA

#Modelo após a correção
Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#OU
Model1.1 <- lm(log.votos ~ log.receitas, data = sen2018)

summary(Model1.1)
#Interpretação no slide

# Call:
#   lm(formula = log.votos ~ log.receitas, data = sen2018)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.9274 -0.8677 -0.0294  0.9192  3.7049 
# 
# Coefficients:
#   Estimate Std. Error t value            Pr(>|t|)    
# (Intercept)   7.43396    0.34037   21.84 <0.0000000000000002 ***
#   log.receitas  0.30823    0.02228   13.84 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.366 on 300 degrees of freedom
# (9 observations deleted due to missingness)
# Multiple R-squared:  0.3896,	Adjusted R-squared:  0.3876 
# F-statistic: 191.5 on 1 and 300 DF,  p-value: < 0.00000000000000022

#Construção da tabela que poderá ser apresentada junto ao texto
#Usando a função tab_model do pacote sjPlot
tab_model(Model1.1, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

#Análise de resíduos####
resid <-  (cbind(sen2018$log.votos, predict(Model1.1), 
                 residuals(Model1.1)))
view(resid)

#Análise de resíduos através de gráficos#### 
#Função autoplot

obj1 <- autoplot(Model1,
                 which = 1:4,
                 nrow = 2,
                 ncol = 2) 

obj1 + theme_classic() 

#Gráficos:
#1º resíduos pelos valores ajustados, 
#linearidade, aproximadamente horizontal
#O primeiro gráfico exibido é útil para testarmos a
#independência entre valores preditos e resíduos

#2º Distribuição normal, deve estar em cima da linha
#O Normal QQ plot nos ajuda a verificar essa 
#exigência ao exibir no eixo horizontal a distribuição
#esperada em uma distribuição normal e no
#vertical os resíduos padronizados

#3º Homocedasticidade - não pode ter padrão triangular
#4º Mostra pra gente se existem outliers


#Linear multipla#### 

#Quais outras variáveis podem ser interessantes para análise 
#da quantidade de votos?
#Idade na época da posse?
#Sexo?
#Grau de instrução?
#Raça?


#Então incialmente vamos rapidamente olhar e organizar essas variáveis 
#Para pode inserir no teste

summary(sen2018$IDADE_DATA_POSSE)
sen2018$Idade <- sen2018$IDADE_DATA_POSSE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 27.0    47.0    55.0    55.1    63.0    83.0 

table(sen2018$DESCRICAO_SEXO)
sen2018$Sexo <- sen2018$DESCRICAO_SEXO
# FEMININO MASCULINO 
# 56       255 

table(sen2018$DESCRICAO_GRAU_INSTRUCAO)

sen2018$Instrução <- sen2018$DESCRICAO_GRAU_INSTRUCAO
sen2018$Instrução <- as.factor(sen2018$Instrução)
sen2018$Instrução <- recode(sen2018$Instrução, 
                            "Baixa" <-  c("ENSINO FUNDAMENTAL INCOMPLETO",
                                          "ENSINO FUNDAMENTAL COMPLETO"),
                            "Média" <-  c("ENSINO M\xc9DIO COMPLETO",
                                          "ENSINO M\xc9DIO INCOMPLETO"),
                            "Superior" <-  c("SUPERIOR COMPLETO",
                                             "SUPERIOR INCOMPLETO"))

table(sen2018$DESCRICAO_COR_RACA)
sen2018$Raca <- sen2018$DESCRICAO_COR_RACA
sen2018$Raca <-  as.character(sen2018$DESCRICAO_COR_RACA)
# IND\xcdGENA     AMARELA      BRANCA       PARDA       PRETA 
#           2           1         210          66          32 

Model2 <- lm(log.votos ~ log.receitas +
               Idade + Sexo +
               Instrução + Raca , data = sen2018)

summary(Model2)

tab_model(Model2, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "numeric_stars")

#Condionantes para o teste de regressão####
#VIF: Variance Inflation Factors

library(olsrr) # Pacote para VIF


ols_vif_tol(Model2)


# Variables  Tolerance       VIF
# 1      log.receitas 0.93141736  1.073633
# 2             Idade 0.90301139  1.107406
# 3     SexoMASCULINO 0.96812327  1.032926
# 4 InstruçãoSuperior 0.99366053  1.006380
# 5       RacaAMARELA 0.50128312  1.994881
# 6        RacaBRANCA 0.01698194 58.886088
# 7         RacaPARDA 0.02266780 44.115445
# 8         RacaPRETA 0.03725117 26.844794

#Condition Index####
ols_eigen_cindex(Model2)


#Coefplot 

#O Coefplot é um tipo de gráfico que pode ser utilizado
#para apresentar graficamente os resultados de um teste 
#de regressão

obj1 <- coefplot(Model2, title = "Análise dos votos a senado em 2018",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

obj2 <- obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                            alpha=105) 
obj2 + formatacao 

