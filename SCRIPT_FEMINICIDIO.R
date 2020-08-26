### ABRINDO BANCO DE DADOS 

library(readxl)
BASE_COMPLETA <- read_excel("Doutorado/Seminario Tematico/BASE_COMPLETA.xlsx")
View(BASE_COMPLETA)


### TRANSFORMANDO VARIAVEIS EM NUMERICAS 

BASE_COMPLETA$Feminicidio <- as.numeric(BASE_COMPLETA$Feminicidio)
BASE_COMPLETA$IDH <- as.numeric(BASE_COMPLETA$IDH)


#### IDENTIFICANDO OUTLIERS

boxplot(BASE_COMPLETA$Feminicidio)
boxplot(BASE_COMPLETA$POPULAÇÃO)
boxplot(BASE_COMPLETA$GINI)
boxplot.stats(BASE_COMPLETA$Feminicidio)


attach(BASE_COMPLETA)
library(ggplot2)

box <- function(var, varname){
  ggplot(data = BASE_COMPLETA, aes(x = '', y = var)) +
    geom_boxplot(width = 0.5) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = varname, x = '') +
    geom_text(aes(label = ifelse(var > quantile(var, 0.75, na.rm = T) + 1.5 * IQR(var, na.rm = T), 
                                 as.character(BASE_COMPLETA$PAÍS), '')),
              hjust = -.5, vjust = .35, size = 3)
}
box(Feminicidio, 'Feminicídio')




### ESTATISTICA DECRITIVA DOS DADOS 

summary(BASE_COMPLETA$Feminicidio)
summary(BASE_COMPLETA$GINI)
summary(BASE_COMPLETA$IDH)
summary(BASE_COMPLETA$`DESEMPREGO TOTAL`)
summary(BASE_COMPLETA$`DESEMPREGO FEMININO`)
summary(BASE_COMPLETA$PIB)

#### ESTATISTICA COMPLETA PRA TODAS VARIAVEIS 
summary(BASE_COMPLETA)


### CORRELA?AO ENTRE FEMINICIDIO E GINI
attach(BASE_COMPLETA)
install.packages("pander")
library(pander)
library(stargazer)
cor1 <- cor(Feminicidio, GINI)
pander(cor.test(Feminicidio, GINI), caption = "Correla??o Feminic?dio e Gini")
plot(Feminicidio, GINI)

cor2 <- cor(Feminicidio, IDH)
pander(cor.test(Feminicidio, IDH), caption = "Correlação Feminicídio e IDH")
plot(Feminicidio, IDH)

cor3 <- cor(Feminicidio, DESEM_TOTAL)
pander(cor.test(Feminicidio, DESEM_TOTAL), caption = "Correlação Feminicídio e Desemprego")
plot(Feminicidio, DESEM_TOTAL)

install.packages('ggarrange')
library(ggarrange)


### CARREGAR PACOTES PARA GRAFICOS
install.packages('tidyverse')
install.packages('ggpubr')

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(sjPlot)

### GRAFICO DE DISPERSAO - FEMINICIDO E GINI
attach(BASE_COMPLETA)
library(ggplot2)
attach(BASE_COMPLETA)
fig1 <- ggplot(BASE_COMPLETA, aes(x=GINI, y=Feminicidio)) + 
  geom_point() +
  theme_sjplot()
fig1

### CORRELA??O ENTRE FEMINICIDIO E IDH 
attach(BASE_COMPLETA)
library(pander)
cor(Feminicidio, IDH)
pander(cor.test(Feminicidio, IDH), caption = "Correla??o Feminicidio e IDH")

##### GRAFICO dispersao - feminicidio e IDH
library(ggplot2)
fig2 <- ggplot(BASE_COMPLETA, aes(x=IDH, y=Feminicidio)) + 
  geom_point() +
  theme_sjplot()
fig2

### CORRELA??O ENTRE FEMINICIDIO E DESEMPREGO TOTAL
attach(BASE_COMPLETA)
library(pander)
cor(Feminicidio, DESEM_TOTAL)
pander(cor.test(Feminicidio, DESEM_TOTAL), 
       caption = "Correla??o Feminicidio e Desemprego Total")

##### GRAFICO DISPERSAO - FEMINICIDIO E DESEMPREGO TOTAL
library(ggplot2)
fig3 <- ggplot(BASE_COMPLETA, aes(x= DESEM_TOTAL, y=Feminicidio)) + 
  geom_point() +
  theme_sjplot()
fig3

### CORRELA?AO ENTRE FEMINICIDIO E DESEMPREGO FEMININO
attach(BASE_COMPLETA)
library(pander)
cor(Feminicidio, `DESEMPREGO FEMININO`)
pander(cor.test(Feminicidio, `DESEMPREGO FEMININO`), 
       caption = "Correla??o Feminicidio e Desemprego Feminino")

##### GRAFICO DISPERSAO - FEMINICIDIO + DESEMPREGO FEMININO
library(ggplot2)
ggplot(BASE_COMPLETA, aes(x=`DESEMPREGO FEMININO`, y=Feminicidio)) + 
  geom_point()


### CORRELA??O ENTRE FEMINICIDIO E PIB
attach(BASE_COMPLETA)
library(pander)
cor(Feminicidio, PIB)
pander(cor.test(Feminicidio, PIB), caption = "Correla??o feminicido e PIB")


#### GRAFICO DISPERSAO - FEMINICIDIO E PIB
library(ggplot2)
ggplot(BASE_COMPLETA, aes(x=PIB, y=Feminicidio)) + 
  geom_point()



### PLOT FIGURAS DISPERSAO - CORRELAÇAO 
install.packages('ggpubr')
library(ggpubr)

Figura4 <- ggarrange(fig1, fig2, fig3, common.legend = FALSE)

annotate_figure(Figura4)


### GRAFICO DE BARRA VARIAVEL DEPENDENTE 

library(tidyverse)
library(hrbrthemes)
grafvd <- BASE_COMPLETA %>%
  ggplot( aes(x=Feminicidio)) +
  geom_histogram( binwidth=30, fill="#69b3a2", color="#e9ecef", alpha=2) +
  ggtitle("Bin size = 600") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
grafvd

BASE_COMPLETA$Feminicidio <- as.numeric(BASE_COMPLETA$Feminicidio)

hist(Feminicidio)



### MODELO DE REGRESSAO LINEAR 1 - TODAS VARIAVES + CONTROLES (PIB + POPULA?AO)

attach(BASE_COMPLETA)
library(coefplot)
library(sjPlot)
library(ggplot2)
library(sjPlot)
library(pander)

regressao1 <- lm(Feminicidio ~ GINI + IDH + DESEM_TOTAL + POP + PIB, 
                 data= BASE_COMPLETA)
summary(regressao1)
par(mfrow=c(2,2))
plot(regressao1)
coefplot(regressao1) +
theme_sjplot()


### MODELO DE REGRESSAO LINEAR 2 - SEM CONTROLES

regressao2 <- lm(Feminicidio ~ GINI + IDH + DESEM_TOTAL, 
                 data= BASE_COMPLETA)
summary(regressao2)
par(mfrow=c(2,2))
plot(regressao2)
coefplot(regressao2) +
  theme_sjplot()
  

### MODELO DE REGRESSAO DE PAINEL 1 
install.packages('plm')
install.packages('gdata')
install.packages('Hmisc')
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)

### RODANDO REGRESSAO DE PAINEL - pooling (DESEMPREGO TOTAL)
library(lme4)
library(plm)

regressao3 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH + 
                    `DESEMPREGO TOTAL` + POPULAÃ‡ÃƒO + PIB,
                  model= 'pooling')
summary(regressao3)
regressao3


### RODANDO REGRESSÃƒO DE PAINEL - within (DESEMPREGO TOTAL)
attach(BASE_COMPLETA)
regressao4 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH + 
                    `DESEMPREGO TOTAL` + POPULAÃ‡ÃƒO + PIB,
                  model= 'within')
summary(regressao4)
regressao4

### REGRESSAO PAINEL POOLING - (DESEMPREGO FEMININO)
library(lme4)
library(plm)

regressao5 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH +
                    `DESEMPREGO FEMININO` + POPULAÃ‡ÃƒO + PIB,
                  model= 'pooling')
summary(regressao5)
regressao5


### REGRESSAO PAINEL within (DESEMPREGO FEMININO)
attach(BASE_COMPLETA)
regressao6 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH +
                    `DESEMPREGO FEMININO` + POPULAÃ‡ÃƒO + PIB,
                  model= 'within')
summary(regressao6)
regressao6


#### PACOTE OUTLIERS 

install.packages('outliers')
library(outliers)
outlier(BASE_COMPLETA$Feminicidio)

#### REGRESSAO LINEAR - SEM OUTLIERS (TODAS AS VARIAVEIS)
attach(BASE_COMPLETA)
library(ggplot2)




attach(BASE_COMPLETA)
regressao10 <-lm(data = BASE_COMPLETA[BASE_COMPLETA$PAÍS!= 
                          c('Brasil', 'México', 'Guatemala')], 
   Feminicidio ~ GINI + IDH + `DESEMPREGO FEMININO` + `DESEMPREGO TOTAL` +
     PIB + POPULAÇÃO)




#### REGRESSAO DE PAINEL 1 - EFEITOS FIXOS

attach(BASE_COMPLETA)
library(lme4)
library(plm)
reg1 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              PIB + POP,
                   cluster = 'ANO', model = 'within', index = 'ANO')
summary(reg1)
reg1

### GERANDO GRAFICO COEFPLOT REG 1

library(coefplot)
library(sjPlot)
coefplot(reg1) +
theme_sjplot()



        
### REGRESSAO PAINEL 2 - MODELO POOLED
library(lme4)
library(plm)
reg2 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              POP + PIB, model = 'pooling')
summary(reg2)
reg2
plot(reg2)  
coefplot(reg2) + 
  theme_sjplot()


### REGRESSAO EFEITO FIXO POR PAÍS 

reg3 <- plm(data = BASE_COMPLETA, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              PIB + POP,
            cluster = 'PAÍS', model = 'within', index = 'PAÍS')
summary(reg3)
reg3
library(coefplot)
library(sjPlot)
coefplot(reg3) +
  theme_sjplot()

### TESTE F DE CHOW - QUAL MODELO MAIS ADEQUADO (COM OUTLIERS)
library(plm)
pFtest(reg3, reg2)

#### REGRESSAO SEM OUTLIERS 

##### ABRIR BANCO DE DADOS 2 
library(readxl)
base_exclu <- read_excel("Doutorado/Seminario Tematico/base_exclu.xlsx")
View(base_exclu)

### TRANSFORMAÇÃO VARIAVEIS NUMERICAS

base_exclu$Feminicidio <- as.numeric(base_exclu$Feminicidio)
base_exclu$IDH <- as.numeric(base_exclu$IDH)


### REGRESSAO 4 - EFEITO FIXO POR ANO (SEM OUTLIERS)

attach(base_exclu)
library(lme4)
library(plm)
reg4 <- plm(data = base_exclu, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              PIB + POP,
            cluster = 'ANO', model = 'within', index = 'ANO')
summary(reg4)
reg4
library(coefplot)
library(sjPlot)
coefplot(reg4) +
  theme_sjplot()


#### REGRESSAO 5 - EFEITO FIXO POR PAIS (SEM OUTLIERS)
attach(base_exclu)
library(lme4)
library(plm)
reg5 <- plm(data = base_exclu, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              PIB + POP,
            cluster = 'PAÍS', model = 'within', index = 'PAÍS')
summary(reg5)
reg5
library(coefplot)
library(sjPlot)
coefplot(reg5) +
  theme_sjplot()


#### REGRESSAO 6 - MODELO POOLED (SEM OUTLIERS)
attach(base_exclu)
library(lme4)
library(plm)
reg6 <- plm(data = base_exclu, Feminicidio ~ GINI + IDH + DESEM_TOTAL +
              POP + PIB, model = 'pooling')
summary(reg6)
reg6
library(coefplot)
library(sjPlot)
plot(reg6)  
coefplot(reg6) + 
  theme_sjplot()


#### TESTE F DE CHOW - verificar qual modelo mais adequado
library(plm)
pFtest(reg5, reg6)
