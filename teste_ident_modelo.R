
## ---

## title: Script para teste de identidade de modelo
## subtitle: Conforme descrito por Regazzi, 1993

## author:
## - Sollano Rabelo Braga 
## - Marcio Leles Romarco de Oliveira

## date: Dezembro, 2016

## output:

##    pdf_document:

##      toc: true

##      toc_depth: 2

##      highlight: tango

##    html_document:

##      toc: true

##      toc_float: true

##      toc_depth: 2

##      highlight: tango

## ---
## \pagebreak
##

## # 1) Carregar pacotes e dados ####

## Primeiro carrega-se os pacotes que serão utilziados neste script.
##
## O pacote readxl será utilizado para carregar os dados direto da planilha do excel,
## e o opacote ggplot2 para que se plote o gráfico ao final do teste.
##
## Todos os calculos serão feitos utilizando apenas funções do R base.
##
## Carrega-se os pacotes com a função library:
library(readxl)
library(ggplot2)

## O exemplo que será utilizado neste script possui dados de dois projetos,
## Diamantina e Serro, e deseja-se saber se o comportamento do diametro é semelhante
## nas duas cidades.
##
## Utilizando a função read_excel, carrega-se a planilha do excel em um objeto:
dados <- read_excel("dados.xlsx")

## Visualiza-se os dados:
dados

## # 2) Definição e ajuste do modelo reduzido ####

## O modelo utilizado para este exemplo será o quadrático, com o 
## diâmetro em função do nitrogenio (N).
##
## A seguir ajusta-se o modelo reduzido, e salva-se o seu resultado em um objeto:
lm_redz <- lm(DAP ~  N + N2, dados)
summary(lm_redz)

## # 3) Criação das variáveis Dummy ####

## O número de dummies criadas é baseado no número comparações que serão feitas,
## que neste caso é o número de projetos. Como são dois projetos, serão duas dummies binárias,
## e duas dummies para cada variável do modelo reduzido, totalizando em 6 dummies.
##
## Para isso é necessário que se tenha o registro dos níveis do fator utilizado,
## no caso, quais projetos serão comparados. A variável que possui essa informação
## é a variável PROJETO, e pode-se verificar os seus níveis com a função levels:

fator <- as.factor(dados$PROJETO)

## Agora cria-se um objeto que contem os nomes dos projetos que serão comparados:
factor_levels <- levels(fator)
factor_levels

## Pronto, agora a informação sobre os projetos foi salva. O próximo passo é a criação 
## das dummies.
##
## Primeiro serão criadas as dummies binárias, e em seguida as dummies que possuem
## as informações de N e N2. 
##

## ## 3.1) Dummies binárias ####

## A seguir cria-se as dummies binárias com um loop for. O loop vai de 1:2,
## pois existem dois projetos para comparação:

## Antes de realizar o loop, cria-se a lista que será utilizado:
lista1 <- list()

for(i in 1:2){
  
  lista1[[paste("D", i, sep = "")]] <- ifelse(dados$PROJETO == factor_levels[i], 1, 0 )
}
lista1

## Agora basta converter a losta em uma matriz:
dummies1 <- do.call(cbind, lista1)
dummies1

## ## 3.2) Dummies para as variáveis ####

## Agora cria-se cria-se as variáveis dummies para cada variável 
## do modelo reduzido (N e N2), repetindo o processo anterior.
##
## Para proceder é necessária a criação de um objeto que possua os nomes dessas variáveis:
VARSX <- c("N", "N2")

## Agora cria-se a lista vazia:
lista2 <- list()

## Para que isso seja possível foi utilizado outro loop, um nível 
## acima do anterior, ou seja,  j representa as variáveis 
## do modelo reduzido, e i representa os projetos:
for(j in 1:2){
  
  for(i in 1:2){
    
    lista2[[paste("D", i, VARSX[j],sep = "")]] <- ifelse( 
      dados$PROJETO == factor_levels[i], dados[[   VARSX[j]   ]] ,  0  ) 
  }
}

lista2

## Agora converte-se a lista em matriz:
dummies2 <- do.call(cbind, lista2)
dummies2

## E por fim une-se as variáveis dummies criadas em um único objeto:
dummies_f <- data.frame(dummies1, dummies2)
dummies_f

## Agora une-se as dummies aos dados originais:
dados_comp <- cbind(dados, dummies_f)
dados_comp

names_dummies <- names(dummies_f)
names_dummies

## # 4) Ajuste do modelo completo ####

## Agora realiza-se o ajuste do modelo completo,
## composto pelas variáveis dummies.
## 
## É utilizado o -1 ao final do modelo, para que se anule o b0:
lm_comp <- lm( DAP ~ D1 + D2 + D1N + D2N + D1N2 + D2N2 - 1 , dados_comp)

summary(lm_comp)

## # 5) Criação da Anova ####

## Agora cria=se os objetos necessários para a construção da anova:

## Correção:
C <- (sum(dados$DAP))^2/nrow(dados)

## Os graus de liberdade serão retirados com base no número de 
## variáveis no lado x dos modelos:
##
## Graus de liberdade do modelo completo:
gl_comp <- ncol(dummies_f)

## Graus de liberdade do modelo reduzido:
gl_redz <- length(VARSX)

## Graus de liberdade da redução:
gl_reducao <- gl_comp - gl_redz

## Graus de liberdade do resíduo:
gl_residuo <- lm_comp$df.residual

## Soma de quadrado de parâmetros (modelo completo):
SQParamC <- sum(lm_comp$fitted.values^2)

## Soma de quadrado da regressão do modelo reduzido:
SQParamR <- sum(lm_redz$fitted.values^2) # + C

## Soma de quadrado da redução:
SQ_reducao <- SQParamC - SQParamR

## Soma de quadrado dos resíduos modelo completo:
SQRes_comp <- sum(lm_comp$residuals^2)

## Quadrado médio de Parametro modelo Completo:
QMParamC <- SQParamC/gl_comp

## Quadrado médio de Parametro modelo Completo:
QMParamR <- SQParamR/gl_redz

## Quadrado médio da redução:
QMReducao <- round(SQ_reducao / gl_reducao, 4)

## Quadrado médio do resíduo:
QMResiduo <- round(SQRes_comp / gl_residuo, 4)

## Calculo do F:
F_regazzi <- round(QMReducao / QMResiduo, 2)

## Calculo do F crítico:
F_tabelado <- round(qf(p = 0.05, df1 = gl_reducao , df2 = gl_residuo, lower.tail = F ), 2)

## Cálculo do p-valor:
p_valor <- pf(F_regazzi , df1 = gl_reducao , df2 = gl_residuo, lower=F)

## Resultado do teste:
resultado <- ifelse(p_valor < 0.05, "*", "ns")

## Agora basta unir tudo em um data frame:
tabela_regazzi <- data.frame(
  
  FV = c("Parametro_c", "Parametro_r", "Reducao", "Residuo"),
  GL = c(gl_comp, gl_redz, gl_reducao, gl_residuo ),
  SQ = round(c(SQParamC, SQParamR, SQ_reducao, SQRes_comp), 2),
  QM = c(QMParamC, QMParamC, QMReducao, QMResiduo ),
  F_Regazzi = c("","", F_regazzi ,""),
  F_tabelado = c("","", F_tabelado,""),
  p.valor = c("", "", signif(p_valor, 3),  ""),
  Resultado = c("", "", resultado,  "")
)
tabela_regazzi 

## # 6) Gráfico ####

## Para realizar o gráfico, utiliza-se a função ggplot:
ggplot(dados, aes(N, DAP) ) +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2, raw=T) ,aes(color=PROJETO), se = F, size = 1.5) + 
  stat_summary(fun.y = mean, geom = "point", size = 3) + 
  theme_gray(base_family = "serif") +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(color="black"),
    axis.line.y = ggplot2::element_line(color="black"),
    legend.title    = ggplot2::element_text(size = 12, face = "bold"),
    legend.text     = ggplot2::element_text(size = 12),
    axis.title      = ggplot2::element_text(size = 14), 
    axis.text       = ggplot2::element_text(size = 12) )
## # 7) Pacote forestr ####
## \pagebreak
##
## O teste pode ser feito de forma direta com o pacote forestr (ainda em desenvolvimento):
##
## Por padrão o F calculado e o resultado utilizam 5% de significancia, 
## que pode ser alterada com o argumento signif, caso desejado:
forestr::ident_model(dados, "PROJETO", DAP ~  N + N2)

## Com a saída completa são mostrados o gráfico dos modelos, 
## a tabela de dummies, o relatório dos dois modelos, e a tabela anova:
forestr::ident_model(dados, "PROJETO", DAP ~  N + N2, output = "full")

## # 8) Citação ####
## Na citação  você pode utilizar: (BRAGA; OLIVEIRA, 2017)
##
## Na referencia, você pode usar
##
## BRAGA S.R; OLIVEIRA M.L.R. Rotina para realização do teste de identidade de modelo. 2017. Disponível em: https://github.com/sollano/teste_ident_modelo
##



