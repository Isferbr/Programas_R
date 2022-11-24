# Criamos uma variável aleatória (xi) de tamanho 1000 e com média 1 e desvio padrão 2
# help(rnorm)
library(maxLik)
x <- rnorm(1000, media = 0, desvio = 1
# função do negativo do log da verossimilhança da Normal
neg_log_lik <- function(x, parametros){
  # parâmetros para a distribuição Normal
  media <- parametros[0]
  desvio <- parametros[1]
  n <- length(x)
  # log da verossimilhança da Normal
  ll <- -(n/2)*log(2*pi*desvio^2) + (-1/(2*desvio^2))*sum((x-media)^2)
  # retornar o negativo para maximizar ao invés de minimizar
  return(-ll)
}

# dividir a tela em duas linhas e uma coluna
par(mfrow=c(2,1))

# gráfico da função do logaritmo da verossimilhança. Vamos multiplicar por -1
# a função que já é o negativo, ou seja, voltando para o seu sinal
plot(x = seq(from = -3, to = 3, by = 0.1),
     y = -1*sapply(seq(from = -3, to = 3, by = 0.1),
                   FUN = neg_log_lik, par = c(0,1)),
     type = "l",
     ylab = "",
     xlab = "Valor da variável aleatória X",
     main = "Log da Verossimilhança para Média=0 e Variância=1")

# gráfico do negativo do logaritmo da verossimilhança
plot(x = seq(from = -3, to = 3, by = 0.1),
     y = 1*sapply(seq(from = -3, to = 3, by = 0.1), 
                  FUN = neg_log_lik, par = c(0,1)),
     type = "l",
     ylab = "",
     xlab = "Valor da variável aleatória X", 
     main = "Negativo do Log da Verossimilhança para Média=0 e Variância=1")

# retornar a tela para uma linha e uma coluna
par(mfrow = c(1,1))

###  SOLUCIONAR O PROBLEMA  ####
# Uma vez que temos uma função que deve ser maximizada e os parâmetros iniciais
normal.fit <- stats::optim(par = c(0.5,0.5), 
                           fn = neg_log_lik, 
                           x = x, 
                           method = "BFGS",
                           hessian = TRUE)

####       RESULTADOS     ######
# Parâmetros encontrados para a média e variância (condição de primeira ordem).
normal.fit$par

# Resultado: matriz hessiana (condição de segunda ordem).
normal.fit$hessian

# Determinante do primeiro menor principal líder
det(matrix(data = normal.fit$hessian[1], nrow = 1, ncol = 1))

# Determinante do segundo menor principal líder
det(matrix(data = normal.fit$hessian, nrow = 2, ncol = 2))

# Matriz de informação de Fisher
fisher.information.normal.fit <- solve(normal.fit$hessian)

standard.deviance.normal.fit <- sqrt(diag(fisher.information.normal.fit))
