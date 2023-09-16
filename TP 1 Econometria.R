library(readxl) 

data <- readxl::read_xlsx("C:\\Users\\matia\\Desktop\\Facultad\\R scrpits\\Data Base Ejercicios\\Base de datos TP 1.xlsx")

# Modelo Lineal sin logs#

Y <- data$Y
X <- as.matrix(data[2:5])
Unos <- rep(1,length(X[,1]))

X <- cbind(Unos,X)

X_inv <-  solve(t(X) %*% X)
beta_hat <- solve(t(X) %*% X) %*% (t(X)%*%Y); beta_hat

#Ya con los Betas calculados, procedemos a calcular los R#

Y_hat <- X %*% beta_hat
e_hat <- Y - Y_hat

SRC <- t(e_hat) %*% e_hat

Y_centrada_cuadrada <- (Y - mean(Y))^2
VT <- sum(Y_centrada_cuadrada)
R2 <- 1 - (SRC / VT)
N <- length(Y)
K <- length(beta_hat)
R2_ajustado <- 1 - ((1-R2)*((N-1)/(N-K)))

R2
R2_ajustado

#Tests T y F#

sigma2_hat <- SRC / (length(Y) - length(beta_hat))
sigma2_hat <- as.vector(sigma2_hat)
var_covar <- sigma2_hat * (solve(t(X) %*% X))

D <- diag(var_covar)
t <- beta_hat / sqrt(D)

# T critico#
T_Critico <- abs(qt(p = 0.025,
   df= N - 1
)) T_Critico
#T empirico#
T_Emp <- as.vector(2*pt(t,df=N-K)); T_Emp

ifelse(T_Critico>T_Emp, "No Rechazo", "Rechazo")

#Aca lo que hice fue en vez de hacer un codigo para cada Beta los hice todos juntos entiendo que es mas eficiente#

#Test F#

F_Calc <- ((R2)/(K-1))/((1-R2)/(N-K)); F_Calc #F empirico#
F_critico <- qf(0.05,df1=K-1,df2=N-K, ncp = F, lower.tail = F); F_critico

#Hacemos el Test#

Resultado <- if(F_critico>F_Calc) {
  "No Rechazo"
} else {
  "Rechazo"
} 
Resultado

# Test de normalidad Jarque-Bera#

Vector_e_hat <- as.vector(e_hat)

install.packages("moments")
library(moments)
jarque.test(Vector_e_hat) #El test indica que no har normalidad en las perturbaciones el P valor esta por debajo de 0.05 Rechaza H0#

#Resumen para contrastar los datos encontrados#

formula <- 'Y ~ X2 + X3 + X4 + X5'
resultados <- lm(formula = formula,data=data)
summary(resultados)

t
T_Critico

######################################## MODELO CON LOGARITMOS ########################################################

library(readxl) 

data <- readxl::read_xlsx("C:\\Users\\matia\\Desktop\\Facultad\\R scrpits\\Data Base Ejercicios\\Base de datos TP 1.xlsx")

Y <- data$Y
X <- as.matrix(data[2:5])
Unos <- rep(1,length(X[,1]))

Y <- log(Y)
X <- data[2:5]
X$X2 <- log(X$X2)
X$X3 <- log(X$X3)
X <- as.matrix(X)
X <- cbind(Unos,X)

X_inv <-  solve(t(X) %*% X)
beta_hat <- solve(t(X) %*% X) %*% (t(X)%*%Y); beta_hat

#Ya con los Betas calculados, procedemos a calcular los R#

Y_hat <- X %*% beta_hat
e_hat <- Y - Y_hat

SRC <- t(e_hat) %*% e_hat

Y_centrada_cuadrada <- (Y - mean(Y))^2
VT <- sum(Y_centrada_cuadrada)
R2 <- 1 - (SRC / VT)
N <- length(Y)
K <- length(beta_hat)
R2_ajustado <- 1 - ((1-R2)*((N-1)/(N-K)))

R2
R2_ajustado

#Tests T y F #

sigma2_hat <- SRC / (length(Y) - length(beta_hat))
sigma2_hat <- as.vector(sigma2_hat)
var_covar <- sigma2_hat * (solve(t(X) %*% X))

D <- diag(var_covar)
t <- beta_hat / sqrt(D)
# T critico#
T_Critico <- abs(qt(p = 0.025,
                    df= N - 1)); T_Critico
#T empirico#
T_Emp <- as.vector(2*pt(t,df=N-K)); T_Emp

ifelse(T_Critico>T_Emp, "No Rechazo", "Rechazo")

#Aca lo que hice fue en vez de hacer un codigo para cada Beta los hice todos juntos entiendo que es mas eficiente#

#Test F#

F_Calc <- ((R2)/(K-1))/((1-R2)/(N-K)); F_Calc #F empirico#
F_critico <- qf(0.05,df1=K-1,df2=N-K, ncp = F, lower.tail = F); F_critico

#Hacemos el Test#

Resultado <- if(F_critico>F_Calc) {
  "No Rechazo"
} else {
  "Rechazo"
};
Resultado

# Test de normalidad Jarque-Bera#

Vector_e_hat <- as.vector(e_hat)

library(moments)
jarque.test(Vector_e_hat) #El test indica que no har normalidad en las perturbaciones el P valor esta por debajo de 0.05 Rechaza H0#

#Resumen para contrastar los datos encontrados#

formula <- 'log(Y) ~ log(X2) + log(X3) + X4 + X5'
resultados <- lm(formula = formula,data=data)
summary(resultados)
