### R code from vignette source 'RDD_Fuzzy.Rnw'

###################################################
### code chunk number 1: setup
###################################################
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(knitr)
set.seed(2020)
options(scipen=9999999)
if (!require("pacman")) install.packages("pacman"); library(pacman) 


###################################################
### code chunk number 2: chunk:1
###################################################
mu <- c(0, 0) # los promedios de ambos vectores
sigma <- matrix(c(1, 0.7, 0.7, 1), ncol = 2) # una matriz de varianza-covarianza
p_load(MASS)
d <- as.data.frame(mvrnorm(2000, mu, sigma)) # creamos la base de datos
colnames(d) <- c("x", "y") # le cambiamos los nombres
corte = 0 # definir el corte
d$z <- ifelse(d$x <  corte, 0, 0.8) # introducimos FUZZYNESS 
# Si d$x < corte, z es 0 con un 100% de probabilidad.
# Si d$x > corte, z es 1 con un 80% de probabilidad. 
# "Compliance"!


###################################################
### code chunk number 3: chunk:2
###################################################
table(d$z)


###################################################
### code chunk number 4: chunk:3
###################################################
d$z <- sapply(X = d$z, FUN = function(x) rbinom(1, 1, prob = x)) # funcion


###################################################
### code chunk number 5: chunk:4
###################################################
table(d$z)


###################################################
### code chunk number 6: chunk:5
###################################################
d$y <- d$y + d$z * 2


###################################################
### code chunk number 7: chunk:6
###################################################
plot(d$x, d$y,
     col = c("steelblue", "darkred")[factor(d$z)],  # asignar color para z(1) y z(0)
     pch= 20, # geometria de los circulos
     cex = 0.5, # tamano de los circulos
     xlab = "x",
     ylab = "y")
# linea vertical 
abline(v = 0, lty = 2)


###################################################
### code chunk number 8: chunk:7
###################################################
# Estimar Fuzzy RDD
p_load(rddtools)
# declarar que objeto "d" es un data frame de RDD
data <- rdd_data(d$y, d$x, 
                 cutpoint = corte, 
                 z = d$z 
                 )
# veamos
head(data)
# Estimar modelo fuzzy RDD
rdd_reg_lm(rdd_object = data, slope = "same")
# En este caso, D es el efecto causal, o rho. Es aprox. 2. 


###################################################
### code chunk number 9: chunk:8
###################################################
p_load(rdd)
band <- IKbandwidth(d$x, d$y)
band # ancho de banda optimo
# corta la base segun el ancho de banda optimo.
# nos quedamos solo con los valores determinados por ``band'' por lado
d_rdd  <- d[d$x < band & d$x > -band, ] 
# veamos la max y min de x de la nueva base datos cortada
summary(d_rdd) # base cortada
# estimamos el modelo (la mini regres.) con la base cortada d_rdd
linear_rdd_model <- lm(y ~ x + z, data=d_rdd)
summary(linear_rdd_model) # z es el efecto cuasi causal local del ``tratamiento''


###################################################
### code chunk number 10: output
###################################################
knitr::purl('RDD_Fuzzy.Rnw')
Stangle('RDD_Fuzzy.Rnw')


