# Instrumental Variables

# Muchas veces en el mundo social tenemos relaciones que son endogenas. 
# Es decir, forman parte de un mecanismo circular que esta generando los datos.
# Usando nuestra formula, en este caso X causaria Y, pero al mismo tiempo,
# Y causaria X. Una pregunta importante en este tipo de relaciones (endogenas) es
# conocer (estimar) cual variable va primero en nuestra relacion causal, X o Y?
# El ejemplo mas clasico es el tema de los huevos y las gallinas: cuando hay mas gallinas,
# hay mas huevos, y cuando hay mas huevos, hay mas gallinas. Que variable (huevo o gallina) va 
# primero en la relacion causal?

# Que otras relaciones endogenas existen?

# Afortunadamente, existen herramientas econometricas que nos podrian ayudar este problema de 
# "reverse causality". Es un problema. Si estimamos Y ~ X, dado que Y --> X, el residuo de Y ~ X esta 
# correlacionado con Y. 

# Ese es el tema de esta clase: variables instrumentales que se estiman a traves de 2SLS.

# Hoja

# Estimemos Variables Instrumentales

## Pensemos en la relacion que hay entre CANTIDAD y PRECIO: es endogena? Quien va primero? Que instrumento podemos encontrar?

library(AER)

options(scipen=100000000)
data(CigarettesSW) # carguemos la base
head(CigarettesSW) # mostremos un poco la base


# price
# cpi= consumer price index (inflacion)

# Preparemos los datos
CigarettesSW$real.price = CigarettesSW$price / CigarettesSW$cpi # calculemos el precio real
CigarettesSW$log.precio = log(CigarettesSW$real.price) # log # PRECIO (X)
CigarettesSW$log.impuesto = log(CigarettesSW$tax) # log IMPUESTOS (Z)
CigarettesSW$log.cantidad = log(CigarettesSW$packs) # log CANTIDAD (Y)


# Plotiemos los datos
pairs(~log.precio+log.cantidad+log.impuesto,data=CigarettesSW, main="Scatterplot Matrix")

## Supuestos de Instrumental Variables
# 1. Z esta correlacionado con X.
# 2. Z no esta correlacionado con Y.
# 3. El coeficiente entre Z y X es significativo.

# Veamos
## Paso 0. Veamos primero si tenemos un problema: Estan los residuos del modelo Precio ~ Cantidad relacionados con Precio?
residuals.ols = as.numeric(lm(log.cantidad ~ log.precio , CigarettesSW)$residuals)

####################################################
# Wait... CUAL ES EL SUPUESTO DEL SUPUESTO AQUI?
####################################################

cor(residuals.ols, CigarettesSW$log.cantidad) # Estan correlacionados? Por que?

# OK; TENEMOS UN PROBLEMA de endogeneidad: debemos estimar un IV

# Veamos si cumplimos con los supuestos

# Supuesto 1
cor(CigarettesSW$log.impuesto,CigarettesSW$log.precio)

# Supuesto 2
cor(CigarettesSW$log.impuesto,CigarettesSW$log.cantidad) # ???

# Supuesto 3
options(scipen=9999999)
summary(lm(log.impuesto ~ log.precio, CigarettesSW))

## OK, mas o menos cumplimos con todos los supuestos

####################################################
# 2SLS via lm
####################################################

## Primera Etapa
first.s = lm(log.impuesto~log.precio,data=CigarettesSW)
z.hat = predict(first.s) # Predicciones (como se sacarian a mano?)
CigarettesSW$z.hat <- z.hat


## Segunda etapa
second.s = lm(log.cantidad~z.hat,data=CigarettesSW)
summary(second.s)

# OK, el coeficiente asociado a z.hat debiera no estar sesgado.

####################################################
# 2SLS via "ivreg"
####################################################

iv.reg = ivreg(log.cantidad~log.impuesto|log.precio,data=CigarettesSW)
summary(iv.reg)

# Comparemos todo en una tabla

library(texreg)
screenreg(list(second.s,iv.reg)) # Que vemos (punto de los logs).